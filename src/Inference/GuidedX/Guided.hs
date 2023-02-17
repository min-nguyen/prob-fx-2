{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}

{- | BBVI inference on a model and guide as separate programs.
-}

module Inference.GuidedX.Guided where

import Data.Maybe
import Data.Proxy
import Data.Bifunctor ( Bifunctor(..) )
import Control.Monad ( replicateM, (>=>), mapAndUnzipM )
import Effects.Dist
import Model
import Effects.EnvRW ( EnvRW, handleEnvRW )
import Effects.State ( modify, handleState, State )
import Effects.Guide
import Env ( Env, union )
import LogP ( LogP(..), normaliseLogPs )
import PrimDist
import Comp ( discharge, Comp(..), call, weaken, LastMember, Member (..), Members, weakenProg, Handler, handleWith, handle )
import Sampler
import           Trace (GradTrace, ParamTrace, Key(..), Some(..), ValueTrace)
import qualified Trace
import Debug.Trace
import qualified Inference.MC.SIM as SIM
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util

data GradEst a where
  UpdateParam :: [(LogP, GradTrace)] -> ParamTrace -> GradEst ParamTrace

type GuidedModel es a = Comp (Guide : Observe : Sample : es) a

type GuidedExec es a = ParamTrace -> GuidedModel es a -> Sampler ((a, GradTrace), LogP)

guidedLoop :: (Members [GradEst, Sampler] fs)
  => Int                                     -- ^ number of optimisation steps (T)
  -> Int                                     -- ^ number of samples to estimate the gradient over (L)
  -> GuidedExec es a -> GuidedModel es a
  -> ParamTrace                             -- ^ guide parameters λ_t, model parameters θ_t
  -> Comp fs ParamTrace      -- ^ final guide parameters λ_T
guidedLoop n_timesteps n_samples exec model params_0 = do
  let guidedStep φ = do
        rs <- replicateM n_samples (call $ exec φ model)
        let wgrads = map (\((_, grad), w) -> (w, grad)) rs
        call (UpdateParam wgrads φ)
  foldr1 (>=>) (replicate n_timesteps guidedStep) params_0

-- | Collect the parameters λ_0 of the guide's initial proposal distributions.
collectGuide :: GuidedModel '[Sampler] a -> Sampler ParamTrace
collectGuide = handleIO . SIM.defaultSample . SIM.defaultObserve . defaultGuide . loop Trace.empty
  where
  loop :: ParamTrace -> Comp (Guide : es) a -> Comp (Guide : es) ParamTrace
  loop params (Val _)   = pure params
  loop params (Op op k) = case prj op of
    Just (Guide d q α)   -> do let params' = Trace.insert (Key α) q params
                               Op op (loop params' . k)
    Nothing -> Op op (loop params . k)

{- | Set the proposal distributions Q(λ) of @Score@ operations.
-}
updateGuide :: forall es a. Member Guide es => ParamTrace -> Comp es a -> Comp es a
updateGuide proposals = loop where
  loop :: Comp es a -> Comp es a
  loop (Val a)   = pure a
  loop (Op op k) = case prj op of
    Just (Guide d q α) -> call (Guide d q' α) >>= (loop . k) where q' = Trace.lookupWithDefault q (Key α) proposals
    Nothing -> Op op (loop . k)

-- | Sample from each @Guide@ distribution
defaultGuide :: forall es a. Member Sampler es => Handler Guide es a a
defaultGuide  = handle Val hop where
  hop :: Guide x -> (() -> x -> Comp es b) -> Comp es b
  hop (Guide (d :: d) (q :: q) α) k = do
      x <- call (drawWithSampler q)
      k () x

prior :: forall es a. Member Sampler es => Handler Sample es a (a, LogP)
prior = handleWith 0 (\lρ x -> Val (x, lρ)) hop
  where
  hop :: LogP -> Sample x -> (LogP -> x -> Comp es b) -> Comp es b
  hop lρ (Sample d α) k = do x <- call $ drawWithSampler d
                             k (lρ + logProb d x) x

gradStep
  :: Double  -- ^ learning rate             η
  -> ParamTrace  -- ^ optimisable distributions Q(λ_t)
  -> GradTrace  -- ^ elbo gradient estimates   E[δelbo]
  -> ParamTrace  -- ^ updated distributions     Q(λ_{t+1})
gradStep η = Trace.intersectLeftWith (\q δλ ->  q `safeAddGrad` (η *| δλ))
