{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}

{- | BBVI inference on a model and guide as separate programs.
-}

module Inference.Guided.Guided where

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
  UpdateParam :: [LogP] -> [GradTrace] -> ParamTrace -> GradEst ParamTrace

type GuidedModel es a = Model (Guide : es) a

type GuidedExec es a = ParamTrace -> GuidedModel es a -> Sampler ((a, GradTrace), LogP)

guidedLoop :: (Members [GradEst, Sampler] fs)
  => Int                                     -- ^ number of optimisation steps (T)
  -> Int                                     -- ^ number of samples to estimate the gradient over (L)
  -> GuidedExec es a -> GuidedModel es a
  -> ParamTrace                             -- ^ guide parameters λ_t, model parameters θ_t
  -> Comp fs ParamTrace      -- ^ final guide parameters λ_T
guidedLoop n_timesteps n_samples exec model params = do
  foldr (>=>) pure [guidedStep n_samples exec model  | t <- [1 .. n_timesteps]] params

guidedStep ::  (Members [GradEst, Sampler] fs)
  => Int
  -> GuidedExec es a -> GuidedModel es a
  -> ParamTrace                            -- ^ guide parameters λ_t
  -> Comp fs ParamTrace    -- ^ next guide parameters λ_{t+1}
guidedStep n_samples exec model params = do
  -- | Execute for L iterations
  ((a, δλs), ws) <- first unzip . unzip <$> replicateM n_samples (call $ exec params model)
  -- | Update the parameters λ of the proposal distributions Q
  call (UpdateParam ws δλs params)

-- | Collect the parameters λ_0 of the guide's initial proposal distributions.
collectGuide :: GuidedModel '[Sampler] a -> Sampler ParamTrace
collectGuide = handleIO . defaultGuide . loop Trace.empty . SIM.defaultSample . SIM.defaultObserve
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
    Just (Guide d q α) -> do
      let q' = fromMaybe q (Trace.lookup (Key α) proposals)
      x <- call (Guide d q' α)
      (loop . k) x
    Nothing -> Op op (loop . k)

-- | Sample from each @Guide@ distribution
defaultGuide :: forall es a. Member Sampler es => Handler Guide es a a
defaultGuide  = handle Val hop where
  hop :: Guide x -> (() -> x -> Comp es b) -> Comp es b
  hop (Guide (d :: d) (q :: q) α) k = do
      x <- call (drawWithSampler q)
      k () x

-- | Sample from each @Guide@ distribution, x ~ Q(X; λ), and record its grad-log-pdf, δlog(Q(X = x; λ)).
handleGuide :: forall es a. Member Sampler es => Handler Guide es a ((a, GradTrace), LogP, LogP)
handleGuide  = handleWith (0, 0, Trace.empty) (\(w_d, w_q, grads) a -> Val ((a, grads), w_d, w_q)) hop where
  hop :: (LogP, LogP, GradTrace) -> Guide x -> ((LogP, LogP, GradTrace) -> x -> Comp es b) -> Comp es b
  hop (w_d, w_q, grads) (Guide (d :: d) (q :: q) α) k = do
      x <- call (drawWithSampler q)
      let d_prior = logProb d x
          q_prior = logProb q x
      k (w_d + d_prior, w_q + q_prior, Trace.insert @q (Key α) (gradLogProb q x) grads) x

gradStep
  :: Double  -- ^ learning rate             η
  -> ParamTrace  -- ^ optimisable distributions Q(λ_t)
  -> GradTrace  -- ^ elbo gradient estimates   E[δelbo]
  -> ParamTrace  -- ^ updated distributions     Q(λ_{t+1})
gradStep η = Trace.intersectLeftWith (\q δλ ->  q `safeAddGrad` (η *| δλ))
