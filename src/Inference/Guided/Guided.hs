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
import           Trace
import Debug.Trace
import qualified Inference.MC.SIM as SIM
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util

data GradUpd a where
  UpdateParam :: [(GradTrace, LogP)] -> GuideTrace -> GradUpd GuideTrace

type GuidedModel es a = Model (Guide : es) a

type GuidedExec es a = GuideTrace -> GuidedModel es a -> Sampler ((a, GradTrace), LogP)

guidedLoop :: (Members [GradUpd, Sampler] fs)
  => Int                                     -- ^ number of optimisation steps (T)
  -> Int                                     -- ^ number of samples to estimate the gradient over (L)
  -> GuidedExec es a -> GuidedModel es a
  -> GuideTrace                             -- ^ guide parameters λ_t, model parameters θ_t
  -> Comp fs GuideTrace      -- ^ final guide parameters λ_T
guidedLoop n_timesteps n_samples exec model params_0 = do
  let guidedStep φ = do
        rs <- replicateM n_samples (call $ exec φ model)
        let wgrads = Prelude.map (\((_, grad), w) -> (grad, w)) rs
        call (UpdateParam wgrads φ)
  foldr1 (>=>) (replicate n_timesteps guidedStep) params_0

-- | Collect the parameters λ_0 of the guide's initial proposal distributions.
collectGuide :: GuidedModel '[Sampler] a -> Sampler GuideTrace
collectGuide = handleIO . defaultGuide . loop Trace.empty .  SIM.defaultSample . SIM.defaultObserve
  where
  loop :: GuideTrace -> Comp (Guide : es) a -> Comp (Guide : es) GuideTrace
  loop params (Val _)   = pure params
  loop params (Op op k) = case prj op of
    Just (Guide d (q :: q) α) -> do let kα = (Key α :: Key q)
                                        params' = Trace.insert kα (Identity q) params
                                    Op op (loop params' . k)
    Nothing -> Op op (loop params . k)

{- | Set the proposal distributions Q(λ) of @Score@ operations.
-}
reuseGuide :: forall es a. Member Guide es => GuideTrace -> Comp es a -> Comp es (a, GradTrace)
reuseGuide proposals = loop Trace.empty where
  loop :: GradTrace -> Comp es a -> Comp es (a, GradTrace)
  loop grads (Val a)   = pure (a, grads)
  loop grads (Op op k) = case prj op of
    Just (Guide d (q :: q) α) -> do let kα = (Key α :: Key q)
                                        Identity q' = fromMaybe (Identity q) (Trace.lookup kα proposals)
                                    x <- call (Guide d q' α)
                                    let gs = Trace.insert kα (VecFor (gradLogProb q' x)) grads
                                    loop gs (k x)
    Nothing -> Op op (loop grads . k)

{- | Reuse the proposal distributions Q(λ) of @Score@ operations.
reuseGuide :: forall es a. Member Guide es => GuideTrace -> Comp es a -> Comp es (a, GuideTrace, GradTrace)
reuseGuide dists = loop (dists, Trace.empty) where
  loop :: (GuideTrace, GradTrace) -> Comp es a -> Comp es (a, GuideTrace, GradTrace)
  loop (dists, grads) (Val a)   = pure (a, dists, grads)
  loop (dists, grads) (Op op k) = case prj op of
    Just (Guide d (q :: q) α) -> do let (q', dists') = Trace.lookupOrInsert (Key α) q dists
                                    x <- call (Guide d q' α)
                                    let grads' = Trace.insert @q (Key α) (gradLogProb q' x) grads
                                    loop (dists', grads') (k x)
    Nothing -> Op op (loop grads . k)
-}

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
  -> GuideTrace  -- ^ optimisable distributions Q(λ_t)
  -> GradTrace  -- ^ elbo gradient estimates   E[δelbo]
  -> GuideTrace  -- ^ updated distributions     Q(λ_{t+1})
gradStep η = undefined -- Trace.intersectLeftWith (\q δλ ->  q `safeAddGrad` (η *| δλ))
