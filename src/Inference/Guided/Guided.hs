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
import Effects.MulDist
import Model
import Effects.EnvRW ( EnvRW, handleEnvRW )
import Effects.State ( modify, handleState, State )
import Effects.GuidedSample
import Env ( Env, union )
import LogP ( LogP(..) )
import Dist
import Comp ( Comp(..), runImpure, call, Member (..), Members, Handler, handleWith, handle )
import Sampler
import           Trace
import Debug.Trace
import qualified Inference.MC.SIM as SIM
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util

type GuidedModel es a = Model (GuidedSample : es) a

type GuidedExec es a  = Guides -> GuidedModel es a -> Sampler ((a, ΔGuides), LogP)

data GradUpdate a where
  GradUpdate :: [(ΔGuides, LogP)] -> Guides -> GradUpdate Guides

guidedLoop :: (Members [GradUpdate, Sampler] fs)
  => Int                                     -- ^ number of optimisation steps (T)
  -> Int                                     -- ^ number of samples to estimate the gradient over (L)
  -> GuidedExec es a -> GuidedModel es a
  -> Guides                             -- ^ guide parameters λ_t, model parameters θ_t
  -> Comp fs Guides      -- ^ final guide parameters λ_T
guidedLoop n_timesteps n_samples exec model params_0 = do
  let guidedStep φ = do
        rs <- replicateM n_samples (call $ exec φ model)
        let wgrads = Prelude.map (\((_, grad), w) -> (grad, w)) rs
        call (GradUpdate wgrads φ)
  foldr1 (>=>) (replicate n_timesteps guidedStep) params_0

-- | Collect the parameters λ_0 of the guide's initial proposal distributions.
collectGuides :: GuidedModel '[Sampler] a -> Sampler Guides
collectGuides = runImpure . defaultGuide . loop Trace.empty .  SIM.defaultSample . SIM.defaultObserve where
  loop :: Guides -> Comp (GuidedSample : es) a -> Comp (GuidedSample : es) Guides
  loop params (Val _)   = pure params
  loop params (Op op k) = case prj op of
    Just (GuidedSample d (q :: q) α) -> do let kα = (Key α :: Key q)
                                               params' = Trace.insert kα (Identity q) params
                                           Op op (loop params' . k)
    Nothing -> Op op (loop params . k)

{- | Set the proposal distributions Q(λ) of @Score@ operations.
-}
useGuides :: forall es a. Member GuidedSample es => Guides -> Comp es a -> Comp es (a, ΔGuides)
useGuides proposals = loop Trace.empty where
  loop :: ΔGuides -> Comp es a -> Comp es (a, ΔGuides)
  loop grads (Val a)   = pure (a, grads)
  loop grads (Op op k) = case prj op of
    Just (GuidedSample d (q :: q) α) -> do let kα = (Key α :: Key q)
                                               Identity q' = fromMaybe (Identity q) (Trace.lookup kα proposals)
                                           x <- call (GuidedSample d q' α)
                                           let gs = Trace.insert kα (VecFor (gradLogProb q' x)) grads
                                           loop gs (k x)
    Nothing -> Op op (loop grads . k)

-- | Sample from each @GuidedSample@ distribution
defaultGuide :: forall es a. Member Sampler es => Handler GuidedSample es a a
defaultGuide  = handle Val hop where
  hop :: GuidedSample x -> (x -> Comp es b) -> Comp es b
  hop (GuidedSample (d :: d) (q :: q) α) k = do
      x <- call (drawWithSampler q)
      k x

prior :: forall es a. Member Sampler es => Handler Sample es a (a, LogP)
prior = handleWith 0 (\lρ x -> Val (x, lρ)) hop
  where
  hop :: LogP -> Sample x -> (LogP -> x -> Comp es b) -> Comp es b
  hop lρ (Sample d α) k = do x <- call $ drawWithSampler d
                             k (lρ + logProb d x) x

gradStep
  :: Double  -- ^ learning rate             η
  -> Guides  -- ^ optimisable distributions Q(λ_t)
  -> ΔGuides -- ^ elbo gradient estimates   E[δelbo]
  -> Guides  -- ^ updated distributions     Q(λ_{t+1})
gradStep η guides grads =
  let scaledGrads = Trace.map (\(VecFor δλ) -> VecFor (η *| δλ)) grads
  in  Trace.intersectWithAdd guides scaledGrads