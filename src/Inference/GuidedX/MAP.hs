{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

{- | BBVI inference on a model and guide as separate programs.
-}

module Inference.GuidedX.MAP
  where

import Inference.GuidedX.Guided
import qualified Inference.GuidedX.MLE as MLE
import Effects.Guide
import Data.Maybe
import LogP
import Sampler
import           Trace (GradTrace, ParamTrace, Key(..), Some(..), ValueTrace)
import qualified Trace
import Inference.MC.LW (likelihood)
import PrimDist
import qualified Vec
import           Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Data.Data (Proxy(..))
import Comp
import Inference.MC.SIM
import Effects.Dist

{- | Top-level wrapper for BBVI inference that takes a separate model and guide.
-}
map :: forall es a. ()
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> GuidedModel '[Sampler] a      -- ^ guide Q(X; λ)
  -> Sampler ParamTrace                 -- ^ final guide parameters λ_T
map num_timesteps num_samples model = do
  λ_0 <- collectGuide model
  -- liftIO (print λ_0)
  (handleIO . MLE.handleNormGradDescent)
    $ guidedLoop num_timesteps num_samples exec model λ_0

-- | Compute Q(X; λ)
exec :: ParamTrace -> GuidedModel '[Sampler] a -> Sampler ((a, GradTrace), LogP)
exec params = handleIO . defaultSample . joint . handleGuide . updateGuide params
  where joint = likelihood

prior :: forall es a. Member Sampler es => Handler Sample es a (a, LogP)
prior = handleWith 0 (\lρ x -> Val (x, lρ)) hop
  where
  hop :: LogP -> Sample x -> (LogP -> x -> Comp es b) -> Comp es b
  hop lρ (Sample d α) k = do x <- call $ drawWithSampler d
                             k (lρ + logProb d x) x

-- | Sample from each @Guide@ distribution, x ~ Q(X; λ), and record its grad-log-pdf, δlog(Q(X = x; λ)).
handleGuide :: forall es a. Members [Sample, Observe] es => Handler Guide es a (a, GradTrace)
handleGuide  = handleWith Trace.empty (\grads a -> Val (a, grads)) hop where
  hop :: GradTrace -> Guide x -> (GradTrace -> x -> Comp es b) -> Comp es b
  hop grads (Guide (d :: d) (q :: q) α) k = do
      x <- call (Sample q α)
      call (Observe d x α)
      k (Trace.insert @q (Key α) (gradLogProb q x) grads) x
