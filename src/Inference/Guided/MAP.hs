{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

{- | BBVI inference on a model and guide as separate programs.
-}

module Inference.Guided.MAP
  where

import Inference.Guided.Guided
import qualified Inference.Guided.MLE as MLE
import qualified Inference.Guided.BBVI as BBVI
import Effects.Guide
import Data.Maybe
import LogP
import Sampler
import           Trace
import Inference.MC.LW (likelihood)
import PrimDist
import qualified Vec
import           Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Data.Data (Proxy(..))
import Comp
import Inference.MC.SIM
import Effects.Dist
import qualified Inference.MC.LW as LW

{- | Top-level wrapper for BBVI inference that takes a separate model and guide.
-}
map :: forall es a. ()
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> GuidedModel '[Sampler] a      -- ^ guide Q(X; λ)
  -> Sampler GuideTrace                 -- ^ final guide parameters λ_T
map num_timesteps num_samples model = do
  λ_0 <- collectGuide model
  -- liftIO (print λ_0)
  (handleIO . MLE.handleNormGradDescent)
    $ guidedLoop num_timesteps num_samples exec model λ_0

-- | Sample from each @Guide@ distribution, x ~ Q(X; λ), and record its grad-log-pdf, δlog(Q(X = x; λ)).
handleGuide :: forall es a. Members '[Sampler] es => Handler Guide es a (a, LogP)
handleGuide  = handleWith 0 (\s a -> Val (a, s)) hop where
  hop :: LogP -> Guide x -> (LogP -> x -> Comp es b) -> Comp es b
  hop w (Guide (d :: d) (q :: q) α) k = do
        r <- random
        let x = draw q r
        k (w + logProb d x) x


-- | Compute Q(X; λ)
exec :: GuideTrace -> GuidedModel '[Sampler] a -> Sampler ((a, GradTrace), LogP)
exec params = handleIO . mergeWeights . handleGuide . defaultSample . defaultObserve . LW.joint 0 . reuseGuide params where
  mergeWeights = fmap (\((x, w_lat), w_obs) -> (x, w_lat + w_obs))
