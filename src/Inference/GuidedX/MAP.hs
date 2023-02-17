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
import qualified Inference.GuidedX.BBVI as BBVI
import Effects.Guide
import Data.Maybe
import LogP
import Sampler
import           Trace (GradTrace, DistTrace, Key(..), Some(..), ValueTrace)
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
  -> Sampler DistTrace                 -- ^ final guide parameters λ_T
map num_timesteps num_samples model = do
  λ_0 <- collectGuide model
  -- liftIO (print λ_0)
  (handleIO . MLE.handleNormGradDescent)
    $ guidedLoop num_timesteps num_samples exec model λ_0

-- | Compute Q(X; λ)
exec :: DistTrace -> GuidedModel '[Sampler] a -> Sampler ((a, GradTrace), LogP)
exec params = handleIO . defaultSample . joint . BBVI.handleGuide . updateGuide params
  where joint = likelihood