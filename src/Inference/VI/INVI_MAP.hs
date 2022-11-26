{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

{- | Maximum likelihood estimation in terms of VI that samples from the prior P(X) and assigns each simulation an
     importance weight P(Y | X; θ).
-}

module Inference.VI.INVI_MAP where

import Data.Maybe ( fromMaybe )
import Data.Bifunctor ( Bifunctor(..) )
import Control.Monad ( replicateM, (>=>) )
import Effects.Dist
import Effects.Lift
import Effects.ObsRW ( ObsRW )
import Effects.State ( modify, handleState, State )
import Env ( Env, Vars, ContainsVars, union, empty, varsToStrs )
import LogP ( LogP(..), normaliseLogPs, expLogP )
import Model
import PrimDist
import Prog ( discharge, Prog(..), call, weaken, LastMember, Member (..), Members, weakenProg )
import           Sampler ( Sampler )
import           Trace (GTrace, DTrace, Key(..), Some(..))
import qualified Trace
import qualified Inference.MC.SIM as SIM
import qualified Inference.VI.BBVI as BBVI
import qualified Inference.VI.VI as VI
import qualified Inference.VI.MAP as MAP
import qualified Inference.VI.MLE as MLE

{- | Top-level wrapper for MAP + INVI inference
-}
invimap :: forall env xs a b. (Show (Env env), (env `ContainsVars` xs))
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> Model env [ObsRW env, Dist] b      -- ^ guide Q(X; λ)
  -> Model env [ObsRW env, Dist] a      -- ^ model P(X, Y; θ)
  -> Env env                            -- ^ model environment (containing only observed data Y)
  -> Vars xs                            -- ^ parameter names θ
  -> Sampler (DTrace, DTrace)           -- ^ final guide + model parameters (λ_T; θ_T)
invimap num_timesteps num_samples guide_model model model_env vars  = do
  -- | Prepare guide
  let guide :: Prog '[Param, Sample] (b, Env env)
      guide = ((second (Env.union model_env) <$>) . VI.installGuideParams . handleCore model_env) guide_model
  -- | Collect initial proposal distributions
  guideParams_0 <- VI.collectGuideParams guide

  -- | Collect initial model parameters θ
  let tags = Env.varsToStrs @env vars
  modelParams_0 <- MLE.collectModelParams tags (handleCore model_env model)
  -- | Run BBVI for T optimisation steps
  (handleLift . VI.handleNormGradDescent)
    $ VI.viLoop num_timesteps num_samples guide VI.handleGuide model (MAP.handleModel tags) (guideParams_0, modelParams_0)
