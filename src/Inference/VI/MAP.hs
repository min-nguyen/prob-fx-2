
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

{-# LANGUAGE TypeApplications #-}

{- | Maximum likelihood estimation in terms of VI that samples from the prior P(X) and assigns each simulation an
     importance weight P(Y | X; θ).
-}

module Inference.VI.MAP (module MLE, module Inference.VI.MAP) where

import Data.Bifunctor ( Bifunctor(..) )
import Control.Monad ( replicateM, (>=>) )
import Effects.Dist
import Effects.Lift
import Effects.ObsRW ( ObsRW )
import Effects.State ( modify, handleState, State )
import Env ( Env, Vars, ContainsVars, union, empty, varsToStrs )
import LogP ( LogP(..), normaliseLogPs )
import Model
import PrimDist
import Prog ( discharge, Prog(..), call, weaken, LastMember, Member (..), Members, weakenProg )
import Sampler
import           Trace (GradTrace, ParamTrace, Key(..), Some(..))
import qualified Trace
import Debug.Trace
import qualified Inference.MC.SIM as SIM
import qualified Inference.VI.VI as VI
import qualified Inference.VI.MLE as MLE
import Inference.MC.LW (joint)

map :: forall env xs a b. (Show (Env env), (env `ContainsVars` xs))
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> Model env [ObsRW env, Dist] a      -- ^ model P(X, Y; θ)
  -> Env env                            -- ^ model environment (containing only observed data Y)
  -> Vars xs                            -- ^ parameter names θ
  -> Sampler ParamTrace                     -- ^ final parameters θ_T
map num_timesteps num_samples model model_env vars = do
  -- | Set up a empty dummy guide Q to return the original input model environment
  let guide :: Prog '[Param, Sample] ((), Env env)
      guide = pure ((), model_env)
      guideParams_0 :: ParamTrace
      guideParams_0 = Trace.empty
  -- | Collect initial model parameters θ
  let tags = Env.varsToStrs @env vars
  -- | Run MLE for T optimisation steps
  (handleLift . VI.handleNormGradDescent) $
      VI.viLoop num_timesteps num_samples guide MLE.handleGuide model handleModel guideParams_0

-- | Handle the model P(X, Y; θ) by returning log-importance-weight P(Y, X; θ)
handleModel :: Model env [ObsRW env, Dist] a -> Env env -> Sampler ((a, Env env), LogP)
handleModel model env =
  (SIM.defaultSample . SIM.defaultObserve . joint 0 . handleCore env) model
