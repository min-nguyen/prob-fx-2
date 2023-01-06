{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE TupleSections #-}

{- | Maximum likelihood estimation in terms of VI that samples from the prior P(X) and assigns each simulation an
     importance weight P(Y | X; θ).
-}

module Inference.VI.MLE where

import Data.Maybe ( fromMaybe )
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
import           Sampler ( Sampler )
import           Trace (GTrace, DTrace, Key(..), Some(..))
import qualified Trace
import qualified Inference.MC.SIM as SIM
import qualified Inference.VI.BBVI as BBVI
import qualified Inference.VI.VI as VI
import Inference.MC.LW (likelihood)

mle :: forall env xs a b. (Show (Env env), (env `ContainsVars` xs))
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> Model env [ObsRW env, Dist] a      -- ^ model P(X, Y; θ)
  -> Env env                            -- ^ model environment (containing only observed data Y)
  -> Vars xs                            -- ^ parameter names θ
  -> Sampler DTrace                     -- ^ final parameters θ_T
mle num_timesteps num_samples model model_env vars = do
  -- | Set up a empty dummy guide Q to return the original input model environment
  let guide :: Prog '[Param, Sample] ((), Env env)
      guide = pure ((), model_env)
      guideParams_0 :: DTrace
      guideParams_0 = Trace.empty
  -- | Collect initial model parameters θ
  let tags = Env.varsToStrs @env vars
  -- | Run MLE for T optimisation steps
  (handleLift . VI.handleNormGradDescent) $
      VI.viLoop num_timesteps num_samples guide handleGuide model (handleModel tags) guideParams_0

-- | Handle the dummy guide Q by returning the original model environment Y and log-importance-weight log(Q(X)) = 0
handleGuide :: Prog [Param, Sample] a -> DTrace -> Sampler ((a, LogP), GTrace)
handleGuide guide _ =
  (SIM.defaultSample . VI.handleGuideParams ) ((, 0) <$> guide)

-- | Handle the model P(X, Y; θ) by returning log-importance-weight P(Y | X; θ)
handleModel :: [Tag] -> Model env [ObsRW env, Dist] a -> Env env -> Sampler ((a, Env env), LogP)
handleModel tags model env  =
  (SIM.defaultSample . likelihood 0 . handleCore env) model