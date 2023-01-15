
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
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
import Effects.EnvRW ( EnvRW, handleEnvRW )
import Effects.State ( modify, handleState, State )
import Env ( Env, Vars, ContainsVars, union, empty, varsToStrs )
import LogP ( LogP(..), normaliseLogPs )
import Model
import PrimDist
import Prog ( discharge, Prog(..), call, weaken, LastMember, Member (..), Members, weakenProg )
import           Sampler ( Sampler )
import           Trace (GradTrace, ParamTrace, Key(..), Some(..))
import qualified Trace
import qualified Inference.MC.SIM as SIM
import           Inference.VI.VI
import Inference.MC.LW (likelihood)

mle :: forall env es a b. es ~ '[Sampler]
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> VIGuide env es a                      -- ^ guide Q(X; λ)
  -> VIModel env es b                      -- ^ model P(X, Y)
  -> Env env                            -- ^ model environment (containing only observed data Y)
  -> Sampler ParamTrace                     -- ^ final parameters θ_T
mle num_timesteps num_samples guide model env = do
  -- | Set up a empty dummy guide Q to return the original input model environment
  λ_0 <- collectParams env guide
  -- | Run MLE for T optimisation steps
  (handleM . handleNormGradDescent) $
      viLoop num_timesteps num_samples guide (handleGuide env) model handleModel λ_0

handleGuide :: es ~ '[Sampler] => Env env -> VIGuide env es a -> ParamTrace -> Sampler (((a, Env env), LogP), GradTrace)
handleGuide env guide params =
  (handleM . SIM.defaultSample . handleParams . fmap (,0) . updateParams params . handleEnvRW env) guide

-- | Handle the model P(X, Y; θ) by returning log-importance-weight P(Y | X; θ)
handleModel :: es ~ '[Sampler] => VIModel env es a -> Env env -> Sampler (a, LogP)
handleModel model env  =
  (handleM . SIM.defaultSample . likelihood 0 . fmap fst . handleEnvRW env) model