
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

{- | Maximum likelihood estimation in terms of VI that samples from the prior P(X) and assigns each simulation an
     importance weight P(Y | X; θ).
-}

module Inference.VI.MAP (module Inference.VI.MAP) where

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
import Sampler
import           Trace (GradTrace, ParamTrace, Key(..), Some(..))
import qualified Trace
import Debug.Trace
import Inference.MC.SIM
import Inference.VI.VI as VI
import Inference.MC.LW (joint)

map :: forall env a b.
     Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> VIGuide env a                      -- ^ guide Q(X; λ)
  -> VIModel env b                      -- ^ model P(X, Y)
  -> Env env                            -- ^ model environment (containing only observed data Y)
  -> Sampler ParamTrace                     -- ^ final parameters θ_T
map num_timesteps num_samples guide model env  = do
  -- | Set up a empty dummy guide Q to return the original input model environment
  λ_0 <- collectParams env guide
  -- | Run MLE for T optimisation steps
  (handleM . VI.handleNormGradDescent) $
      VI.viLoop num_timesteps num_samples guide (handleGuide env) model handleModel λ_0

handleGuide :: Env env -> VIGuide env a -> ParamTrace -> Sampler (((a, Env env), LogP), GradTrace)
handleGuide env guide params =
  (handleM . defaultSample . handleParams . weighGuide . updateParams params . handleEnvRW env) guide

-- | Handle the model P(X, Y; θ) by returning log-importance-weight P(Y, X; θ)
handleModel :: VIModel env a -> Env env -> Sampler (a, LogP)
handleModel model env =
  (handleM . defaultSample . defaultObserve . joint 0 . fmap fst . handleEnvRW env) model
