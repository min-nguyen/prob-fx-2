



{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

{- | Particle Independence Metropolis.
-}

module Inference.MC.PIM where

import Comp
import Sampler
import LogP
import Trace (Trace, filterTrace)
import Effects.MulDist
import Dist
import Model
import Env
import Effects.EnvRW
import qualified Data.Map as Map
import Inference.MC.SIM as SIM
import qualified Inference.MC.SSMH as SSMH
import Inference.MC.MH as MH
import           Inference.MC.SIS as SIS
import           Inference.MC.SMC (handleResampleMul, advance)
import qualified Inference.MC.SMC as SMC
import qualified Inference.MC.IM as IM

{- | Top-level wrapper for PIM inference.
-}
pimWith :: forall env vars a. (env `ContainsVars` vars)
  => Int                                            -- ^ number of SSMH steps
  -> Int                                            -- ^ number of particles
  -> MulModel env [EnvRW env, MulDist, Sampler] a                  -- ^ model
  -> Env env                                        -- ^ input environment
  -> Vars vars                                      -- ^ parameter names
  -> Sampler [Env env]                              -- ^ output environments
pimWith mh_steps n_prts gen_model env_in obs_vars = do
  -- | Handle model to probabilistic program
  let model   = conditionWith env_in gen_model
  -- | Convert observable variables to strings
  let tags = varsToStrs @env obs_vars
  -- | Initialise sample trace to include only parameters
  τθ_0       <- (fmap (filterTrace tags . snd) . runImpure .  reuseTrace Map.empty . defaultObserve) model
  pmmh_trace <- (runImpure . IM.handleProposal . mh mh_steps τθ_0 (exec n_prts)) model
  pure (map (snd . fst . fst) pmmh_trace)

pim :: Int -> Int -> Trace -> Model '[Sampler] a -> Sampler [((a, LogP), Trace)]
pim mh_steps n_prts τθ = runImpure . IM.handleProposal . mh mh_steps τθ (exec n_prts)

{- | Handle probabilistic program using SSMH and compute the average log-probability using SMC.
-}
exec :: Int -> ModelExec '[Sampler] LogP a
exec n τθ prog   = do
  let exec_prt :: ModelStep '[Sampler] LogP a
      exec_prt (p, w) = (fmap fst .  runImpure .  reuseTrace τθ . advance w) p
  (as, ρs) <- (fmap unzip . runImpure . handleResampleMul . pfilter n 0 exec_prt ) prog
  return ((head as, logMeanExp ρs), τθ)

