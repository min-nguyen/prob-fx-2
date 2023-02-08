



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
import Effects.Dist
import PrimDist
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
pim :: forall env vars a. (env `ContainsVars` vars)
  => Int                                            -- ^ number of SSMH steps
  -> Int                                            -- ^ number of particles
  -> GenModel env [EnvRW env, Dist, Sampler] a                  -- ^ model
  -> Env env                                        -- ^ input environment
  -> Vars vars                                      -- ^ parameter names
  -> Sampler [Env env]                              -- ^ output environments
pim mh_steps n_prts model env_in obs_vars = do
  -- | Handle model to probabilistic program
  let prog_0   = handleCore env_in model
  -- | Convert observable variables to strings
  let tags = varsToStrs @env obs_vars
  -- | Initialise sample trace to include only parameters
  τθ_0       <- (fmap (filterTrace tags . snd) . handleIO .  reuseTrace Map.empty . defaultObserve) prog_0
  pmmh_trace <- (handleIO . IM.handleProposal . mh mh_steps τθ_0 (exec n_prts)) prog_0
  pure (map (snd . fst . fst) pmmh_trace)

{- | Handle probabilistic program using SSMH and compute the average log-probability using SMC.
-}
exec :: Int -> ModelHandler '[Sampler] LogP
exec n τθ prog   = do
  let exec_prt :: ParticleHandler '[Sampler] LogP
      exec_prt logp = fmap fst .  handleIO .  reuseTrace τθ . advance logp
  (as, ρs) <- (fmap unzip . handleIO . handleResampleMul . pfilter n exec_prt 0) prog
  return ((head as, logMeanExp ρs), τθ)

