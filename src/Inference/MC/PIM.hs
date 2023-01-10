



{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

{- | Particle Independence Metropolis.
-}

module Inference.MC.PIM where

import Prog
import Sampler
import LogP
import Trace (Trace, filterTrace)
import Effects.Dist
import PrimDist
import Model
import Env
import Effects.Lift
import Effects.EnvRW
import qualified Data.Map as Map
import Inference.MC.SIM as SIM
import qualified Inference.MC.MH as MH
import Inference.MC.Metropolis as Metropolis
import           Inference.MC.SIS as SIS
import           Inference.MC.SMC as SMC
import qualified Inference.MC.IM as IM

{- | Top-level wrapper for PMMH inference.
-}
pim :: forall env vars a. (env `ContainsVars` vars)
  => Int                                            -- ^ number of MH steps
  -> Int                                            -- ^ number of particles
  -> Model env [EnvRW env, Dist] a                  -- ^ model
  -> Env env                                        -- ^ input environment
  -> Vars vars                                      -- ^ parameter names
  -> Sampler [Env env]                              -- ^ output environments
pim mh_steps n_prts model env_in obs_vars = do
  -- | Handle model to probabilistic program
  let prog_0   = handleCore env_in model
  -- | Convert observable variables to strings
  let tags = varsToStrs @env obs_vars
  -- | Initialise sample trace to include only parameters
  τθ_0       <- (fmap (filterTrace tags . snd) . reuseSamples Map.empty . defaultObserve) prog_0
  pmmh_trace <- (handleLift . IM.handleAccept . metropolis mh_steps τθ_0 (handleModel n_prts)) prog_0
  pure (map (snd . fst . fst) pmmh_trace)

{- | Handle probabilistic program using MH and compute the average log-probability using SMC.
-}
handleModel ::
     Int                                          -- ^ number of particles
  -> ProbProg a                                   -- ^ probabilistic program
  -> Trace                                       -- ^ proposed initial log-prob + sample trace
  -> Sampler ((a, LogP), Trace)                  -- ^ proposed final log-prob + sample trace
handleModel n prog τθ  = do
  let handleParticle :: ProbProg a -> Sampler (ProbProg a, LogP)
      handleParticle = fmap fst . reuseSamples τθ . suspend
  (as, ρs) <- (fmap unzip . handleLift . handleResampleMul . pfilter handleParticle prog) ((unzip . replicate n) (prog, 0))
  return ((head as, logMeanExp ρs), τθ)

