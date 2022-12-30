{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

{- | Particle Marginal Metropolis-Hastings inference.
-}

module Inference.MC.PMMH where

import Prog
import Sampler
import LogP
import Trace (Trace, filterTrace)
import Effects.Dist
import PrimDist
import Model
import Env
import Effects.Lift
import Effects.ObsRW
import qualified Data.Map as Map
import Inference.MC.SIM as SIM
import qualified Inference.MC.MH as MH
import Inference.MC.Metropolis as Metropolis
import           Inference.MC.SIS as SIS
import           Inference.MC.SMC as SMC

{- | Top-level wrapper for PMMH inference.
-}
pmmh :: forall env vars a. (env `ContainsVars` vars)
  => Int                                            -- ^ number of MH steps
  -> Int                                            -- ^ number of particles
  -> Model env [ObsRW env, Dist] a                  -- ^ model
  -> Env env                                        -- ^ input environment
  -> Vars vars                                      -- ^ parameter names
  -> Sampler [Env env]                              -- ^ output environments
pmmh mh_steps n_prts model env_in obs_vars = do
  -- | Handle model to probabilistic program
  let prog_0   = handleCore env_in model
  -- | Convert observable variables to strings
  let tags = varsToStrs @env obs_vars
  -- | Initialise sample trace to include only parameters
  τθ_0       <- (fmap (filterTrace tags . snd) . reuseSamples Map.empty . defaultObserve) prog_0
  pmmh_trace <- (handleLift . handleAccept . metropolis mh_steps (0, τθ_0) (handleModel n_prts)) prog_0
  pure (map (snd . fst . fst) pmmh_trace)

{- | Handle probabilistic program using MH and compute the average log-probability using SMC.
-}
handleModel ::
     Int                                          -- ^ number of particles
  -> ProbProg a                                   -- ^ probabilistic program
  -> (LogP, Trace)                               -- ^ proposed initial log-prob + sample trace
  -> Sampler ((a, LogP), Trace)                  -- ^ proposed final log-prob + sample trace
handleModel n prog (_, τθ)  = do
  let handleParticle :: ProbProg a -> Sampler (ProbProg a, LogP)
      handleParticle = fmap fst . reuseSamples τθ . suspend
  (as, ρs) <- (handleLift . handleResampleMul . fmap unzip . pfilter handleParticle prog) ((unzip . replicate n) (prog, 0))
  let a   = head as
      ρ   = logMeanExp ρs
  pure ((a, ρ), τθ)

{- | An acceptance mechanism for PMMH.
-}
handleAccept :: HasSampler fs
  => Prog (Accept LogP : fs) a -> Prog fs a
handleAccept (Val x)   = pure x
handleAccept (Op op k) = case discharge op of
  Right (Propose τθ)
    ->  do α <- randomFrom' (Map.keys τθ)
           r <- random'
           let τθ' = Map.insert α r τθ
           (handleAccept . k) (0, τθ')
  Right (Accept lρ lρ')
    ->  do u <- random'
           (handleAccept . k) (exp (lρ' - lρ) > u)
  Left op'
    -> Op op' (handleAccept . k)