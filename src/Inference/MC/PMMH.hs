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
import Trace (STrace, filterTrace)
import Effects.Dist
import PrimDist
import Model
import Env
import Effects.Lift
import Effects.ObsRW
import qualified Data.Map as Map
import qualified Inference.MC.SIM as SIM
import qualified Inference.MC.MH as MH
import Inference.MC.Metropolis as Metropolis
import qualified Inference.MC.SIS as SIS
import qualified Inference.MC.SMC as SMC
import Inference.MC.SMC (Particle(particleLogProb, Particle))

{- | Top-level wrapper for PMMH inference.
-}
pmmh :: forall env vars a. (env `ContainsVars` vars)
  => Int                                            -- ^ number of MH steps
  -> Int                                            -- ^ number of particles
  -> Model env [ObsRW env, Dist] a                  -- ^ model
  -> Env env                                        -- ^ input environment
  -> Vars vars                                      -- ^ parameter names
  -> Sampler [Env env]                              -- ^ output environments
pmmh mh_steps n_particles model env_in obs_vars = do
  -- | Handle model to probabilistic program
  let prog_0   = handleCore env_in model
      strace_0 = Map.empty
  -- | Convert observable variables to strings
  let tags = varsToStrs @env obs_vars
  pmmh_trace <- handleLift (pmmhInternal mh_steps n_particles tags strace_0 prog_0)
  pure (map (snd . fst) pmmh_trace)

{- | PMMH inference on a probabilistic program.
-}
pmmhInternal :: (LastMember (Lift Sampler) fs)
  => Int                                          -- ^ number of MH steps
  -> Int                                          -- ^ number of particles
  -> [Tag]                                        -- ^ parameter names
  -> STrace                                       -- ^ initial sample trace
  -> ProbProg a                                   -- ^ probabilistic program
  -> Prog fs [(a, (LogP, STrace))]
pmmhInternal mh_steps n_particles tags strace_0 =
  handleAccept tags . metropolisLoop mh_steps (ctx_0, strace_0) (handleModel n_particles tags)
  where
    ctx_0 = LogP 0

{- | Handle probabilistic program using MH and compute the average log-probability using SMC.
-}
handleModel ::
     Int                                          -- ^ number of particles
  -> [Tag]                                        -- ^ parameter names
  -> (LogP, STrace)                               -- ^ proposed initial log-prob + sample trace
  -> ProbProg a                                   -- ^ probabilistic program
  -> Sampler (a, (LogP, STrace))                  -- ^ proposed final log-prob + sample trace
handleModel n_particles tags (_, strace) prog = do
  (a, strace') <- (Metropolis.reuseSamples strace . SIM.handleObs) prog
  let params = filterTrace tags strace'
  prts   <- ( handleLift
            . SMC.handleResampleMul
            . SIS.sis n_particles (((fst <$>) . Metropolis.reuseSamples params) . SMC.handleObs)) prog
  let logZ = logMeanExp (map (SMC.particleLogProb . snd) prts)
  pure (a, (logZ, strace'))

{- | An acceptance mechanism for PMMH.
-}
handleAccept :: LastMember (Lift Sampler) fs
  => [Tag]                                      -- ^ parameter names
  -> Prog (Accept LogP : fs) a -> Prog fs a
handleAccept tags = loop where
  loop (Val x)   = pure x
  loop (Op op k) = case discharge op of
    Right (Propose (_, strace))
      ->  do (_, prp_strace) <- lift (MH.propose tags strace)
             let prp_ctx = LogP 0
             (loop . k) (prp_ctx, prp_strace)
    Right (Accept log_p log_p')
      ->  do u <- lift $ sample (mkUniform 0 1)
             (loop . k) (expLogP (log_p' - log_p) > u)
    Left op' -> Op op' (loop . k)