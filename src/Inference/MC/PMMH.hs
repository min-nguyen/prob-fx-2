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
import qualified Inference.MC.SIM as SIM
import qualified Inference.MC.MH as MH
import Inference.MC.Metropolis as Metropolis
import qualified Inference.MC.SIS as SIS
import qualified Inference.MC.SMC as SMC

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
      trace_0 = Map.empty
  -- | Convert observable variables to strings
  let tags = varsToStrs @env obs_vars
  pmmh_trace <- handleLift (pmmhInternal mh_steps n_prts tags trace_0 prog_0)
  pure (map (snd . fst) pmmh_trace)

{- | PMMH inference on a probabilistic program.
-}
pmmhInternal :: (HasSampler fs)
  => Int                                          -- ^ number of MH steps
  -> Int                                          -- ^ number of particles
  -> [Tag]                                        -- ^ parameter names
  -> Trace                                       -- ^ initial sample trace
  -> ProbProg a                                   -- ^ probabilistic program
  -> Prog fs [(a, (LogP, Trace))]
pmmhInternal mh_steps n_prts tags trace_0 =
  handleAccept tags . metropolisLoop mh_steps (s_0, trace_0) (handleModel n_prts tags)
  where
    s_0 = LogP 0

{- | Handle probabilistic program using MH and compute the average log-probability using SMC.
-}
handleModel ::
     Int                                          -- ^ number of particles
  -> [Tag]                                        -- ^ parameter names
  -> ProbProg a                                   -- ^ probabilistic program
  -> (LogP, Trace)                               -- ^ proposed initial log-prob + sample trace
  -> Sampler (a, (LogP, Trace))                  -- ^ proposed final log-prob + sample trace
handleModel n_prts tags  prog (_, trace)  = do
  (a, trace') <- (Metropolis.reuseSamples trace . SIM.handleObs) prog
  let params = filterTrace tags trace'
  prts   <- ( handleLift
            . SMC.handleResampleMul
            . SIS.sis n_prts (((fst <$>) . Metropolis.reuseSamples params) . SMC.handleObs) (LogP 0)) prog
  let logZ = logMeanExp (map snd prts)
  pure (a, (logZ, trace'))

{- | An acceptance mechanism for PMMH.
-}
handleAccept :: HasSampler fs
  => [Tag]                                      -- ^ parameter names
  -> Prog (Accept LogP : fs) a -> Prog fs a
handleAccept tags = loop where
  loop (Val x)   = pure x
  loop (Op op k) = case discharge op of
    Right (Propose (_, trace))
      ->  do (_, prp_trace) <- lift (MH.propose tags trace)
             let prp_s = LogP 0
             (loop . k) (prp_s, prp_trace)
    Right (Accept log_p log_p')
      ->  do u <- lift $ sample (mkUniform 0 1)
             (loop . k) (expLogP (log_p' - log_p) > u)
    Left op' -> Op op' (loop . k)