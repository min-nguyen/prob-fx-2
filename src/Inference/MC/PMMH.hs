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
      trace_0 = Map.empty
  -- | Convert observable variables to strings
  let tags = varsToStrs @env obs_vars
  pmmh_trace <- handleLift (pmmhInternal mh_steps n_prts tags trace_0 prog_0)
  pure (map (snd . fst . fst) pmmh_trace)

{- | PMMH inference on a probabilistic program.
-}
pmmhInternal :: (HasSampler fs)
  => Int                                          -- ^ number of MH steps
  -> Int                                          -- ^ number of particles
  -> [Tag]                                        -- ^ parameter names
  -> Trace                                       -- ^ initial sample trace
  -> ProbProg a                                   -- ^ probabilistic program
  -> Prog fs [((a, LogP), Trace)]
pmmhInternal mh_steps n_prts tags trace_0 =
  handleAccept tags . metroLoop mh_steps (0, trace_0) (handleModel n_prts tags)

{- | Handle probabilistic program using MH and compute the average log-probability using SMC.
-}
handleModel ::
     Int                                          -- ^ number of particles
  -> [Tag]                                        -- ^ parameter names
  -> ProbProg a                                   -- ^ probabilistic program
  -> (LogP, Trace)                               -- ^ proposed initial log-prob + sample trace
  -> Sampler ((a, LogP), Trace)                  -- ^ proposed final log-prob + sample trace
handleModel n tags prog (_, τ)  = do
  (a, τ') <- (reuseSamples τ . defaultObserve) prog
  let θ'  = filterTrace tags τ'
  prts    <- (handleLift . handleResampleMul
            . sis n (fmap fst . reuseSamples θ' . suspend) 0) prog
  let ρ   = logMeanExp (map snd prts)
  pure ((a, ρ), τ')

{- | An acceptance mechanism for PMMH.
-}
handleAccept :: HasSampler fs
  => [Tag]                                      -- ^ parameter names
  -> Prog (Accept LogP : fs) a -> Prog fs a
handleAccept tags = loop where
  loop (Val x)   = pure x
  loop (Op op k) = case discharge op of
    Right (Propose (_, τ))
      ->  do (_, prp_trace) <- lift (MH.propose tags τ)
             (loop . k) (0, prp_trace)
    Right (Accept lρ lρ')
      ->  do u <- random'
             (loop . k) (exp (lρ' - lρ) > u)
    Left op' -> Op op' (loop . k)