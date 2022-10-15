{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

{- | Particle Marginal Metropolis-Hastings inference.
-}

module Inference.PMMH where

import Control.Monad
import Prog
import Sampler
import LogP
import Trace
import Effects.Dist
import PrimDist
import Model
import Env
import Effects.Lift
import Effects.ObsRW
import qualified Data.Map as Map
import qualified Inference.SIM as SIM
import qualified Inference.MH as MH
import Inference.ARS as ARS
import qualified Inference.SIS as SIS
import qualified Inference.SMC as SMC
import Util
import Inference.SMC (Particle(particleLogProb, Particle))

{- | Top-level wrapper for PMMH inference.
-}
pmmh :: forall env es a xs. (env `ContainsVars` xs)
  => Int                                            -- ^ number of MH steps
  -> Int                                            -- ^ number of particles
  -> Model env [ObsRW env, Dist, Lift Sampler] a    -- ^ model
  -> Env env                                        -- ^ input environment
  -> Vars xs                                        -- ^ variable names of model parameters
  -> Sampler [Env env]                              -- ^ output environments
pmmh mh_steps n_particles model env_in obs_vars = do
  -- | Handle model to probabilistic program
  let prog_0   = handleCore env_in model
      strace_0 = Map.empty
  -- | Convert observable variables to strings
  let tags = varsToStrs @env obs_vars
  pmmh_trace <- (handleLift . SIM.handleSamp . SIM.handleObs)
                (pmmhInternal mh_steps n_particles tags strace_0 prog_0)
  pure (map (snd . fst . fst) pmmh_trace)

{- | PMMH inference on a probabilistic program.
-}
pmmhInternal :: (ProbSig es)
  => Int
  -> Int
  -> [Tag]
  -> STrace
  -> Prog es a
  -> Prog es [((a, LogP), STrace)]
pmmhInternal mh_steps n_particles tags strace_0 =
  arLoop mh_steps strace_0 (handleModel n_particles tags) (handleAccept tags)

{- | Handle probabilistic program using MH and compute the average log-probability using SMC.
-}
handleModel :: ProbSig es
  => Int                                          -- ^ number of particles
  -> [Tag]                                        -- ^ tags indicating model parameters
  -> STrace                                       -- ^ sample traces
  -> Prog es a                                    -- ^ probabilistic program
  -> Prog es ((a, LogP), STrace)
handleModel n_particles tags strace prog = do
  ((a, _), strace') <- MH.handleModel strace prog
  let params = filterTrace tags strace'
  prts   <- ( (map snd . fst <$>)
            . MH.handleSamp params
            . SMC.smcInternal n_particles) prog
  let logZ  = logMeanExp (map SMC.particleLogProb prts)
  pure ((a, logZ), strace')

{- | An acceptance mechanism for PMMH.
-}
handleAccept :: LastMember (Lift Sampler) es
  => [Tag]
  -> Prog (Accept LogP : es) a
  -> Prog es a
handleAccept tags = loop where
  loop (Val x)   = pure x
  loop (Op op k) = case discharge op of
    Right (Propose strace lptrace)
        ->  do  let αs = Map.keys (if Prelude.null tags then strace else filterTrace tags strace)
                α <- lift (sample (UniformD 0 (length αs - 1))) >>= pure . (αs !!)
                r <- lift sampleRandom
                (loop . k) (α, r)
    Right (Accept α log_p log_p')
      ->  do  let acceptance_ratio = (exp . unLogP) (log_p' - log_p)
              u <- lift $ sample (Uniform 0 1)
              (loop . k) (u < acceptance_ratio)
    Left op' -> Op op' (loop . k)