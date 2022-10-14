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
  let prog = (handleDist . handleObsRW env_in) (runModel model)
  -- | Convert observable variables to strings
      tags = varsToStrs @env obs_vars
  -- | Run PMMH
  pmmh_trace <- (handleLift . SIM.handleSamp . SIM.handleObs . handleAccept)
                        (pmmhInternal mh_steps n_particles prog Map.empty tags)
  -- Return the accepted model environments
  pure (map (snd . fst . fst) pmmh_trace)

{- | Perform PMMH on a probabilistic program.
-}
pmmhInternal :: (Members [Observe, Sample] es, LastMember (Lift Sampler) es) =>
      Int                                     -- ^ number of MH steps
   -> Int                                     -- ^ number of particles
   -> Prog es a  -- ^ probabilistic program
   -> STrace                               -- ^ initial sample trace
   -> [Tag]                                   -- ^ tags indicating the model parameters
   -> Prog (MH.Accept LogP : es) [((a, LogP), STrace)]       -- ^ trace of accepted outputs, samples, and logps
pmmhInternal mh_steps n_particles prog strace param_tags = do
  let mh_prog = weakenProg prog
  -- | Perform initial run of MH
  pmmh_0 <- runPMMH n_particles mh_prog param_tags strace
  -- | A function performing n pmmhsteps
  foldl (>=>) pure (replicate mh_steps (pmmhStep n_particles mh_prog param_tags)) [pmmh_0]

{- | Perform one iteration of PMMH by drawing a new sample and then rejecting or accepting it.
-}
pmmhStep ::  (Members [Observe, Sample] es, LastMember (Lift Sampler) es) =>
     Int                                          -- ^ number of particles
  -> Prog (MH.Accept LogP : es) a       -- ^ probabilistic program
  -> [Tag]                                        -- ^ tags indicating model parameters
  -> [((a, LogP), STrace)]                     -- ^ trace of previous mh outputs
  -> Prog (MH.Accept LogP : es) [((a, LogP), STrace)]
pmmhStep n_particles prog tags pmmh_trace = do
  let pmmh_ctx@(_, strace) = head pmmh_trace
  -- | Propose a new random value for a sample site
  (α_samp, r) <- lift (MH.propose strace tags)
  -- | Run one iteration of PMMH
  pmmh_ctx'   <- runPMMH n_particles prog tags (Map.insert α_samp r strace)
  b <- lift (accept pmmh_ctx pmmh_ctx')
  if b then pure (pmmh_ctx':pmmh_trace)
       else pure pmmh_trace

{- | Handle probabilistic program using MH and compute the average log-probability using SMC.
-}
runPMMH :: (Members [Observe, Sample] es, LastMember (Lift Sampler) es)
  => Int                                          -- ^ number of particles
  -> Prog es a      -- ^ probabilistic program
  -> [Tag]                                        -- ^ tags indicating model parameters
  -> STrace                                    -- ^ sample traces
  -> Prog es ((a, LogP), STrace)
runPMMH n_particles prog tags strace = do
  ((a, _), strace') <- MH.runMH strace prog
  let params = filterTrace tags strace'
  prts   <- ( (map snd . fst <$>)
            . MH.handleSamp params
            . SIS.sis n_particles SMC.particleRunner SMC.particleResampler) prog
  let logZ  = logMeanExp (map SMC.particleLogProb prts)
  pure ((a, logZ), strace')

{- | An acceptance mechanism for PMMH.
-}
accept ::
     ((a, LogP), STrace)                 -- ^ previous PMMH ctx
  -> ((a, LogP), STrace)                 -- ^ proposed PMMH ctx
  -> Sampler Bool
accept ((_, log_p), _) ((_, log_p'), _) = do
  let acceptance_ratio = (exp . unLogP) (log_p' - log_p)
  u <- sample (Uniform 0 1)
  pure (u < acceptance_ratio)

handleAccept :: LastMember (Lift Sampler) es => Prog (MH.Accept LogP : es) a -> Prog es a
handleAccept (Val x)   = pure x
handleAccept (Op op k) = case discharge op of
  Right (MH.Accept α ((_, log_p), _) ((a, log_p'), _))
    ->  do  let acceptance_ratio = (exp . unLogP) (log_p' - log_p)
            u <- lift $ sample (Uniform 0 1)
            handleAccept $ k (u < acceptance_ratio)
  Left op' -> Op op' (handleAccept . k)