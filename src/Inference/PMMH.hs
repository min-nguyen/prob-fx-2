{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Inference.PMMH where

import qualified Data.Map as Map
import Control.Monad
import Effects.Dist
import Effects.ObsReader
import Effects.Lift
import Prog
import PrimDist
import Sampler
import Model
import Env
import Trace
import LogP
import qualified Inference.MH as MH
import qualified Inference.SMC as SMC
import qualified Inference.SIS as SIS
import qualified Inference.SIM as SIM
import Util

type PMMHTrace a = MH.MHTrace LogP a

pmmhTopLevel :: forall es a env xs.
  (es ~ '[ObsReader env, Dist, Lift Sampler], FromSTrace env, ValidSpec env xs, Show a)
   => Int                                    -- Number of mhSteps
   -> Int                                    -- Number of particles
   -> Model env es a                         -- Model
   -> Env env                           -- List of model observed variables
   -> ObsVars xs                             -- Tags indicated sample sites of interest
   -> Sampler [(a, Env env, LogP)]  -- Trace of all accepted outputs, samples, and logps
pmmhTopLevel mh_steps n_particles model env obsvars = do
  let prog = (handleDist . handleObsRead env) (runModel model)
      tags = asTags @env obsvars
  -- Perform initial run of mh
  mhTrace <- pmmh mh_steps n_particles prog Map.empty tags
  return (map (mapsnd3 (fromSTrace @env)) mhTrace)

pmmh :: (es ~ '[Observe, Sample, Lift Sampler], Show a)
   => Int                              -- Number of mhSteps
   -> Int                              -- Number of particles
   -> Prog es a                        -- Model
   -> STrace                           -- Initial sample trace
   -> [Tag]                            -- Tags indicated sample sites of interest
   -> Sampler (PMMHTrace a)            -- Trace of all accepted outputs, samples, and logps
pmmh mh_steps n_particles prog strace_0 tags = do
  -- perform initial run of mh
  let α_0 = ("", 0)
  (y_0, strace_0, _) <- MH.runMH strace_0 α_0 prog
  -- get samples of prior parameters
  let priorSamples_0 = filterSTrace tags strace_0
  -- perform initial run of smc to compute likelihood
  ctxs <- SIS.sis n_particles SMC.smcResampler SMC.smcPopulationHandler SIM.handleObs (handleSamp priorSamples_0) prog
  let -- get final log probabilities of each particle
      lps     = map (snd3 . snd) ctxs
      -- compute average
      logW_0  = logMeanExp lps

  -- A function performing n pmmhsteps
  let pmmhs  = foldl (>=>) return (replicate mh_steps (pmmhStep n_particles prog tags))
  l <- pmmhs [(y_0, strace_0, logW_0)]
  -- Return pmmhTrace in correct order of execution (due to pmmhStep prepending new results onto head of trace)
  return $ reverse l

pmmhStep :: (es ~ '[Observe, Sample, Lift Sampler], Show a)
  => Int                -- Number of particles
  -> Prog es a          -- Model
  -> [Tag]              -- Tags indicating prior random variables
  -> PMMHTrace a        -- Trace of previous mh outputs
  -> Sampler (PMMHTrace a)
pmmhStep n_particles prog tags pmmhTrace =
  MH.mhStep prog tags (acceptSMC n_particles prog tags) pmmhTrace

acceptSMC :: Show a => Int -> Prog '[Observe, Sample, Lift Sampler] a -> [Tag] -> MH.Accept LogP a
acceptSMC n_particles prog tags _ (a, strace', lptrace') (_, _, logW) = do
  let priorSamples = filterSTrace tags strace'
  printS $ "prior samples" ++ show priorSamples
  -- run SMC using prior samples
  ctxs <- SIS.sis n_particles SMC.smcResampler SMC.smcPopulationHandler SIM.handleObs (handleSamp priorSamples) prog
  let -- get final log probabilities of each particle
      lps     = map (snd3 . snd) ctxs
      -- compute average
      logW'   = logMeanExp lps
      -- compute acceptance ratio to see if we use samples or samples'
      {-  if logW' and logW = -Infinity, this ratio can be NaN which is fine, in which case comparing u < Nan returns false
          if logW > -Infinity and logW = -Infinity, this ratio can be Infinity, which is fine. -}
      acceptance_ratio = exp (logP $ logW' - logW)
  return ((a, strace', logW'), acceptance_ratio)

handleSamp :: STrace -> Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
handleSamp  samples = loop
  where
  loop :: Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
  loop (Val x) = return x
  loop (Op u k) = case u of
      PrintPatt s ->
        printLift s >> loop (k ())
      SampPatt d α ->
        do let maybe_y = MH.lookupSample samples d α
          --  printLift $ "using " ++ show maybe_y
           case maybe_y of
             Nothing -> lift (sample d) >>= (loop . k)
             Just x  -> (loop . k) x
      DecompLeft u' ->
         Op u' (loop . k)
