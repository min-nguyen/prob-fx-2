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

module Old.Inference.PMMH where

import Control.Monad
import Effects.Dist
import Effects.ObsReader
import Effects.Lift
import qualified Data.Map as Map
import Freer
import Sampler
import Model
import ModelEnv
import STrace
import qualified Inference.MH as MH
import qualified Inference.SMC as SMC
import qualified Inference.SIS as SIS
import Util

type PMMHTrace a = [(a, SDTrace, SIS.LogP)]

pmmh :: forall es a b env e. (es ~ '[ObsReader env, Dist, Lift Sampler], FromSTrace env, Show a)
   => Int                              -- Number of mhSteps
   -> Int                              -- Number of particles
   -> Model env es a                   -- Model
   -> ModelEnv env                     -- List of model observed variables
   -> [Tag]                            -- Tags indicated sample sites of interest
   -> Sampler [(a, ModelEnv env, SIS.LogP)]  -- Trace of all accepted outputs, samples, and logps
pmmh mh_steps n_particles model env_0 tags = do
  -- Perform initial run of mh
  mhTrace <- pmmhWithSTrace mh_steps n_particles (runDist . runObsReader env_0 $ runModel model) Map.empty tags
  return (map (mapsnd3 (fromSDTrace @env)) mhTrace)

pmmhWithSTrace :: (es ~ '[Observe, Sample, Lift Sampler], Show a)
   => Int                              -- Number of mhSteps
   -> Int                              -- Number of particles
   -> Prog es a
   -> SDTrace
   -> [Tag]                            -- Tags indicated sample sites of interest
   -> Sampler (PMMHTrace a)            -- Trace of all accepted outputs, samples, and logps
pmmhWithSTrace mh_steps n_particles prog samples_0 tags = do
  -- perform initial run of mh
  let α_0 = ("", 0)
  (y_0, samples_0, _) <- MH.runMH samples_0 α_0 prog
  -- get samples of prior parameters
  let priorSamples_0 = Map.filterWithKey (\(tag, i) _ -> tag `elem` tags) samples_0
  printS $ "Prior samples: " ++ show priorSamples_0
  -- perform initial run of smc to compute likelihood
  ctxs <- SIS.sis 10 SMC.smcResampler SMC.smcPopulationHandler SMC.runObserve (runSample priorSamples_0) prog
  let -- get final log probabilities of each particle
      lps     = map (snd3 . snd) ctxs
      -- compute average
      logW_0   = SIS.logMeanExp lps
  -- A function performing n pmmhsteps
  let pmmhs  = foldl (>=>) return (replicate mh_steps (pmmhStep n_particles prog tags))
  l <- pmmhs [(y_0, samples_0, logW_0)]
  -- Return pmmhTrace in correct order of execution (due to pmmhStep prepending new results onto head of trace)
  return $ reverse l

pmmhStep :: Show a => (es ~ '[Observe, Sample, Lift Sampler])
  => Int                -- Number of particles
  -> Prog es a          -- Model
  -> [Tag]              -- Tags indicating prior random variables
  -> PMMHTrace a        -- Trace of previous mh outputs
  -> Sampler (PMMHTrace a)
pmmhStep n_particles model tags trace = do
  let -- get previous mh output
      (x, samples, logW) = head trace
  -- select new sample address
  let sampleSites = if null tags then samples
                    else  Map.filterWithKey (\(tag, i) _ -> tag `elem` tags) samples
  α_samp_ind <- sample (DiscrUniformDist 0 (Map.size sampleSites - 1) Nothing Nothing)
  let (α_samp, _) = Map.elemAt α_samp_ind sampleSites
  -- run mh with new sample address
  (x', samples', _) <- MH.runMH samples α_samp model
  -- get proposal prior samples to reuse for smc
  let priorSamples = Map.filterWithKey (\(tag, i) _ -> tag `elem` tags) samples'
  printS $ "prior samples" ++ show priorSamples
  -- run SMC using prior samples
  ctxs <- SIS.sis n_particles SMC.smcResampler SMC.smcPopulationHandler SMC.runObserve (runSample priorSamples) model
  let -- get final log probabilities of each particle
      lps     = map (snd3 . snd) ctxs
      -- compute average
      logW'   = SIS.logMeanExp lps
      -- compute acceptance ratio to see if we use samples or samples'
      {-  if logW' and logW = -Infinity, this ratio can be NaN which is fine.
          if logW > -Infinity and logW = -Infinity, this ratio can be Infinity, which is fine. -}
      acceptance_ratio = exp (SIS.logP $ logW' - logW)
  -- printS $ "logW' : " ++ show logW' ++ " logW: " ++ show logW
  u <- sample (UniformDist 0 1 Nothing Nothing)
  -- printS $ "acceptance: " ++ show acceptance_ratio ++ " u: " ++ show u
  if u < acceptance_ratio
     then do return ((x', samples', logW'):trace)
     else do return trace

runSample :: SDTrace -> Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
runSample  samples = loop
  where
  loop :: Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
  loop (Val x) = return x
  loop (Op u k) = case u of
      PrintPatt s ->
        lift (liftS (putStrLn s)) >> loop (k ())
      SampPatt d α ->
        do let maybe_y = MH.lookupSample samples d α
          --  printLift $ "using " ++ show maybe_y
           case maybe_y of
             Nothing -> lift (sample d) >>= (loop . k)
             Just x  -> (loop . k) x
      DecompLeft u' ->
         Op u' (loop . k)
