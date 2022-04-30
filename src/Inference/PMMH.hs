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

-- mh :: forall es a b env e. (es ~ '[ObsReader env, Dist, Lift Sampler], FromSTrace env)
--    => Int                              -- Number of mhSteps per data point
--    -> (b -> Model env es a)            -- Model awaiting input variable
--    -> [Tag]                            -- Tags indicated sample sites of interest
--    -> b                                -- List of model input variables
--    -> ModelEnv env                     -- List of model observed variables
--    -> Sampler [(a, ModelEnv env, LPTrace)]  -- Trace of all accepted outputs, samples, and logps
-- mh n model tags x_0 env_0 = do
--   -- Perform initial run of mh
--   mhTrace <- mhWithSTrace n (runDist . runObsReader env_0 $ runModel (model x_0)) Map.empty tags
--   return (map (mapsnd3 (fromSDTrace @env)) mhTrace)

-- mhWithSTrace :: (es ~ '[Observe, Sample, Lift Sampler])
--    => Int                              -- Number of mhSteps per data point
--    -> Prog es a
--    -> SDTrace
--    -> [Tag]                            -- Tags indicated sample sites of interest
--    -> Sampler (TraceMH a)              -- Trace of all accepted outputs, samples, and logps
-- mhWithSTrace n prog samples_0 tags = do
--   -- Perform initial run of mh
--   let α_0 = ("", 0)
--   (y_0, samples_0, logps_0) <- runMH samples_0 α_0 prog
--   -- A function performing n mhsteps for one data point
--   let mhs  = foldl (>=>) return (replicate n (mhStep prog tags))
--   -- Construct list of n mhsteps, one for each data point
--   -- Perform mhNstep for each data point, propagating (x, samples, logps) through
--   l <- mhs [(y_0, samples_0, logps_0)]
--   -- Return mhTrace in correct order of execution (due to mhStep prepending new results onto head of trace)
--   return $ reverse l

pmmhStep :: Show a => (es ~ '[Observe, Sample, Lift Sampler])
  => Prog es a          -- Model
  -> [Tag]              -- Tags indicating prior random variables
  -> PMMHTrace a        -- Trace of previous mh outputs
  -> Sampler (PMMHTrace a)
pmmhStep model tags trace = do
  let -- Get previous mh output
      (x, samples, logW) = head trace
  let sampleSites = if null tags then samples
                    else  Map.filterWithKey (\(tag, i) _ -> tag `elem` tags) samples
  α_samp_ind <- sample (DiscrUniformDist 0 (Map.size sampleSites - 1) Nothing Nothing)

  let (α_samp, _) = Map.elemAt α_samp_ind sampleSites
  -- run mh with new sample address
  (x', samples', _) <- MH.runMH samples α_samp model
  -- get prior samples to reuse for smc
  let priorSamples = Map.filterWithKey (\(tag, i) _ -> tag `elem` tags) samples'

  -- run SIS using prior samples
  ctxs <- SIS.sis 100 SMC.smcResampler SMC.smcPopulationHandler SMC.runObserve (runSample priorSamples) model

  let -- get final log probabilities of each particle
      lps     = map (snd3 . snd) ctxs
      -- compute average
      logW'   = SIS.logMeanExp lps
      -- do some acceptance ratio to see if we use samples or samples'
      acceptance_ratio = logW'/logW
  -- liftS $ print $ "acceptance ratio" ++ show acceptance_ratio
  u <- sample (UniformDist 0 1 Nothing Nothing)

  if SIS.LogP u < acceptance_ratio
    then do -- liftS $ putStrLn $ "Accepting " -- ++ show (Map.lookup α_samp samples') -- ++ show logps' ++ "\nover      "
            -- ++ show logps
            -- ++ "\nwith α" ++ show α_samp ++ ": "
            -- ++ show acceptance_ratio ++ " > " ++ show u
            -- liftS $ print "accepting"
            return ((x', samples', logW'):trace)
    else do
            -- liftS $ putStrLn $ "Rejecting " ++ show (Map.lookup α_samp samples') -- ++ show logps' ++ "\nover      "
            -- ++ show logps
            --  ++ "\nwith α" ++ show α_samp ++ ": "
            --  ++ show acceptance_ratio ++ " < u: " ++ show u
            return trace

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
           case maybe_y of
             Nothing -> lift (sample d) >>= (loop . k)
             Just x  -> (loop . k) x
      DecompLeft u' ->
         Op u' (loop . k)
