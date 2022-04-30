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
import Inference.SMC hiding (runObserve)
import Inference.SIS hiding (runSample, runObserve)

-- | Perform one step of MH for a single data point
mhStep :: (es ~ '[Observe, Sample, Lift Sampler])
  => Prog es a   -- Model
  -> [Tag]            -- Tags indicating sample sites of interest
  -> MH.TraceMH a        -- Trace of previous mh outputs
  -> Sampler (MH.TraceMH a)
mhStep model tags trace = do
  let -- Get previous mh output
      (x, samples, logps) = head trace
  let sampleSites = if null tags then samples
                    else  Map.filterWithKey (\(tag, i) _ -> tag `elem` tags) samples
  α_samp_ind <- sample (DiscrUniformDist 0 (Map.size sampleSites - 1) Nothing Nothing)

  let (α_samp, _) = Map.elemAt α_samp_ind sampleSites
  -- run mh with new sample address
  (x', samples', logps') <- MH.runMH samples α_samp model
  -- get samples to reuse for smc
  let priorSamples = Map.filterWithKey (\(tag, i) _ -> tag `elem` tags) samples'
  -- do some acceptance ratio to see if we use samples or samples'
  acceptance_ratio <- liftS $ MH.accept α_samp samples samples' logps logps'
  -- liftS $ print $ "acceptance ratio" ++ show acceptance_ratio
  u <- sample (UniformDist 0 1 Nothing Nothing)

  if u < acceptance_ratio
    then do -- liftS $ putStrLn $ "Accepting " -- ++ show (Map.lookup α_samp samples') -- ++ show logps' ++ "\nover      "
            -- ++ show logps
            -- ++ "\nwith α" ++ show α_samp ++ ": "
            -- ++ show acceptance_ratio ++ " > " ++ show u
            -- liftS $ print "accepting"
            return ((x', samples', logps'):trace)
    else do
            -- liftS $ putStrLn $ "Rejecting " ++ show (Map.lookup α_samp samples') -- ++ show logps' ++ "\nover      "
            -- ++ show logps
            --  ++ "\nwith α" ++ show α_samp ++ ": "
            --  ++ show acceptance_ratio ++ " < u: " ++ show u
            return trace

-- sisCustom :: forall a env ctx es.
--      (Accum ctx, Show ctx, FromSTrace env, Show a)
--   => Int
--   -> Resampler       ctx (Observe : Sample : Lift Sampler : '[])  a
--   -> ParticleHandler ctx (Observe : Sample : Lift Sampler : '[]) a
--   -> Model env [ObsReader env, Dist, Lift Sampler] a
--   -> ModelEnv env
--   -> Sampler [(a, ctx)]
-- sisCustom n_particles resampler pophdl model env = do
--   let prog_0  = (runDist . runObsReader env) (runModel model)
--       progs   = replicate n_particles (weaken' prog_0)
--       ctxs    = aempty n_particles
--   printS $ show ctxs
--   (runLift . runSample . runObserve) (loopSIS n_particles resampler pophdl (progs, ctxs))

runSample :: Addr -> SDTrace -> Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
runSample α_samp samples = loop
  where
  loop :: Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
  loop (Val x) = return x
  loop (Op u k) = case u of
      PrintPatt s ->
        lift (liftS (putStrLn s)) >> loop (k ())
      SampPatt d α ->
        do let maybe_y = MH.lookupSample samples d α α_samp
           case maybe_y of
             Nothing -> lift (sample d) >>= (loop . k)
             Just x  -> (loop . k) x
      DecompLeft u' ->
         Op u' (loop . k)
