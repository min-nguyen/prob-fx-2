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

module Inference.SMC where

import Control.Monad
import PrimDist
import Env
import Prog
import Model
import LogP
import Trace
import Sampler
import Effects.Dist
import Effects.NonDet
import Effects.Lift
import Effects.ObsReader
import Effects.State
import qualified Inference.SIS as SIS
import qualified Inference.SIM as SIM
import OpenSum (OpenSum)

smcToplevel :: forall env es' a. (FromSTrace env, Show a) =>
  (es' ~ [ObsReader env, Dist, Lift Sampler]) =>
  Int -> Model env es' a -> Env env -> Sampler [(a, LogP, Env env)]
smcToplevel n_particles model env = do
  let prog_0 = (handleDist . handleObsRead env) (runModel model)
  smc n_particles prog_0 env

smc :: forall env es' a. (FromSTrace env, Show a) =>
  (es' ~ [Observe, Sample, Lift Sampler]) =>
  Int -> Prog es' a -> Env env -> Sampler [(a, LogP, Env env)]
smc n_particles prog env = do
  as_ps_straces <- SIS.sis n_particles smcResampler smcPopulationHandler SIM.handleObs SIM.handleSamp prog
  pure $ map (\(a, (addr, p, strace)) -> (a, p, fromSTrace @env strace)) as_ps_straces

smcPopulationHandler :: Members [Observe, Sample, Lift Sampler] es
  => SIS.ParticleHandler  ([Addr], LogP, STrace) es a
smcPopulationHandler particles = do
  -- Merge particles into single non-deterministic program using 'asum', and run to next checkpoint
  particles_ctxs <- (handleNonDet . traceSamples . breakObserve ) (asum particles)
  -- List of particles that can be resumed, their observe breakpoint address, the log probability at that break point, and an accumulated sample trace
  let particles_ctxs' = map (\((prog, α, p), strace) -> (prog, ([α], p,  strace))) particles_ctxs
  pure particles_ctxs'

smcResampler :: LastMember (Lift Sampler) es => SIS.Resampler ([Addr], LogP, STrace) es a
smcResampler ctxs_0 ctxs_1sub0 particles = do
  -- for each particle, compute normalised accumulated log weights, and accumulated sample traces
  let (obs_addrs_1, logWs_1, straces_1)      = unzip3 $ SIS.accum ctxs_1sub0 ctxs_0
      n_particles = length particles
  -- printLift $ "LogWs " ++ show logWs_1
  -- printLift $ "Resampling probabilities " ++ show (map (exp . logP) logWs_1)
  -- Select particles to continue with
  particle_idxs :: [Int] <- replicateM n_particles $  lift (sample (DiscreteDist (map (exp . logP) logWs_1)))
  let resampled_particles     = map (particles !!) particle_idxs
      resampled_logWs         = map (logWs_1 !!) particle_idxs
      resampled_straces       = map (straces_1 !!) particle_idxs
  -- prinT $ "continuing with" ++ show   straces'
  pure (resampled_particles, zip3 obs_addrs_1 resampled_logWs resampled_straces)

-- When discharging Observe, return the rest of the program, and the log probability
breakObserve :: Members [Lift Sampler, Observe] es => Prog es a -> Prog es (Prog es a, Addr, LogP)
breakObserve  (Val x) = pure (Val x, ("", 0), 0)
breakObserve  (Op op k) = case op of
      ObsPatt d y α -> do
        let logp = logProb d y
        -- printLift $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show logp
        Val (k y, α, LogP logp)
      _ -> Op op (breakObserve . k)