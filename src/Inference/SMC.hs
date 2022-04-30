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
-- import Data.Extensible hiding (Member)
import qualified Data.Map as Map
import Data.Maybe
import Data.Bifunctor
import Data.Map (Map)
import ModelEnv
import Control.Monad
import Control.Applicative

import Effects.Dist
import Freer
import Model
import Effects.NonDet
import Sampler
import Effects.Lift
import Effects.ObsReader
import Effects.State
import STrace
import Sampler
import Effects.Writer
import Effects.Dist
import Inference.SIS (sis, Accum(..), ParticleHandler, Resampler, LogP(..), logMeanExp)
import qualified OpenSum as OpenSum
import OpenSum (OpenSum)
import Util

smcToplevel :: forall env es' a. (FromSTrace env, Show a) =>
  (es' ~ [ObsReader env, Dist, Lift Sampler]) =>
  Int -> Model env es' a -> ModelEnv env -> Sampler [(a, LogP, ModelEnv env)]
smcToplevel n_particles model env = do
  let prog_0 = (runDist . runObsReader env) (runModel model)
  smc n_particles prog_0 env

smc :: forall env es' a. (FromSTrace env, Show a) =>
  (es' ~ [Observe, Sample, Lift Sampler]) =>
  Int -> Prog es' a -> ModelEnv env -> Sampler [(a, LogP, ModelEnv env)]
smc n_particles prog_0 env = do
  as_ps_straces <- sis n_particles smcResampler smcPopulationHandler runObserve runSample  prog_0
  return $ map (\(a, (addr, p, strace)) -> (a, p, fromSDTrace @env strace)) as_ps_straces


smcPopulationHandler :: Members [Observe, Sample, Lift Sampler] es
  => ParticleHandler  ([Addr], LogP, SDTrace) es a
smcPopulationHandler progs = do
  -- Merge particles into single non-deterministic program using 'asum', and run to next checkpoint
  progs_ctxs <- (runNonDet . runState Map.empty . traceSamples . breakObserve ) (asum progs)
  -- List of particles that can be resumed, their observe breakpoint address, the log probability at that break point, and an accumulated sample trace
  let progs_ctxs' = map (\((prog, α, p), strace) -> (prog, ([α], p,  strace))) progs_ctxs
  return progs_ctxs'

smcResampler :: Member (Lift Sampler) es => Resampler ([Addr], LogP, SDTrace) es a
smcResampler logWs_straces_0 logWs_straces_1sub0 progs = do
  let -- for each particle, compute normalised accumulated log weights, and accumulated sample traces
      (obs_addrs_0, logWs_0, straces_0) = unzip3  logWs_straces_0
  printLift $ "LogWs0 " ++ show logWs_0
  let (obs_addrs_1sub, logWs_1sub0, straces_1sub0) = unzip3  logWs_straces_1sub0
  printLift $ "LogWs1sub0 " ++ show logWs_1sub0
  let (obs_addrs_1, logWs_1, straces_1)      = unzip3 $ accum logWs_straces_1sub0 logWs_straces_0
      n_particles = length progs
  printLift $ "LogWs " ++ show logWs_1
  printLift $ "Resampling probabilities " ++ show (map (exp . logP) logWs_1)
  -- Select particles to continue with
  particle_idxs :: [Int] <- replicateM n_particles $  lift (sample (DiscreteDist (map (exp . logP) logWs_1) Nothing Nothing))
  let resampled_progs         = map (progs !!) particle_idxs
      resampled_logWs         = map (logWs_1 !!) particle_idxs
      resampled_straces       = map (straces_1 !!) particle_idxs
  -- prinT $ "continuing with" ++ show   straces'
  return (resampled_progs, zip3 obs_addrs_1 resampled_logWs resampled_straces)

traceSamples :: (Member Sample es) => Prog es a -> Prog (State SDTrace : es) a
traceSamples  (Val x)  = return x
traceSamples  (Op u k) = case u of
    SampPatt d α ->  Op (weaken u) (\x -> do updateSDTrace α d x
                                             traceSamples (k x))
    _   -> Op (weaken u) (traceSamples . k)

-- When discharging Observe, return the rest of the program, and the log probability
breakObserve :: Members [Lift Sampler, Observe] es => Prog es a -> Prog es (Prog es a, Addr, LogP)
breakObserve  (Val x) = return (Val x, ("", 0), 0)
breakObserve  (Op op k) = case op of
      ObsPatt d y α -> do
        let logp = logProb d y
        printLift $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show logp
        Val (k y, α, LogP logp)
      _ -> Op op (breakObserve . k)

runSample :: Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
runSample = loop
  where
  loop :: Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
  loop (Val x) = return x
  loop (Op u k) =
    case  u of
      SampPatt d α ->
        lift (sample d) >>= \x -> loop (k x)
      PrintPatt s  ->
        lift (liftS (putStrLn s)) >>= loop . k
      DecompLeft u' ->
        Op u' (loop . k)

runObserve :: Prog (Observe : es) a -> Prog es a
runObserve  (Val x) = return x
runObserve  (Op op k) = case op of
      ObsPatt d y α -> do
        runObserve (k y)
      Other op -> Op op (runObserve . k)