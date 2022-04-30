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
import Inference.SIS (sis, Accum(..), Resampler, LogP(..), logMeanExp)
import qualified OpenSum as OpenSum
import OpenSum (OpenSum)
import Util


smc :: forall env es' a. (FromSTrace env, Show a) =>
  (es' ~ [ObsReader env, Dist, Lift Sampler]) =>
  Int -> Model env es' a -> ModelEnv env -> Sampler [(a, LogP, ModelEnv env)]
smc n_particles model env = do
  as_ps_straces <- sis n_particles smcResampler smcPopulationHandler runObserve runSample  model env
  return $ map (\(a, (p, strace)) -> (a, p, fromSDTrace @env strace)) as_ps_straces

smcPopulationHandler :: Members [Observe, Sample] es
  =>         [Prog (NonDet:es) a]
  -> Prog es [(Prog (NonDet:es) a, (LogP, SDTrace))]
smcPopulationHandler progs = do
  -- Merge particles into single non-deterministic program using 'asum', and run to next checkpoint
  progs_ctxs <- (runNonDet . runState Map.empty . traceSamples . breakObserve ) (asum progs)
  let progs_ctxs' = map (\((prog, p), strace) -> (prog, (p, strace))) progs_ctxs
  return progs_ctxs'

smcResampler :: Member Sample es => Resampler (LogP, SDTrace) es a
smcResampler logWs_straces_0 logWs_straces_1sub0 progs = do
  let -- for each particle, compute normalised accumulated log weights, and accumulated sample traces
      (logWs_1    , straces_1)      = unzip $ accum logWs_straces_1sub0 logWs_straces_0
      n_particles = length progs
  prinT $ "LogWs " ++ show logWs_1
  prinT $ "Resampling probabilities " ++ show (map (exp . logP) logWs_1)
  -- prinT $ show straces_1
  -- Select particles to continue with
  particle_idxs :: [Int] <- replicateM n_particles $ send (Sample (DiscreteDist (map (exp . logP) logWs_1) Nothing Nothing) undefined)
  let resampled_progs         = map (progs !!) particle_idxs
      resampled_logWs         = map (logWs_1 !!) particle_idxs
      resampled_straces       = map (straces_1 !!) particle_idxs
  -- prinT $ "continuing with" ++ show   straces'
  return (resampled_progs, zip resampled_logWs resampled_straces)

traceSamples :: (Member Sample es) => Prog es a -> Prog (State SDTrace : es) a
traceSamples  (Val x)  = return x
traceSamples  (Op u k) = case u of
    SampPatt d α ->  Op (weaken u) (\x -> do updateSDTrace α d x
                                             traceSamples (k x))
    _   -> Op (weaken u) (traceSamples . k)

-- When discharging Observe, return the rest of the program, and the log probability
breakObserve :: Member Observe es => Prog es a -> Prog es (Prog es a, LogP)
breakObserve  (Val x) = return (Val x, 0)
breakObserve  (Op op k) = case op of
      ObsPatt d y α -> do
        let logp = logProb d y
        -- prinT $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show logp
        Val (k y, LogP logp)
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

-- smcResampler :: Member Sample es => Resampler (Double, STrace) es a
-- smcResampler logWs_straces_0 logWs_straces_1sub0 progs = do
--   let -- Get log weights and sample traces from previous particle run
--       (logWs_0    , straces_0)      = unzip logWs_straces_0
--       -- Get log weights and sample traces since previous particle run
--       (logWs_1sub0, straces_1sub0)  = unzip logWs_straces_1sub0

--   let -- Accumulate sample traces
--       straces_1 = zipWith Map.union straces_1sub0 straces_0
--       -- Compute normalized log probabilities of current particles = log (Ws_1sub0 / sum Ws_0)
--       logWs_1 = map (log . ((/ sum (map exp logWs_0)) . exp)) logWs_1sub0
--       n_particles = length progs
--   prinT $ "LogWs " ++ show logWs_1
--   -- Select particles to continue with
--   particle_idxs :: [Int] <- replicateM n_particles $ send (Sample (DiscreteDist (map exp logWs_1) Nothing Nothing) undefined)
--   -- prinT $ "particle idx " ++ show particle_idxs
--   let progs'         = map (progs !!) particle_idxs
--       logWs'         = map (logWs_1 !!) particle_idxs
--       straces'       = map (straces_1 !!) particle_idxs
--   -- prinT $ "continuing with" ++ show   straces'
--   return (progs', zip logWs' straces')
