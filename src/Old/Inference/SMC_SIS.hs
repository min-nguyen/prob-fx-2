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

module Inference.SMC_SIS where
-- import Data.Extensible hiding (Member)
import qualified Data.Map as Map
import Data.Maybe
import Data.Bifunctor
import Data.Map (Map)
import ModelEnv
import Control.Monad
import Control.Applicative

import Dist
import Freer
import Model
import NonDet
import Sampler
import IO
import ObsReader
import State
import STrace
import Sampler
import Writer
import Inference.SIS (sis, Accum(..), Resampler)
import qualified OpenSum as OpenSum
import OpenSum (OpenSum)
import Util

smc :: forall env es' a. (FromSTrace env, Show a) =>
  (es' ~ [ObsReader env, Dist]) =>
  Int -> Model env es' a -> ModelEnv env -> Sampler [(a, Double, ModelEnv env)]
smc n_particles model env = do
  as_ps_straces <- sis n_particles smcResampler smcPopulationHandler model env
  return $ map (\(a, (p, strace)) -> (a, p, fromSTrace @env strace)) as_ps_straces

smcPopulationHandler :: Members [Observe, Sample] es
  =>         [Prog (NonDet:es) a]
  -> Prog es [(Prog (NonDet:es) a, (Double, STrace))]
smcPopulationHandler progs = do
  -- Merge particles into single non-deterministic program using 'asum', and run to next checkpoint
  progs_ctxs <- (runNonDet . runState Map.empty . traceSamples . breakObserve ) (asum progs)
  let progs_ctxs' = map (\((prog, p), strace) -> (prog, (p, strace))) progs_ctxs
  return progs_ctxs'

{- Text book version of SMC that uses log mean exp: this causes the log weights to tend to -infinity too fast -}

logMeanExp :: [Double] -> Double
logMeanExp logWₙₛ₁ = let _L = length logWₙₛ₁
                     in   log ( (1.0/fromIntegral _L) * (sum (map exp logWₙₛ₁)))

smcResampler :: Member Sample es => Resampler (Double, STrace) es a
smcResampler logWs_straces_0 logWs_straces_1sub0 progs = do
  let -- Get log weights and sample traces from previous particle run
      (logWs_0    , straces_0)      = unzip logWs_straces_0
      -- Get log weights and sample traces since previous particle run
      (logWs_1sub0, straces_1sub0)  = unzip logWs_straces_1sub0
      -- Compute log mean exp of previous probabilities
      logZ  = logMeanExp logWs_0
  prinT $ "logZ = " ++ show logZ
      -- Compute normalized log probabilities of current particles
  let logWs_1 = map (+ logZ) logWs_1sub0
      -- Accumulate sample traces
      straces_1 = zipWith Map.union straces_1sub0 straces_0
      n_particles = length progs
  prinT $ "LogWs " ++ show logWs_1
  -- prinT $ show straces_1
  -- Select particles to continue with
  particle_idxs :: [Int] <- replicateM n_particles $ send (Sample (DiscreteDist (map exp logWs_1) Nothing Nothing) undefined)
  -- prinT $ "particle idx " ++ show particle_idxs
  let progs'         = map (progs !!) particle_idxs
      logWs'         = map (logWs_1 !!) particle_idxs
      straces'       = map (straces_1 !!) particle_idxs
  -- prinT $ "continuing with" ++ show   straces'
  return (progs', zip logWs' straces')

traceSamples :: (Member Sample es) => Prog es a -> Prog (State STrace : es) a
traceSamples  (Val x)  = return x
traceSamples  (Op u k) = case u of
    SampPatt d α ->  Op (weaken u) (\x -> do updateSTrace α x
                                             traceSamples (k x))
    _   -> Op (weaken u) (traceSamples . k)

-- When discharging Observe, return the rest of the program, and the log probability
breakObserve :: Member Observe es => Prog es a -> Prog es (Prog es a, Double)
breakObserve  (Val x) = return (Val x, 0)
breakObserve  (Op op k) = case op of
      ObsPatt d y α -> do
        let logp = logProb d y
        -- prinT $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show logp
        Val (k y, logp)
      _ -> Op op (breakObserve . k)

-- When discharging Observe, return the rest of the program, and the log probability
runObserve :: Member Sample es => Prog (Observe : es) a -> Prog es (Prog (Observe : es) a, Double)
runObserve  (Val x) = return (Val x, 0)
runObserve  (Op op k) = case op of
      ObsPatt d y α -> do
        let logp = logProb d y
        -- prinT $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show logp
        Val (k y, logp)
      Other op -> Op op (runObserve . k)