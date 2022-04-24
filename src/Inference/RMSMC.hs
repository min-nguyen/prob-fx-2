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

module Inference.RMSMC where
-- import Data.Extensible hiding (Member)
import qualified Data.Map as Map
import Data.Maybe
import Data.Bifunctor
import Data.Map (Map)
import Debug.Trace
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
import Inference.MH
import Inference.SIS
    ( sis, Accum(accum), ParticleHandler, Resampler, logMeanExp )
import OpenSum (OpenSum)
import Util

rmsmc :: forall env es' a. (FromSTrace env, Show a) =>
  (es' ~ [ObsReader env, Dist, Lift Sampler]) =>
  Int -> Int -> Model env es' a -> ModelEnv env -> Sampler [(a, Double, ModelEnv env)]
rmsmc n_particles mh_steps model env = do
  let model_0 = (runDist . runObsReader env) (runModel model)
  as_ps_straces <- sis n_particles (rmsmcResampler mh_steps model_0) rmsmcPopulationHandler model env
  return $ map (\(a, (addr, p, strace)) -> (a, p, fromSDTrace @env strace)) as_ps_straces

rmsmcPopulationHandler :: Members [Observe, Sample] es
  => ParticleHandler  ([Addr], Double, SDTrace) es a
rmsmcPopulationHandler progs = do
  -- Merge particles into single non-deterministic program using 'asum', and run to next checkpoint
  progs_ctxs <- (runNonDet . runState Map.empty . traceSamples . breakObserves ) (asum progs)
  -- List of particles that can be resumed, their observe breakpoint address, the log probability at that break point, and an accumulated sample trace
  let progs_ctxs' = map (\((prog, α, p), strace) -> (prog, ([α], p,  strace))) progs_ctxs
  return progs_ctxs'

rmsmcResampler :: forall es a.
     Int
  -> Prog [Observe, Sample, Lift Sampler] a -- the initial program, representing the entire unevaluated model execution (having already provided a model environment)
  -> Resampler ([Addr], Double, SDTrace) [Observe, Sample, Lift Sampler] a
rmsmcResampler mh_steps model_0 ctx_0 ctx_1sub0 progs_1 = do
  let
      (obs_addrs_0,     logWs_0    , straces_0)         = unzip3 ctx_0
      -- Get log weights and sample traces since previous particle run
      (obs_addrs_1sub0, logWs_1sub0, straces_1sub0) = unzip3 ctx_1sub0
      -- accumulate observe addresses
      obs_addrs_1 = zipWith accum obs_addrs_1sub0 obs_addrs_0
      -- accumulate log probabilities
      logWs_1 = map (+ logMeanExp logWs_0) logWs_1sub0
      -- for each particle, merge their incremental sample trace with their sample trace up until the previous break point
      straces_1 = zipWith Map.union straces_1sub0 straces_0
      n_particles = length progs_1
  prinT $ "LogWs " ++ show logWs_1
  prinT $ "Resampling probabilities " ++ show (map exp logWs_1)

  -- select particles to continue with
  particle_idxs :: [Int] <- replicateM n_particles $ send (Sample (DiscreteDist (map exp logWs_1) Nothing Nothing) undefined)
  prinT $ "particle idx " ++ show particle_idxs
  let -- select sample traces to continue with
      resampled_straces = map (straces_1 !!) particle_idxs
      -- get most recent observe address
      α_break       = (head . head) obs_addrs_1
      -- insert break point to perform MH up to
      partial_model = insertBreakpoint α_break model_0

  -- perform metropolis-hastings using each resampled particle's sample trace
  mhTraces <- lift $ mapM (\sdtrace -> mhWithSTrace mh_steps partial_model sdtrace []) resampled_straces
  let -- get the continuations of each particle from the break point, and weaken with non-det effect
      moved_particles     = map (weakenNonDet . fst3 . last) mhTraces
      -- get the sample traces of each particle up until the break point
      moved_straces = map (snd3 . last) mhTraces
      -- get the log prob traces of each particle up until the break point
      lptraces     = map (thrd3 . last) mhTraces
      -- filter log probability traces to only include that for observe operations
      obs_lptraces = map (Map.filterWithKey (\k a -> k `elem` head obs_addrs_1)) lptraces
      -- compute total log probability of each particle up until break point
      moved_logWs  = map (sum . map snd . Map.toList) obs_lptraces

  return (moved_particles, zip3 obs_addrs_1 moved_logWs moved_straces)

traceSamples :: (Member Sample es) => Prog es a -> Prog (State SDTrace : es) a
traceSamples  (Val x)  = return x
traceSamples  (Op u k) = case u of
    SampPatt d α ->  Op (weaken u) (\x -> do updateSDTrace α d x
                                             traceSamples (k x))
    _   -> Op (weaken u) (traceSamples . k)

breakObserves :: Member Observe es => Prog es a -> Prog es (Prog es a, Addr, Double)
breakObserves  (Val x) = return (Val x, ("", 0), 0)
breakObserves  (Op op k) = case op of
      ObsPatt d y α -> do
        let logp = logProb d y
        -- prinT $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show logp
        Val (k y, α, logp)
      _ -> Op op (breakObserves . k)

insertBreakpoint :: Members [Observe, Sample] es =>
  Addr -> Prog es a -> Prog es (Prog es a)
insertBreakpoint α_break (Val x) = return (Val x)
insertBreakpoint α_break (Op op k) = case op of
      ObsPatt d y α -> do
        if α_break == α
          then Val (k y)
          else Op op (insertBreakpoint α_break . k)
      _ -> Op op (insertBreakpoint α_break . k)

