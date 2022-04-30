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
import qualified Inference.SMC as SMC
import Inference.SIS
    ( sis, Accum(accum), ParticleHandler, Resampler, LogP(..), logMeanExp )
import OpenSum (OpenSum)
import Util

rmsmcToplevel :: forall env es' a. (FromSTrace env, Show a) =>
  (es' ~ [ObsReader env, Dist, Lift Sampler]) =>
  Int -> Int -> Model env es' a -> ModelEnv env -> Sampler [(a, LogP, ModelEnv env)]
rmsmcToplevel n_particles mh_steps model env = do
  let model_0 = (runDist . runObsReader env) (runModel model)
  rmsmc n_particles mh_steps model_0 env

rmsmc :: forall env es' a. (FromSTrace env, Show a) =>
  (es' ~ [Observe, Sample, Lift Sampler]) =>
  Int -> Int -> Prog es' a -> ModelEnv env -> Sampler [(a, LogP, ModelEnv env)]
rmsmc n_particles mh_steps model env = do
  as_ps_straces <- sis n_particles (rmsmcResampler mh_steps model) SMC.smcPopulationHandler SMC.runObserve SMC.runSample model
  return $ map (\(a, (addr, p, strace)) -> (a, p, fromSDTrace @env strace)) as_ps_straces

rmsmcResampler :: forall es a.
     Int
  -> Prog [Observe, Sample, Lift Sampler] a -- the initial program, representing the entire unevaluated model execution (having already provided a model environment)
  -> Resampler ([Addr], LogP, SDTrace) [Observe, Sample, Lift Sampler] a
rmsmcResampler mh_steps model ctx_0 ctx_1sub0 progs_1 = do
  -- run SMC resampling
  (obs_addrs, _, resampled_straces) <- unzip3 . snd <$> SMC.smcResampler ctx_0 ctx_1sub0 progs_1
  let -- get most recent observe address
      α_break       = (head . head) obs_addrs
      -- insert break point to perform MH up to
      partial_model = insertBreakpoint α_break model

  -- perform metropolis-hastings using each resampled particle's sample trace
  mhTraces <- lift $ mapM (\sdtrace -> mhWithSTrace mh_steps partial_model sdtrace []) resampled_straces
  let -- get the continuations of each particle from the break point, and weaken with non-det effect
      moved_particles     = map (weakenNonDet . fst3 . last) mhTraces
      -- get the sample traces of each particle up until the break point
      moved_straces = map (snd3 . last) mhTraces
      -- get the log prob traces of each particle up until the break point
      lptraces     = map (thrd3 . last) mhTraces
      -- filter log probability traces to only include that for observe operations
      obs_lptraces = map (Map.filterWithKey (\k a -> k `elem` head obs_addrs)) lptraces
      -- compute total log probability of each particle up until break point
      moved_logWs  = map (LogP . sum . map snd . Map.toList) obs_lptraces

  return (moved_particles, zip3 obs_addrs moved_logWs moved_straces)

insertBreakpoint :: Members [Observe, Sample] es =>
  Addr -> Prog es a -> Prog es (Prog es a)
insertBreakpoint α_break (Val x) = return (Val x)
insertBreakpoint α_break (Op op k) = case op of
      ObsPatt d y α -> do
        if α_break == α
          then Val (k y)
          else Op op (insertBreakpoint α_break . k)
      _ -> Op op (insertBreakpoint α_break . k)

