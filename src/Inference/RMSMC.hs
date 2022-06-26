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

import qualified Data.Map as Map
import Data.Map (Map)
import Env
import Prog
import Model
import Sampler
import Trace
import LogP
import Control.Monad
import Control.Applicative
import Effects.Dist
import Effects.Lift
import Effects.ObsReader
import Effects.NonDet
import qualified Inference.MH as MH
import qualified Inference.SMC as SMC
import qualified Inference.SIM as SIM
import qualified Inference.SIS as SIS
import OpenSum (OpenSum)
import Util

rmsmcToplevel :: forall env es' a. (FromSTrace env, Show a) =>
  (es' ~ [ObsReader env, Dist, Lift Sampler]) =>
  Int -> Int -> Model env es' a -> Env env -> Sampler [(a, LogP, Env env)]
rmsmcToplevel n_particles mh_steps model env = do
  let prog = (handleDist . handleObsRead env) (runModel model)
  rmsmc n_particles mh_steps prog env

rmsmc :: forall env es' a. (FromSTrace env, Show a) =>
  (es' ~ [Observe, Sample, Lift Sampler]) =>
  Int -> Int -> Prog es' a -> Env env -> Sampler [(a, LogP, Env env)]
rmsmc n_particles mh_steps prog env = do
  as_ps_straces <- SIS.sis n_particles (rmsmcResampler mh_steps prog) SMC.smcPopulationHandler SIM.handleObs SIM.handleSamp prog
  pure $ map (\(a, (addr, p, strace)) -> (a, p, fromSTrace @env strace)) as_ps_straces

rmsmcResampler :: forall es a.
     Int
  -> Prog [Observe, Sample, Lift Sampler] a -- the initial program, representing the entire unevaluated model execution (having already provided a model environment)
  -> SIS.Resampler ([Addr], LogP, STrace) [Observe, Sample, Lift Sampler] a
rmsmcResampler mh_steps prog ctx_0 ctx_1sub0 progs_1 = do
  -- run SMC resampling, ignore log weights of particles
  (obs_addrs, _, resampled_straces) <- unzip3 . snd <$> SMC.smcResampler ctx_0 ctx_1sub0 progs_1
  let -- get most recent observe address
      α_break       = (head . head) obs_addrs
      -- insert break point to perform MH up to
      partial_model = insertBreakpoint α_break prog

  -- perform metropolis-hastings using each resampled particle's sample trace
  mhTraces <- lift $ mapM (\strace -> MH.mh mh_steps partial_model strace []) resampled_straces
  let -- get the continuations of each particle from the break point, and weaken with non-det effect
      moved_particles = map (weakenNonDet . fst3 . head) mhTraces
      -- get the sample traces of each particle up until the break point
      moved_straces   = map (snd3 . head) mhTraces
      -- get the log prob traces of each particle up until the break point
      lptraces        = map (thrd3 . head) mhTraces
      -- filter log probability traces to only include that for observe operations
      obs_lptraces    = map (Map.filterWithKey (\k a -> k `elem` head obs_addrs)) lptraces
      -- compute log weights of particles, that is, the total log probability of each particle up until the break point
      moved_logWs     = map (LogP . sum . map snd . Map.toList) obs_lptraces

  pure (moved_particles, zip3 obs_addrs moved_logWs moved_straces)

insertBreakpoint :: Members [Observe, Sample] es =>
  Addr -> Prog es a -> Prog es (Prog es a)
insertBreakpoint α_break (Val x) = pure (Val x)
insertBreakpoint α_break (Op op k) = case op of
      ObsPatt d y α -> do
        if α_break == α
          then Val (k y)
          else Op op (insertBreakpoint α_break . k)
      _ -> Op op (insertBreakpoint α_break . k)

