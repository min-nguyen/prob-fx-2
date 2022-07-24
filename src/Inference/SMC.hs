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

import qualified Data.Map as Map
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

{- | A top-level function for calling Sequential Monte Carlo on a model.
-}
smc :: (FromSTrace env, Show a)
  => Int
  -> Model env [ObsReader env, Dist, Lift Sampler] a
  -> Env env
  -> Sampler [((a, LogP), Env env)]
smc n_particles model env = do
  let prog = (handleDist . handleObsRead env) (runModel model)
  smcInternal n_particles prog env

{- | For calling Sequential Monte Carlo on a probabilistic program.
-}
smcInternal :: (FromSTrace env, Show a)
  => Int
  -> Prog [Observe, Sample, Lift Sampler] a
  -> Env env
  -> Sampler [((a, LogP), Env env)]
smcInternal n_particles prog env = do
  final_ctxs <- SIS.sis n_particles smcParticleHdlr smcResampler  SIM.handleObs SIM.handleSamp prog
  pure $ map (\(a, SIS.ParticleCtx logp strace _) -> ((a, logp), fromSTrace strace)) final_ctxs

{- | Runs a population of particles to the next breakpoint in the program.
-}
smcParticleHdlr :: Members [Observe, Sample, Lift Sampler] es
  => SIS.ParticleHandler es a
smcParticleHdlr particles = do
  {- Merge particles into single non-deterministic program using 'asum' and run the program to the next breakpoint.
     This returns a list containing:
      1. particles that can be resumed
      2. the address of the @Observe@ breakpoint
      3. the incremental sample trace since the previous breakpoint
  -}
  particles'_ctxs' <- (handleNonDet . traceSamples . breakObserve ) (asum particles)
  {- Reformat the data into a list of particles and particle contexts.
  -}
  mapM (\((prog, logp, α), strace) -> pure (prog, SIS.ParticleCtx logp strace [α])) particles'_ctxs'

{- | Resamples a population of particles according to their normalised log-weights.
-}
smcResampler :: LastMember (Lift Sampler) es
  => SIS.ParticleResampler es a
smcResampler ctxs_0 ctxs_1sub0 particles = do
  -- Accumulate the contexts of all particles and get their normalised log-weights
  let ctxs       = SIS.accumParticleCtx ctxs_0 ctxs_1sub0
      logws      = map (exp . unLogP . SIS.particleLogProb) ctxs
  -- Select particles to continue with
  particle_idxs :: [Int] <- replicateM (length particles) $ lift (sample (Categorical logws))
  let resampled_particles = map (zip particles ctxs !! ) particle_idxs
  pure resampled_particles

{- | A handler that invokes a breakpoint upon matching against @Observe@, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation
       3. the address of the breakpoint
-}
breakObserve :: Members [Lift Sampler, Observe] es
  => Prog es a
  -> Prog es (Prog es a, LogP, Addr)
breakObserve  (Val x) = pure (Val x, 0, ("", 0))
breakObserve  (Op op k) = case op of
      ObsPrj d y α -> Val (k y, logProb d y, α)
      _            -> Op op (breakObserve . k)