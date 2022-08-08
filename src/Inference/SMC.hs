{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module Inference.SMC where

import Control.Monad ( replicateM )
import Effects.Dist ( pattern ObsPrj, handleDist, Addr, Dist, Observe, Sample )
import Effects.Lift ( Lift, lift )
import Effects.NonDet ( asum, handleNonDet )
import Effects.ObsRW
import Env ( Env )
import LogP ( LogP(..) )
import Model ( Model(runModel) )
import OpenSum (OpenSum)
import PrimDist ( PrimDist(Categorical), sample, logProb )
import Prog ( LastMember, Prog(..), Members )
import qualified Data.Map as Map
import qualified Inference.SIM as SIM
import qualified Inference.SIS as SIS
import Sampler ( Sampler )

{- | A top-level function for calling Sequential Monte Carlo on a model.
-}
smc :: Show a
  => Int
  -> Model env [ObsRW env, Dist, Lift Sampler] a
  -> Env env
  -> Sampler [Env env]
smc n_particles model env = do
  let prog = (handleDist . handleObsRW env) (runModel model)
  final_ctxs <- smcInternal n_particles prog env
  pure $ map (snd . fst) final_ctxs

{- | For calling Sequential Monte Carlo on a probabilistic program.
-}
smcInternal
  :: Int
  -> Prog [Observe, Sample, Lift Sampler] a
  -> Env env
  -> Sampler [(a, SIS.ParticleCtx)]
smcInternal n_particles prog env =
  SIS.sis n_particles smcParticleHdlr smcResampler SIM.handleObs SIM.handleSamp prog

{- | Runs a population of particles to the next breakpoint in the program.
-}
smcParticleHdlr :: Members [Observe, Sample, Lift Sampler] es
  => SIS.ParticleHandler es a
smcParticleHdlr particles = do
  {- Merge particles into single non-deterministic program using 'asum' and run the program to the next breakpoint.
     This returns a list containing:
      1. the particles that can be resumed
      2. the address of their @Observe@ breakpoint
      3. their incremental sample trace since the previous breakpoint
  -}
  particles'_ctxs' <- (handleNonDet . breakObserve) (asum particles)
  {- Reformat the data into a list of particles and particle contexts.
  -}
  mapM (\(prog, logp, α) -> pure (prog, SIS.ParticleCtx logp [α])) particles'_ctxs'

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