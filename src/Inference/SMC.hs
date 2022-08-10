{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MonoLocalBinds #-}

module Inference.SMC where

import Control.Monad ( replicateM )
import Effects.Dist ( pattern ObsPrj, handleDist, Addr, Dist, Observe, Sample )
import Effects.Lift ( Lift, lift )
import Effects.NonDet ( asum, handleNonDet )
import Effects.ObsRW
import Env ( Env )
import LogP ( LogP(..), logMeanExp )
import Model ( Model(runModel) )
import OpenSum (OpenSum)
import PrimDist ( PrimDist(Categorical), sample, logProb )
import Prog ( LastMember, Prog(..), Members, Member )
import qualified Data.Map as Map
import qualified Inference.SIM as SIM
import qualified Inference.SIS as SIS
import Sampler ( Sampler )

{- | The particle context for SMC
-}
data SMCParticle = SMCParticle {
    particleLogProb  :: LogP    -- ^ associated log-probability
  , particleObsAddrs :: [Addr]  -- ^ addresses of @Observe@ operations encountered so far
  }

instance SIS.ParticleCtx SMCParticle where
  pempty            = SMCParticle 0 []
  paccum ctxs ctxs' =
    --  Compute normalised accumulated log weights
    let logprobs = let logZ = logMeanExp (map particleLogProb ctxs)
                   in  map ((+ logZ) . particleLogProb) ctxs'
    --  Update the Observe operations encountered
        obsaddrs = zipWith (++) (map particleObsAddrs ctxs') (map particleObsAddrs ctxs)
    in  zipWith SMCParticle logprobs obsaddrs

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
  -> Sampler [(a, SMCParticle)]
smcInternal n_particles prog env =
  SIS.sis n_particles smcParticleHdlr smcResampler SIM.handleObs SIM.handleSamp prog

{- | Runs a population of particles to the next breakpoint in the program.
-}
smcParticleHdlr :: Members [Observe, Sample, Lift Sampler] es
  => SIS.ParticleHandler SMCParticle es a
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
  mapM (\(prog, logp, α) -> pure (prog, SMCParticle logp [α])) particles'_ctxs'

{- | Resamples a population of particles according to their normalised log-weights.
-}
smcResampler :: LastMember (Lift Sampler) es
  => SIS.ParticleResampler SMCParticle es a
smcResampler ctxs_0 ctxs_1sub0 particles = do
  -- Accumulate the contexts of all particles and get their normalised log-weights
  let ctxs       = SIS.paccum ctxs_0 ctxs_1sub0
      logws      = map (exp . unLogP . particleLogProb) ctxs
  -- Select particles to continue with
  particle_idxs :: [Int] <- replicateM (length particles) $ lift (sample (Categorical logws))
  let resampled_particles = map (zip particles ctxs !! ) particle_idxs
  pure resampled_particles

{- | A handler that invokes a breakpoint upon matching against the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation
       3. the address of the breakpoint
-}
breakObserve :: Member Observe es
  => Prog es a
  -> Prog es (Prog es a, LogP, Addr)
breakObserve  (Val x) = pure (Val x, 0, ("", 0))
breakObserve  (Op op k) = case op of
      ObsPrj d y α -> Val (k y, logProb d y, α)
      _            -> Op op (breakObserve . k)