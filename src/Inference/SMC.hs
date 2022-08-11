{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

{- Sequential Monte Carlo inference.
-}

module Inference.SMC where

import Control.Monad ( replicateM )
import Effects.Dist ( pattern ObsPrj, handleDist, Addr, Dist, Observe, Sample )
import Effects.Lift ( Lift, lift )
import Effects.NonDet ( asum, handleNonDet, NonDet )
import Effects.ObsRW
import Env ( Env )
import LogP ( LogP(..), logMeanExp )
import Model ( Model(runModel) )
import OpenSum (OpenSum)
import PrimDist ( PrimDist(Categorical), sample, logProb )
import Prog ( LastMember, Prog(..), Members, Member, call, weakenProg, discharge )
import qualified Data.Map as Map
import qualified Inference.SIM as SIM
import qualified Inference.SIS as SIS
import Inference.SIS (Resample(..), ParticleResampler, ParticleRunner)
import Sampler ( Sampler )

{- | The context of a particle for SMC.
-}
data SMCParticle = SMCParticle {
    particleLogProb  :: LogP    -- ^ associated log-probability
  , particleObsAddrs :: [Addr]  -- ^ addresses of @Observe@ operations encountered so far
  }

instance SIS.ParticleCtx SMCParticle where
  pempty            = SMCParticle 0 []
  paccum ctxs ctxs' =
    -- | Compute normalised accumulated log weights
    let logprobs = let logZ = logMeanExp (map particleLogProb ctxs)
                   in  map ((+ logZ) . particleLogProb) ctxs'
    -- | Update the Observe operations encountered
        obsaddrs = zipWith (++) (map particleObsAddrs ctxs') (map particleObsAddrs ctxs)
    in  zipWith SMCParticle logprobs obsaddrs

{- | Call SMC on a model.
-}
smc
  :: Int                                              -- ^ number of particles
  -> Model env [ObsRW env, Dist, Lift Sampler] a      -- ^ model
  -> Env env                                          -- ^ input model environment
  -> Sampler [Env env]                                -- ^ output model environments of each particle
smc n_particles model env = do
  let prog = (handleDist . handleObsRW env) (runModel model)
  final_ctxs <- smcInternal n_particles prog
  pure $ map (snd . fst) final_ctxs

{- | Call SMC on a probabilistic program.
-}
smcInternal
  :: Int                                              -- ^ number of particles
  -> Prog [Observe, Sample, Lift Sampler] a           -- ^ probabilistic program
  -> Sampler [(a, SMCParticle)]                       -- ^ final particle results and contexts
smcInternal n_particles prog =
  SIS.sis n_particles particleRunner particleResampler (weakenProg @(Resample SMCParticle) prog)

{- | A handler for resampling particles according to their normalized log-likelihoods.
-}
particleResampler :: ParticleResampler SMCParticle
particleResampler (Val x) = Val x
particleResampler (Op op k) = case discharge op of
  Left  op'        -> Op op' (particleResampler  . k)
  Right (Resample (prts, ctxs)) -> do
    -- | Get the normalised log-weight for each particle
    let logws = map (exp . unLogP . particleLogProb) ctxs
    -- | Select particles to continue with
    idxs <- replicateM (length ctxs) $ lift (sample (Categorical logws))
    let resampled_prts = map (prts !! ) idxs
        resampled_ctxs = map (ctxs !! ) idxs
    (particleResampler . k) ((resampled_prts, resampled_ctxs), idxs)

{- | A handler that invokes a breakpoint upon matching against the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation + the address of the breakpoint
-}
particleRunner :: ParticleRunner SMCParticle
particleRunner (Val x) = pure (Val x, SMCParticle 0 [("", 0)])
particleRunner (Op op k) = case op of
      ObsPrj d y α -> Val (k y, SMCParticle (logProb d y) [α])
      _            -> Op op (particleRunner . k)

