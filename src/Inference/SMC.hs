{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

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
import Inference.SIS (Resample(..), ParticleResampler, ParticleRunner, ParticleCtx)
import Sampler

{- | The context of a particle for SMC.
-}
newtype SMCParticle = SMCParticle {
    particleLogProb  :: LogP    -- ^ associated log-probability
  } deriving Num

instance ParticleCtx SMCParticle where
  pempty            = SMCParticle 0
  paccum ctxs ctxs' =
    -- | Compute normalised accumulated log weights
    let logprobs = let logZ = logMeanExp (map particleLogProb ctxs)
                   in  map ((+ logZ) . particleLogProb) ctxs'
    in  map SMCParticle logprobs
    -- -- | Update the Observe operations encountered
    --     obsaddrs = zipWith (++) (map particleObsAddrs ctxs') (map particleObsAddrs ctxs)
    -- in  zipWith SMCParticle logprobs obsaddrs

{- | Call SMC on a model.
-}
smc
  :: Int                                              -- ^ number of particles
  -> Model env [ObsRW env, Dist, Lift Sampler] a      -- ^ model
  -> Env env                                          -- ^ input model environment
  -> Sampler [Env env]                                -- ^ output model environments of each particle
smc n_particles model env = do
  let prog = (handleDist . handleObsRW env) (runModel model)
  smcInternal n_particles prog >>= pure . map (snd . fst)

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
  Right (Resample (prts, ctxs)) -> do
    lift $ liftIO $ print "hi-1.5"
    -- | Get the normalised log-weight for each particle
    let logws = map (exp . unLogP . particleLogProb) ctxs
    lift $ liftIO $ print (length ctxs)
    -- | Select particles to continue with
    idxs <- replicateM (length ctxs) $ lift (sample (Categorical logws))
    lift $ liftIO $ print "hi-1.7"
    let resampled_prts = map (prts !! ) idxs
        resampled_ctxs = map (ctxs !! ) idxs

    lift $ liftIO $ print "hi-1.8"
    (particleResampler . k) ((resampled_prts, resampled_ctxs), idxs)
  Left op' -> Op op' (particleResampler . k)

{- | A handler that invokes a breakpoint upon matching against the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation + the address of the breakpoint
-}
particleRunner :: ParticleRunner SMCParticle
particleRunner (Val x) = pure (Val x, SMCParticle 0)
particleRunner (Op op k) = case op of
      ObsPrj d y α -> Val (k y, SMCParticle (logProb d y))
      _            -> Op op (particleRunner . k)

