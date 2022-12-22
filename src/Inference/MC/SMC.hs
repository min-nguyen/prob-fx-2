{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}

{- Sequential Monte Carlo inference.
-}

module Inference.MC.SMC where

import           Control.Monad ( replicateM )
import qualified Data.Vector as Vector
import           Effects.Dist ( pattern ObsPrj, handleDist, Addr, Dist, Observe (..), Sample )
import           Effects.Lift ( Lift, lift, liftPrint, handleLift, HasSampler)
import           Effects.ObsRW ( ObsRW, handleObsRW )
import           Env ( Env )
import           LogP ( LogP(..), logMeanExp )
import           Model ( Model(runModel), ProbProg )
import           PrimDist ( mkCategorical, sample, logProb )
import           Prog ( LastMember, Prog(..), Members, Member, call, weakenProg, discharge, prj )
import qualified Data.Map as Map
import qualified Inference.MC.SIM as SIM
import qualified Inference.MC.SIS as SIS
import           Inference.MC.SIS (Resample(..), ResampleHandler, ParticleHandler)
import           Sampler ( Sampler, sampleRandom, sampleCategorical)

{- | Call SMC on a model.
-}
smc
  :: Int                                -- ^ number of particles
  -> Model env [ObsRW env, Dist] a      -- ^ model
  -> Env env                            -- ^ input model environment
  -> Sampler [Env env]                  -- ^ output model environments of each particle
smc n_prts model env_in = do
  -- | Handle model to probabilistic program
  let prog_0 = (handleDist . handleObsRW env_in) (runModel model)
  smc_trace <- handleLift (smcInternal n_prts prog_0)
  pure (map (snd . fst) smc_trace)

{- | Call SMC on a probabilistic program.
-}
smcInternal :: HasSampler fs
  => Int                       -- ^ number of particles
  -> ProbProg a                 -- ^ probabilistic program
  -> Prog fs [(a, LogP)]   -- ^ final particle results and contexts
smcInternal n_prts  =
  handleResampleMul . SIS.sis n_prts handleParticle 0

{- | A handler that invokes a breakpoint upon matching against the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation
-}
handleParticle :: ProbProg a -> Sampler (ProbProg a, LogP)
handleParticle = SIM.handleSamp . handleObs

handleObs :: Prog (Observe : es) a -> Prog es (Prog (Observe : es) a, LogP)
handleObs (Val x)   = Val (Val x, 0)
handleObs (Op op k) = case discharge op of
  Right (Observe d y α) -> Val (k y, logProb d y)
  Left op'              -> Op op' (handleObs . k)

{- | A handler for multinomial resampling of particles.
-}
handleResampleMul :: HasSampler fs => ResampleHandler fs LogP
handleResampleMul (Val x) = Val x
handleResampleMul (Op op k) = case discharge op of
  Right (Resample (prts, lρs) _) -> do
    -- | Select particles to continue with
    idxs <- lift (resampleMul lρs)
    let resampled_prts  = map (prts !! ) idxs
        resampled_logws = map (lρs !! ) idxs
    (handleResampleMul . k) (resampled_prts, resampled_logws)
  Right (Accum ss ss') -> do
    (handleResampleMul . k) (normaliseParticles ss ss')
  Left op' -> Op op' (handleResampleMul . k)

normaliseParticles  :: [LogP] -> [LogP] -> [LogP]
normaliseParticles log_ps log_ps' =
  let logZ = logMeanExp log_ps in  map (+ logZ) log_ps'

resampleMul :: [LogP] -> Sampler [Int]
resampleMul lρs = do
  let ps = map exp lρs
  -- | Select particles to continue with
  replicateM (length ps) (Sampler.sampleCategorical (Vector.fromList ps))

{- | A handler for systematic resampling of particles.
-}
handleResampleSys :: HasSampler fs => ResampleHandler fs LogP
handleResampleSys (Val x) = Val x
handleResampleSys (Op op k) = case discharge op of
  Right (Resample (prts, lρs) _) -> do
    -- | Get the weights for each particle
    let ps = map exp lρs
    -- | Select particles to continue with
    u <- lift Sampler.sampleRandom
    let prob i = ps !! i
        n      = length ps
        inc = 1 / fromIntegral n
        f i _ _ _ acc | i == n = acc
        f i v j q acc =
          if v < q
            then f (i + 1) (v + inc) j q (j - 1 : acc)
            else f i v (j + 1) (q + prob j) acc
        idxs = f 0 (u / fromIntegral n) 0 0 []
        resampled_prts = map (prts !! ) idxs
        resampled_ss = map (lρs !! ) idxs

    (handleResampleSys . k) (resampled_prts, resampled_ss)
  Right (Accum ss ss') -> do
    (handleResampleMul . k) (normaliseParticles ss ss')
  Left op' -> Op op' (handleResampleSys . k)
