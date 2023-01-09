

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE InstanceSigs #-}


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
import           Inference.MC.SIM as SIM
import qualified Inference.MC.SIS as SIS
import           Inference.MC.SIS (Resample(..), ResampleHandler, ParticleHandler, pfilter)
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
  smc_trace <- mulpfilter n_prts prog_0
  pure (map (snd . fst) smc_trace)

{- | Call SMC on a probabilistic program.
-}
mulpfilter :: Int -> ProbProg a -> Sampler [(a, LogP)]
mulpfilter n_prts model =
 (handleLift . handleResampleMul . pfilter handleParticle model) (prts, ρs)
 where (prts, ρs) = (unzip . replicate n_prts) (model, 0)

{- | A handler that invokes a breakpoint upon matching against the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation
-}
handleParticle :: ProbProg a -> Sampler (ProbProg a, LogP)
handleParticle = defaultSample . suspend

suspend :: ProbProg a -> Prog '[Sample] (ProbProg a, LogP)
suspend (Val x)   = Val (Val x, 0)
suspend (Op op k) = case discharge op of
  Right (Observe d y α) -> Val (k y, logProb d y)
  Left op'              -> Op op' (suspend . k)

{- | A handler for multinomial resampling of particles.
-}
handleResampleMul :: HasSampler fs => ResampleHandler fs LogP
handleResampleMul (Val x) = Val x
handleResampleMul (Op op k) = case discharge op of
  Right (Resample (prts, ρs) _) -> do
    -- | Select particles to continue with
    idxs <- lift (replicateM (length ρs) (Sampler.sampleCategorical (Vector.fromList (map exp ρs))))
    let prts_res  = map (prts !! ) idxs
        ρs_res    = map (ρs  !! ) idxs
    (handleResampleMul . k) (prts_res, ρs_res)
  Right (Accum ρs1 ρs2) -> do
    let ρs = map (+ logMeanExp ρs1) ρs2
    (handleResampleMul . k) ρs
  Left op' -> Op op' (handleResampleMul . k)

normaliseParticles  :: [LogP] -> [LogP] -> [LogP]
normaliseParticles ρs ρs' =
  let logZ = logMeanExp ρs in  map (+ logZ) ρs'

resampleMul :: [LogP] -> Sampler [Int]
resampleMul ρs = do
  let ps = map exp ρs
  -- | Select particles to continue with
  replicateM (length ps) (Sampler.sampleCategorical (Vector.fromList ps))

{- | A handler for systematic resampling of particles.
-}
handleResampleSys :: HasSampler fs => ResampleHandler fs LogP
handleResampleSys (Val x) = Val x
handleResampleSys (Op op k) = case discharge op of
  Right (Resample (prts, ρs) _) -> do
    -- | Get the weights for each particle
    let ps = map exp ρs
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
        idxs     = f 0 (u / fromIntegral n) 0 0 []
        prts_res = map (prts !! ) idxs
        ρs_res   = map (ρs !! ) idxs

    (handleResampleSys . k) (prts_res, ρs_res)
  Right (Accum ss ss') -> do
    (handleResampleMul . k) (normaliseParticles ss ss')
  Left op' -> Op op' (handleResampleSys . k)
