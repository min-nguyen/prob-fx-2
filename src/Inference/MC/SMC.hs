

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
import           Effects.Lift ( liftPrint, handleM)
import           Effects.EnvRW ( EnvRW, handleEnvRW )
import           Env ( Env )
import           LogP ( LogP(..), logMeanExp )
import           Model ( GenModel(runModel), Model )
import           PrimDist ( mkCategorical, drawWithSampler, logProb )
import           Prog ( LastMember, Prog(..), Members, Member, call, weakenProg, discharge, prj, handle, Handler )
import qualified Data.Map as Map
import           Inference.MC.SIM as SIM
import qualified Inference.MC.SIS as SIS
import           Inference.MC.SIS (Resample(..), ParticleHandler, pfilter)
import           Sampler ( Sampler, sampleRandom, sampleCategorical)

{- | Call SMC on a model.
-}
smc
  :: Int                                -- ^ number of particles
  -> GenModel env [EnvRW env, Dist, Sampler] a      -- ^ model
  -> Env env                            -- ^ input model environment
  -> Sampler [Env env]                  -- ^ output model environments of each particle
smc n_prts model env_in = do
  -- | Handle model to probabilistic program
  let prog_0 = (handleDist . handleEnvRW env_in) (runModel model)
  smc_trace <- mulpfilter n_prts prog_0
  pure (map (snd . fst) smc_trace)

{- | Call SMC on a probabilistic program.
-}
mulpfilter :: Int -> Model '[Sampler] a -> Sampler [(a, LogP)]
mulpfilter n_prts model =
 (handleM . handleResampleMul . pfilter handleParticle) prts
 where prts = replicate n_prts (model, 0)

{- | A handler that invokes a breakpoint upon matching against the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation
-}
handleParticle :: Model '[Sampler] a -> LogP -> Sampler (Model '[Sampler] a, LogP)
handleParticle model logp = (handleM . defaultSample . suspend logp) model

suspend :: LogP -> Handler Observe es a (Prog (Observe : es) a, LogP)
suspend logp (Val x)   = Val (Val x, logp)
suspend logp (Op op k) = case discharge op of
  Right (Observe d y α) -> Val (k y, logp + logProb d y)
  Left op'              -> Op op' (suspend logp . k)

{- | A handler for multinomial resampling of particles.
-}

handleResampleMul :: Member Sampler es => Handler (Resample LogP) es b b
handleResampleMul = handle () (const Val) (const hop) where
  hop :: Member Sampler es =>  Resample LogP x -> (() -> x -> Prog es b) -> Prog es b
  hop  (Resample (prts, ρs)) k = do
    let n = length ρs
    idxs <- call (replicateM n (Sampler.sampleCategorical (Vector.fromList (map exp ρs))))
    let prts_res  = map (prts !! ) idxs
        ρs_res    = (replicate n . logMeanExp . map (ρs  !! )) idxs

    k () (prts_res, ρs_res)

resampleMul :: [LogP] -> Sampler [Int]
resampleMul ρs = do
  let ps = map exp ρs
  -- | Select particles to continue with
  replicateM (length ps) (Sampler.sampleCategorical (Vector.fromList ps))

{- | A handler for systematic resampling of particles.
-}
handleResampleSys :: Member Sampler fs => Handler (Resample LogP) fs a a
handleResampleSys (Val x) = Val x
handleResampleSys (Op op k) = case discharge op of
  Right (Resample (prts, ρs)) -> do
    -- | Get the weights for each particle
    let ps = map exp ρs
    -- | Select particles to continue with
    u <- call Sampler.sampleRandom
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
        ρs_mean   = map (const (logMeanExp ρs_res)) ρs_res

    (handleResampleSys . k) (prts_res, ρs_mean)
  Left op' -> Op op' (handleResampleSys . k)
