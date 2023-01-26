

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
import           Effects.EnvRW ( EnvRW, handleEnvRW )
import           Env ( Env )
import           LogP ( LogP(..), logMeanExp )
import           Model ( GenModel(runModel), Model )
import           PrimDist ( mkCategorical, drawWithSampler, logProb )
import           Comp ( LastMember, Comp(..), Members, Member, call, weakenProg, discharge, prj, handle, Handler )
import qualified Data.Map as Map
import           Inference.MC.SIM as SIM
import qualified Inference.MC.SIS as SIS
import           Inference.MC.SIS (Resample(..), ParticleHandler, pfilter)
import           Sampler ( Sampler, random, sampleCategorical, handleIO)

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
mulpfilter n_prts model = (handleIO . handleResampleMul . pfilter handleParticle) wprts
 where wprts = replicate n_prts (model, 0)

{- | A handler that invokes a breakpoint upon matching against the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation
-}
handleParticle :: Model '[Sampler] a -> LogP -> Sampler (Model '[Sampler] a, LogP)
handleParticle model w = (handleIO . defaultSample . step w) model

step :: LogP -> Handler Observe es a (Comp (Observe : es) a, LogP)
step w (Val x)   = Val (Val x, w)
step w (Op op k) = case discharge op of
  Right (Observe d y α) -> Val (k y, w + logProb d y)
  Left op'              -> Op op' (step w . k)

{- | A handler for multinomial resampling of particles.
-}

handleResampleMul :: Member Sampler es => Handler (Resample LogP) es b b
handleResampleMul = handle () (const Val) (const hop) where
  hop :: Member Sampler es =>  Resample LogP x -> (() -> x -> Comp es b) -> Comp es b
  hop  (Resample (prts, ws)) k = do
    let n = length ws
    idxs <- call (replicateM n (Sampler.sampleCategorical (Vector.fromList (map exp ws))))
    let prts_res  = map (prts !! ) idxs
        ws_res    = (replicate n . logMeanExp . map (ws  !! )) idxs

    k () (zip prts_res ws_res)

resampleMul :: [LogP] -> Sampler [Int]
resampleMul ws = do
  let ps = map exp ws
  -- | Select particles to continue with
  replicateM (length ps) (Sampler.sampleCategorical (Vector.fromList ps))

{- | A handler for systematic resampling of particles.
-}
handleResampleSys :: Member Sampler fs => Handler (Resample LogP) fs a a
handleResampleSys (Val x) = Val x
handleResampleSys (Op op k) = case discharge op of
  Right (Resample (prts, ws)) -> do
    -- | Get the weights for each particle
    let ps = map exp ws
    -- | Select particles to continue with
    u <- random
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
        ws_res   = map (ws !! ) idxs
        ws_mean   = map (const (logMeanExp ws_res)) ws_res

    (handleResampleSys . k) (zip prts_res ws_mean)
  Left op' -> Op op' (handleResampleSys . k)
