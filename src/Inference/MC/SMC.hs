

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}


{- Sequential Monte Carlo inference.
-}

module Inference.MC.SMC where

import           Control.Monad ( replicateM )
import qualified Data.Vector as Vector
import           Effects.MulDist ( pattern ObsPrj, handleMulDist, Addr, MulDist, Observe (..), Sample )
import           Effects.EnvRW ( EnvRW, handleEnvRW )
import           Env ( Env )
import           LogP ( LogP(..), logMeanExp, logSumExp )
import           Model ( MulModel(runModel), Model )
import           Dist ( mkCategorical, drawWithSampler, logProb )
import           Comp ( LastMember, Comp(..), Members, Member, runImpure, call, weakenProg, discharge, prj, handle, handleWith, Handler)
import qualified Data.Map as Map
import           Inference.MC.SIM as SIM
import qualified Inference.MC.SIS as SIS
import           Inference.MC.SIS (Resample(..), ModelStep, pfilter)
import           Sampler ( Sampler, random, sampleCategorical, liftIO )

{- | Call SMC on a model.
-}
mulpfilterWith
  :: Int                                -- ^ number of particles
  -> MulModel env [EnvRW env, MulDist, Sampler] a      -- ^ model
  -> Env env                            -- ^ input model environment
  -> Sampler [Env env]                  -- ^ output model environments of each particle
mulpfilterWith n_prts gen_model env_in = do
  -- | Handle model to probabilistic program
  let model = (handleMulDist . handleEnvRW env_in) (runModel gen_model)
  smc_trace <- mulpfilter n_prts model
  pure (map (snd . fst) smc_trace)

{- | Call SMC on a probabilistic program.
-}
mulpfilter :: Int -> Model '[Sampler] a -> Sampler [(a, LogP)]
mulpfilter n_prts = runImpure . handleResampleMul . pfilter n_prts 0 exec

{- | A handler that invokes a breakpoint upon matching against the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation
-}
exec :: (Model '[Sampler] a, LogP) -> Sampler (Model '[Sampler] a, LogP)
exec (p, w) = (runImpure . defaultSample . advance w) p

advance :: LogP -> Handler Observe es a (Comp (Observe : es) a, LogP)
advance w (Val x)   = Val (Val x, w)
advance w (Op op k) = case discharge op of
  Right (Observe d y α) -> Val (k y, w + logProb d y)
  Left op'              -> Op op' (advance w . k)

{- | A handler for multinomial resampling of particles.
-}

handleResampleMul :: Member Sampler es => Handler (Resample LogP) es b b
handleResampleMul = handle Val hop where
  hop :: Member Sampler es =>  Resample LogP x -> (x -> Comp es b) -> Comp es b
  hop  (Resample pws) k = do
    let (ps, ws) = unzip pws;
        -- | Compute the sum of all particles' probabilities (in LogP form, i.e. their logSumExp)
        z        = logSumExp ws
    if  -- | Require at least some particles' probabilities to be greater than zero
        not (isInfinite z)
      then do
        let -- | Normalise the particles' probabilities (by dividing by their total)
            ws_norm  = map (exp . subtract z) ws
            n        = length ws
        idxs <- call $ (replicateM n . Sampler.sampleCategorical) (Vector.fromList ws_norm)
        let -- | Resample particles
            ps_res   = map (ps !! ) idxs
            -- | Get average particle probability (in LogP form, i.e. their logMeanExp)
            w_avg    = z - log (fromIntegral n)
        -- | Set the post-resampling weights to be uniform as the average particle weight
        --   (such that their logSumExp is the same before and after resampling).
        k (map (, w_avg) ps_res)
      else  k pws