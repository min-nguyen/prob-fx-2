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

module Inference.SMC where

import Control.Monad ( replicateM )
import Effects.Dist ( pattern ObsPrj, handleDist, Addr, Dist, Observe (..), Sample )
import Effects.Lift ( Lift, lift, liftPrint, handleLift)
import Effects.ObsRW ( ObsRW, handleObsRW )
import Env ( Env )
import LogP ( LogP(..), logMeanExp, expLogP )
import Model ( Model(runModel), ProbProg )
import OpenSum (OpenSum)
import PrimDist ( mkCategorical, sample, logProb )
import Prog ( LastMember, Prog(..), Members, Member, call, weakenProg, discharge, prj )
import qualified Data.Map as Map
import qualified Inference.SIM as SIM
import qualified Inference.SIS as SIS
import Inference.SIS (Resample(..), ResampleHandler, ParticleHandler, ParticleCtx (..))
import Sampler ( Sampler, sampleRandom)

{- | The context of a particle for SMC.
-}
newtype Particle = Particle {
    particleLogProb  :: LogP      -- ^ associated log-probability
  } deriving (Num, ParticleCtx)

instance ParticleCtx LogP where
  pempty :: LogP
  pempty = 0
  -- | Compute normalised accumulated log weights
  paccum :: [LogP] -> [LogP] -> [LogP]
  paccum log_ps log_ps' = let logZ = logMeanExp log_ps
                          in  map (+ logZ) log_ps'

{- | Call SMC on a model.
-}
smc
  :: Int                                -- ^ number of particles
  -> Model env [ObsRW env, Dist] a      -- ^ model
  -> Env env                            -- ^ input model environment
  -> Sampler [Env env]                  -- ^ output model environments of each particle
smc n_particles model env_in = do
  -- | Handle model to probabilistic program
  let prog_0 = (handleDist . handleObsRW env_in) (runModel model)
  smc_trace <- handleLift (smcInternal n_particles prog_0)
  pure (map (snd . fst) smc_trace)

{- | Call SMC on a probabilistic program.
-}
smcInternal :: (LastMember (Lift Sampler) fs)
  => Int                       -- ^ number of particles
  -> ProbProg a                 -- ^ probabilistic program
  -> Prog fs [(a, Particle)]   -- ^ final particle results and contexts
smcInternal n_particles =
  handleResampleMul . SIS.sis n_particles handleParticle

{- | A handler that invokes a breakpoint upon matching against the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation
-}
handleParticle :: ProbProg a -> Sampler (ProbProg a, Particle)
handleParticle = SIM.handleSamp . handleObs

handleObs :: Prog (Observe : es) a -> Prog es (Prog (Observe : es) a, Particle)
handleObs (Val x)   = Val (Val x,  Particle 0)
handleObs (Op op k) = case discharge op of
  Right (Observe d y α) -> Val (k y,  Particle (logProb d y))
  Left op'              -> Op op' (handleObs . k)

{- | A handler for multinomial resampling of particles.
-}
handleResampleMul :: LastMember (Lift Sampler) fs => ResampleHandler fs Particle
handleResampleMul (Val x) = Val x
handleResampleMul (Op op k) = case discharge op of
  Right (Resample (prts, ctxs) _) -> do
    -- | Get the weights for each particle
    let ws = map (expLogP . particleLogProb) ctxs
    -- | Select particles to continue with
    idxs <- replicateM (length ws) $ lift (sample (mkCategorical ws))
    let resampled_prts = map (prts !! ) idxs
        resampled_ctxs = map (ctxs !! ) idxs
    (handleResampleMul . k) ((resampled_prts, resampled_ctxs), idxs)
  Left op' -> Op op' (handleResampleMul . k)

{- | A handler for systematic resampling of particles.
-}
handleResampleSys :: LastMember (Lift Sampler) fs => ResampleHandler fs Particle
handleResampleSys (Val x) = Val x
handleResampleSys (Op op k) = case discharge op of
  Right (Resample (prts, ctxs) _) -> do
    -- | Get the weights for each particle
    let ws = map (expLogP . particleLogProb) ctxs
    -- | Select particles to continue with
    u <- lift sampleRandom
    let prob i = ws !! i
        n      = length ws
        inc = 1 / fromIntegral n
        f i _ _ _ acc | i == n = acc
        f i v j q acc =
          if v < q
            then f (i + 1) (v + inc) j q (j - 1 : acc)
            else f i v (j + 1) (q + prob j) acc
        idxs = f 0 (u / fromIntegral n) 0 0 []
        resampled_prts = map (prts !! ) idxs
        resampled_ctxs = map (ctxs !! ) idxs

    (handleResampleSys . k) ((resampled_prts, resampled_ctxs), idxs)
  Left op' -> Op op' (handleResampleSys . k)
