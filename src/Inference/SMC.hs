{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE InstanceSigs #-}

{- Sequential Monte Carlo inference.
-}

module Inference.SMC where

import Control.Monad ( replicateM )
import Effects.Dist ( pattern ObsPrj, handleDist, Addr, Dist, Observe (..), Sample )
import Effects.Lift ( Lift, lift, liftPrint, handleLift)
import Effects.ObsRW ( ObsRW, handleObsRW )
import Env ( Env )
import LogP ( LogP(..), logMeanExp )
import Model ( Model(runModel), ProbSig )
import OpenSum (OpenSum)
import PrimDist ( Categorical(..), sample, logProb )
import Prog ( LastMember, Prog(..), Members, Member, call, weakenProg, discharge )
import qualified Data.Map as Map
import qualified Inference.SIM as SIM
import qualified Inference.SIS as SIS
import Inference.SIS (Resample(..), ResampleHandler, ParticleHandler, ParticleCtx (..))
import Sampler ( Sampler, sampleRandom)
import Prog (prj)

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
  :: Int                                              -- ^ number of particles
  -> Model env [ObsRW env, Dist, Lift Sampler] a      -- ^ model
  -> Env env                                          -- ^ input model environment
  -> Sampler [Env env]                                -- ^ output model environments of each particle
smc n_particles model env_in = do
  -- | Handle model to probabilistic program
  let prog_0 = (handleDist . handleObsRW env_in) (runModel model)
  smc_trace <- (handleLift . SIM.handleSamp . SIM.handleObs)
               (smcInternal n_particles prog_0)
  pure (map (snd . fst) smc_trace)

{- | Call SMC on a probabilistic program.
-}
smcInternal :: ProbSig es
  => Int                       -- ^ number of particles
  -> Prog es a                 -- ^ probabilistic program
  -> Prog es [(a, Particle)]   -- ^ final particle results and contexts
smcInternal n_particles =
  SIS.sis n_particles handleParticle handleResampleMul

{- | A handler that invokes a breakpoint upon matching against the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation
-}
handleParticle :: Member Observe es
  -- | a particle
  => Prog es a
  -- | (a particle suspended at the next step, corresponding context)
  -> Prog es (Prog es a, Particle)
handleParticle (Val x)   = pure (Val x,  Particle 0)
handleParticle (Op op k) = case prj op of
  Just (Observe d y α) -> Val (k y,  Particle (logProb d y))
  Nothing              -> Op op (handleParticle . k)

{- | A handler for multinomial resampling of particles.
-}
handleResampleMul :: LastMember (Lift Sampler) es => ResampleHandler es Particle
handleResampleMul (Val x) = Val x
handleResampleMul (Op op k) = case discharge op of
  Right (Resample (prts, ctxs) _) -> do
    -- | Get the weights for each particle
    let ws = map (exp . unLogP . particleLogProb) ctxs
    -- | Select particles to continue with
    idxs <- replicateM (length ws) $ lift (sample (Categorical ws))
    let resampled_prts = map (prts !! ) idxs
        resampled_ctxs = map (ctxs !! ) idxs
    liftPrint ("Weights: " ++ show ws)
    (handleResampleMul . k) ((resampled_prts, resampled_ctxs), idxs)
  Left op' -> Op op' (handleResampleMul . k)

{- | A handler for systematic resampling of particles.
-}
handleResampleSys :: LastMember (Lift Sampler) es => ResampleHandler es Particle
handleResampleSys (Val x) = Val x
handleResampleSys (Op op k) = case discharge op of
  Right (Resample (prts, ctxs) _) -> do
    -- | Get the weights for each particle
    let ws = map (exp . unLogP . particleLogProb) ctxs
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
