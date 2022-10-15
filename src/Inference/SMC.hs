{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

{- Sequential Monte Carlo inference.
-}

module Inference.SMC where

import Control.Monad ( replicateM )
import Effects.Dist ( pattern ObsPrj, handleDist, Addr, Dist, Observe, Sample )
import Effects.Lift ( Lift, lift, handleLift )
import Effects.NonDet ( asum, handleNonDet, NonDet )
import Effects.ObsRW ( ObsRW, handleObsRW )
import Env ( Env )
import LogP ( LogP(..), logMeanExp )
import Model ( Model(runModel) )
import OpenSum (OpenSum)
import PrimDist ( Categorical(..), sample, logProb )
import Prog ( LastMember, Prog(..), Members, Member, call, weakenProg, discharge )
import qualified Data.Map as Map
import qualified Inference.SIM as SIM
import qualified Inference.SIS as SIS
import Inference.SIS (Resample(..), ParticleResampler, ParticleHandler, ParticleCtx (..))
import Sampler ( Sampler, sampleRandom )

{- | The context of a particle for SMC.
-}
newtype Particle = Particle {
    particleLogProb  :: LogP      -- ^ associated log-probability
  } deriving (Num, ParticleCtx)

{- | Call SMC on a model.
-}
smc
  :: Int                                              -- ^ number of particles
  -> Model env [ObsRW env, Dist, Lift Sampler] a      -- ^ model
  -> Env env                                          -- ^ input model environment
  -> Sampler [Env env]                                -- ^ output model environments of each particle
smc n_particles model env_in = do
  let prog = (handleDist . handleObsRW env_in) (runModel model)
  smcInternal n_particles prog >>= pure . map (snd . fst)

{- | Call SMC on a probabilistic program.
-}
smcInternal
  :: Int                                              -- ^ number of particles
  -> Prog [Observe, Sample, Lift Sampler] a           -- ^ probabilistic program
  -> Sampler [(a, Particle)]                          -- ^ final particle results and contexts
smcInternal n_particles =
  handleLift . SIM.handleSamp . SIM.handleObs . SIS.sis n_particles particleRunner handleResample

{- | A handler for multinomial resampling of particles.
-}
handleResample :: LastMember (Lift Sampler) es => ParticleResampler es Particle
handleResample (Val x) = Val x
handleResample (Op op k) = case discharge op of
  -- Right (Resample (prts, ctxs, prog_0)) -> do
  Right (Resample (prts, ctxs) _) -> do
    -- | Get the weights for each particle
    let ws = map (exp . unLogP . particleLogProb) ctxs
    -- | Select particles to continue with
    idxs <- replicateM (length ws) $ lift (sample (Categorical ws))
    let resampled_prts = map (prts !! ) idxs
        resampled_ctxs = map (ctxs !! ) idxs

    (handleResample . k) ((resampled_prts, resampled_ctxs), idxs)
  Left op' -> Op op' (handleResample . k)

{- | A handler for systematic resampling of particles.
-}
handleResampleSys :: LastMember (Lift Sampler) es => ParticleResampler es Particle
handleResampleSys (Val x) = Val x
handleResampleSys (Op op k) = case discharge op of
  -- Right (Resample (prts, ctxs, prog_0)) -> do
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

{- | A handler that invokes a breakpoint upon matching against the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation + the address of the breakpoint
-}
particleRunner :: Member Observe es
  -- | a particle
  => Prog es a
  -- | (a particle suspended at the next step, corresponding context)
  -> Prog es (Prog es a, Particle)
particleRunner = loop
  where loop (Val x) = pure (Val x,  Particle 0)
        loop (Op op k) = case op of
          ObsPrj d y α -> Val (k y,  Particle (logProb d y))
          _            -> Op op (particleRunner . k)

