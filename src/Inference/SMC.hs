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
import PrimDist ( PrimDist(Categorical), sample, logProb )
import Prog ( LastMember, Prog(..), Members, Member, call, weakenProg, discharge )
import qualified Data.Map as Map
import qualified Inference.SIM as SIM
import qualified Inference.SIS as SIS
import Inference.SIS (Resample(..), ParticleResampler, ParticleRunner, ParticleCtx (..))
import Sampler ( Sampler )

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
  -> Sampler [(a, Particle)]                       -- ^ final particle results and contexts
smcInternal n_particles =
  handleLift . SIM.handleSamp . SIM.handleObs . SIS.sis n_particles particleRunner particleResampler

{- | A handler for resampling particles according to their normalized log-likelihoods.
-}
particleResampler :: ParticleResampler Particle
particleResampler (Val x) = Val x
particleResampler (Op op k) = case discharge op of
  Right (Resample (prts, ctxs, prog_0)) -> do
    -- | Get the weights for each particle
    let ws = map (exp . unLogP . particleLogProb) ctxs
    -- | Select particles to continue with
    idxs <- multinomial ws
    let resampled_prts = map (prts !! ) idxs
        resampled_ctxs = map (ctxs !! ) idxs

    (particleResampler . k) ((resampled_prts, resampled_ctxs), idxs)
  Left op' -> Op op' (particleResampler . k)

-- | Multinomial resampler
multinomial :: LastMember (Lift Sampler) es => [Double] -> Prog es [Int]
multinomial ws = replicateM (length ws) $ lift (sample (Categorical ws))

-- | Systematic resampler
systematic :: Double -> [Double] -> [Int]
systematic u ps = f 0 (u / fromIntegral n) 0 0 []
  where
    prob i = ps !! i
    n = length ps
    inc = 1 / fromIntegral n
    f i _ _ _ acc | i == n = acc
    f i v j q acc =
      if v < q
        then f (i + 1) (v + inc) j q (j - 1 : acc)
        else f i v (j + 1) (q + prob j) acc

{- | A handler that invokes a breakpoint upon matching against the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation + the address of the breakpoint
-}
particleRunner :: ParticleRunner Particle
particleRunner (Val x) = pure (Val x, Particle 0)
particleRunner (Op op k) = case op of
  ObsPrj d y α -> Val (k y, Particle (logProb d y))
  _            -> Op op (particleRunner . k)

