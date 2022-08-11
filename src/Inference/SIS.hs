{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

{- An infrastructure for Sequential Importance Sampling (particle filter).
-}

module Inference.SIS where

import Data.Map (Map)
import qualified Data.Map as Map
import Effects.Dist ( Addr, Observe (Observe), Sample, pattern ObsPrj )
import Effects.Lift ( Lift, handleLift )
import Effects.NonDet ( foldVals, weakenNonDet, NonDet, asum, branchWeaken, handleNonDet )
import LogP ( LogP, logMeanExp )
import Prog ( Prog (..), weakenProg, Member, discharge, call, weaken, LastMember, Members )
import Sampler ( Sampler )
import Util ( uncurry3 )
import Inference.SIM as SIM
import Data.Bifunctor

{- | A @ParticleCtx@ contains data about the execution of a particular particle.
-}
class ParticleCtx ctx where
  -- | For each particle, accumulate its incremental context and its previous context
  paccum  :: [ctx] -- ^ previously acccumulated context
          -> [ctx] -- ^ incremental context
          -> [ctx]
  -- | Initialise the contexts for n particles
  pempty :: ctx

{- | A @ParticleRunner@ handler runs a particle to the next @Observe@ break point.
-}
type ParticleRunner ctx
  = forall es a. Member Observe es
  -- | a particle
  => Prog es a
  -- | (a particle suspended at the next step, corresponding context)
  -> Prog es (Prog es a, ctx)

{- | A @ParticleResampler@ handler decides which of the current particles to continue execution with.
-}
type ParticleResampler ctx
  =  forall es a. LastMember (Lift Sampler) es
  => Prog (Resample ctx : es) a
  -> Prog es a

{- | The @Resample@ effect for resampling according.
-}
data Resample ctx a where
  Resample :: ([a], [ctx]) -> Resample ctx ([a], [ctx])

{- | A top-level template for sequential importance sampling.
-}
sis :: ParticleCtx ctx
  => Int                                                                    -- ^ num of particles
  -> ParticleRunner    ctx
  -> ParticleResampler ctx
  -> Prog [Resample ctx, Observe, Sample, Lift Sampler] a                   -- ^ model
  -> Sampler [(a, ctx)]                                                     -- ^ (final particle output, final particle context)
sis n_particles particleRunner particleResampler prog = do

  let population_0 = unzip $ replicate n_particles (weakenNonDet prog, pempty)

  -- | Execute the population until termination
  (handleLift . SIM.handleSamp . SIM.handleObs . particleResampler) (loopSIS particleRunner population_0)

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
loopSIS
  :: (ParticleCtx ctx
    , Members [Observe, Resample ctx] es
    , LastMember (Lift Sampler) es)
  => ParticleRunner ctx
  -> ([Prog (NonDet : es) a], [ctx])  -- ^ particles and corresponding contexts
  -> Prog es [(a, ctx)]
loopSIS particleRunner (particles, ctxs) = do
  -- | Run particles to next checkpoint and accumulate their contexts
  (particles', ctxs') <- second (ctxs `paccum`) . unzip <$> (handleNonDet . particleRunner . asum) particles
  case foldVals particles' of
    -- | If all programs have finished, return their results along with their accumulated contexts
    Right vals  -> (`zip` ctxs') <$> vals
    -- | Otherwise, pick the programs to continue with
    Left  _     -> call (Resample (particles', ctxs')) >>= loopSIS particleRunner