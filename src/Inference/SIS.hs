{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

{- An infrastructure for Sequential Importance Sampling (particle filter).
-}

module Inference.SIS where

import Data.Map (Map)
import qualified Data.Map as Map
import Effects.Dist ( Addr, Observe (Observe), Sample, pattern ObsPrj )
import Effects.Lift ( Lift, handleLift, lift )
import Effects.NonDet ( foldVals, weakenNonDet, NonDet, asum, branchWeaken, handleNonDet )
import LogP ( LogP, logMeanExp )
import Prog ( Prog (..), weakenProg, Member, discharge, call, weaken, LastMember, Members )
import Sampler
import Util ( uncurry3 )
import Model
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

{- | The @Resample@ effect for resampling according to collection of particle contexts.
-}
data Resample ctx a where
  Resample
    -- | (particles, contexts)
    :: ([SISProg ctx a], [ctx], ProbProg a)
    -- | ((resampled particles, resampled contexts), idxs)
    -> Resample ctx (([SISProg ctx a], [ctx]), [Int])

type SISProg ctx a = Prog [Resample ctx, Observe, Sample, Lift Sampler] a

{- | A @ParticleRunner@ handler runs a particle to the next @Observe@ break point.
-}
type ParticleRunner ctx
  = forall es a. (Members [Observe, Sample] es, LastMember (Lift Sampler) es)
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


{- | A top-level template for sequential importance sampling.
-}
sis :: forall ctx a. ParticleCtx ctx
  => Int                                                      -- ^ number of particles
  -> ParticleRunner    ctx                                    -- ^ handler for running particles
  -> ParticleResampler ctx                                    -- ^ handler for resampling particles
  -> Prog [Observe, Sample, Lift Sampler] a     -- ^ model
  -> Sampler [(a, ctx)]                                       -- ^ (final particle output, final particle context)
sis n_particles particleRunner particleResampler prog_0 = do
  -- | Create an initial population of particles and contexts
  let population = unzip $ replicate n_particles (weakenProg @(Resample ctx) prog_0, pempty)
  -- | Execute the population until termination
  (handleLift . SIM.handleSamp . SIM.handleObs . particleResampler )
    (loopSIS particleRunner prog_0 population)

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
loopSIS :: (ParticleCtx ctx)
  => ParticleRunner ctx                     -- ^ handler for running particles
  -> ProbProg a
  -> ([SISProg ctx a], [ctx])        -- ^ input particles and corresponding contexts
  -> SISProg ctx [(a, ctx)]                     -- ^ final particle results and corresponding contexts
loopSIS particleRunner prog_0 (particles, ctxs) = do
  -- | Run particles to next checkpoint and accumulate their contexts
  lift $ liftIO $ print ("SIS: " ++ show (length ctxs))
  (particles', ctxs') <-  second (ctxs `paccum`) . unzip <$> mapM particleRunner particles
  lift $ liftIO $ print (length ctxs')
  -- | Check termination status of particles
  case foldVals particles' of
    -- | If all particles have finished, return their results and contexts
    Right vals  -> (`zip` ctxs') <$> vals
    -- | Otherwise, pick the particles to continue with
    Left  _     -> call (Resample (particles', ctxs', prog_0))
                      >>= loopSIS particleRunner prog_0 . fst