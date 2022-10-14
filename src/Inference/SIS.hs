{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

{- An infrastructure for Sequential Importance Sampling.
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
  -- | Initialise a particle context
  pempty :: ctx
  -- | For each particle, accumulate its incremental context and its previous context
  paccum  :: [ctx] -- ^ previously acccumulated context
          -> [ctx] -- ^ incremental context
          -> [ctx]

instance ParticleCtx LogP where
  pempty                =  0
  -- | Compute normalised accumulated log weights
  paccum log_ps log_ps' = let logZ = logMeanExp log_ps
                          in  map (+ logZ) log_ps'

{- | The @Resample@ effect for resampling according to collection of particle contexts.
-}
data Resample es ctx  a where
  Resample
    -- | ((particles, contexts), initial probabilistic program)
    -- :: ([SISProg ctx a], [ctx], ProbProg a)
    :: ([Prog (Resample es ctx : es) a], Prog es a, [ctx])
    -- | ((resampled particles, resampled contexts), idxs)
    -> Resample es ctx  (([Prog (Resample es ctx : es) a], [ctx]), [Int])

-- data Resample' es a where
--   Resample'
--     -- | ((particles, contexts), initial probabilistic program)
--     -- :: ([SISProg ctx a], [ctx], ProbProg a)
--     :: ([Prog (Resample' es : es) a], [Prog es a], [TracedParticle])
--     -- | ((resampled particles, resampled contexts), idxs)
--     -> Resample' es (([Prog (Resample' es : es) a], [TracedParticle]), [Int])


{- | A @ParticleRunner@ handler runs a particle to the next @Observe@ break point.
-}
type ParticleRunner ctx
  = forall es a. (Members [Observe, Sample] es, LastMember (Lift Sampler) es)
  -- | a particle
  => Prog es a
  -- | (a particle suspended at the next step, corresponding context)
  -> Prog es (Prog es a,  ctx)

{- | A @ParticleResampler@ handler decides which of the current particles to continue execution with.
-}
type ParticleResampler es ctx a
  =  LastMember (Lift Sampler) es
  => Prog (Resample es ctx : es) a
  -> Prog es a


{- | A top-level template for sequential importance sampling.
-}
sis :: forall ctx a b es. ParticleCtx ctx => (Members [Observe, Sample] es, LastMember (Lift Sampler) es)
  => Int                                                      -- ^ number of particles
  -> ParticleRunner    ctx                                    -- ^ handler for running particles
  -> ParticleResampler es ctx [(a, ctx)]                      -- ^ handler for resampling particles
  -> Prog es a                   -- ^ initial probabilistic program
  -> Prog es [(a, ctx)]          -- ^ (final particle output, final particle context)
sis n_particles particleRunner particleResampler prog = do
  -- | Create an initial population of particles and contexts
  let population = unzip $ replicate n_particles (weakenProg @(Resample es ctx) prog, pempty)
  -- | Execute the population until termination
  particleResampler (loopSIS particleRunner prog population)

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
loopSIS :: forall ctx es a. ParticleCtx ctx => (Members [Observe, Sample] es, LastMember (Lift Sampler) es)
  => ParticleRunner ctx                     -- ^ handler for running particles
  -> Prog es a                             -- ^ initial probabilistic program
  -> ([Prog (Resample es ctx  : es) a], [ctx])               -- ^ input particles and corresponding contexts
  -> Prog (Resample es ctx  : es) [(a, ctx)]                 -- ^ final particle results and corresponding contexts
loopSIS particleRunner prog = loop
  where
    loop :: ([Prog (Resample es ctx : es) a], [ctx]) -> Prog (Resample es ctx : es) [(a, ctx)]
    loop (particles, ctxs) = do
      -- | Run particles to next checkpoint and accumulate their contexts
      (particles',  ctxs') <- second (ctxs `paccum`) . unzip <$> mapM particleRunner particles
      -- | Check termination status of particles
      case foldVals particles' of
        -- | If all particles have finished, return their results and contexts
        Right vals  -> (`zip` ctxs') <$> vals
        -- | Otherwise, pick the particles to continue with
        Left  _     -> --call (Resample (particles', ctxs', prog))
                         -- >>= loop . fst
                       call (Resample (particles', prog, ctxs'))
                          >>= loop . fst