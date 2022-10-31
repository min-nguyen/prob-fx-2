{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

{- An infrastructure for Sequential Importance (Re)Sampling.
-}

module Inference.SIS where

import Data.Map (Map)
import qualified Data.Map as Map
import Effects.Dist ( Addr, Observe (Observe), Sample, pattern ObsPrj )
import Effects.Lift ( Lift, handleLift, lift )
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

{- | The @Resample@ effect for resampling according to collection of particle contexts.
-}
data Resample es ctx  a where
  Resample
    -- | (particles, contexts)
    :: ([Prog (Resample es ctx : es) a], [ctx])
    -- | initial probabilistic program
    -> Prog es a
    -- | ((resampled programs, resampled ctxs), resampled idxs)
    -> Resample es ctx (([Prog (Resample es ctx : es) a], [ctx]), [Int])

{- | A @ParticleHandler@  runs a particle to the next @Observe@ break point.
-}
type ParticleHandler ctx
  = forall es a. (ProbSig es)
  -- | a particle
  => Prog es a
  -- | (a particle suspended at the next step, corresponding context)
  -> Prog es (Prog es a, ctx)

{- | A @ResampleHandler@ decides which of the current particles to continue execution with.
-}
type ResampleHandler es ctx
  =  forall a. (ProbSig es)
  => Prog (Resample es ctx : es) a
  -> Prog es a

{- | A top-level template for sequential importance sampling.
-}
sis :: (ParticleCtx ctx, ProbSig es)
  => Int                                                                       -- ^ number of particles
  -> (forall a es. ProbSig es => Prog es a -> Prog es (Prog es a, ctx))        -- ^ handler for running particles
  -> Prog es a                                                                 -- ^ initial probabilistic program
  -> Prog (Resample es ctx : es) [(a, ctx)]                                                        -- ^ (final particle output, final particle context)
sis n_particles hdlParticle  prog_0 = do
  -- | Create an initial population of particles and contexts
  let population = unzip $ replicate n_particles (weakenProg prog_0, pempty)
  -- | Execute the population until termination
  loopSIS hdlParticle prog_0 population

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
loopSIS :: forall ctx es a. (ParticleCtx ctx, ProbSig es)
  => ParticleHandler ctx                                    -- ^ handler for running particles
  -> Prog es a                                              -- ^ initial probabilistic program
  -> ([Prog (Resample es ctx : es) a], [ctx])               -- ^ input particles and corresponding contexts
  -> Prog (Resample es ctx  : es) [(a, ctx)]                -- ^ final particle results and corresponding contexts
loopSIS hdlParticle prog_0 = loop
  where
    loop :: ([Prog (Resample es ctx : es) a], [ctx]) -> Prog (Resample es ctx : es) [(a, ctx)]
    loop (particles, ctxs) = do
      -- | Run particles to next checkpoint and accumulate their contexts
      (particles',  ctxs') <- second (ctxs `paccum`) . unzip <$> mapM hdlParticle particles
      -- | Check termination status of particles
      case foldVals particles' of
        -- | If all particles have finished, return their results and contexts
        Right vals  -> (`zip` ctxs') <$> vals
        -- | Otherwise, pick the particles to continue with
        Left  _     -> call (Resample (particles', ctxs') prog_0) >>= loop . fst

{- | Check whether a list of programs have all terminated.
     If at least one program is unfinished, return all programs.
     If all programs have finished, return a single program that returns all results.
-}
foldVals :: [Prog es' a] -> Either [Prog es' a] (Prog es [a])
foldVals (Val v : progs) = do
  vs <- foldVals progs
  pure ((v:) <$> vs)
foldVals []    = pure (Val [])
foldVals progs = Left progs
