{-# LANGUAGE RankNTypes #-}


{-# LANGUAGE PatternSynonyms #-}


{-# LANGUAGE FlexibleContexts #-}

{- An infrastructure for Sequential Importance (Re)Sampling.
-}

module Inference.MC.SIS where

import           Control.Monad ( mapAndUnzipM )
import           Data.Map (Map)
import qualified Data.Map as Map
import           Effects.Dist ( Addr, Observe (Observe), Sample, pattern ObsPrj )
import           LogP ( LogP, logMeanExp )
import           Comp ( Comp (..), weakenProg, Member, discharge, call, weaken, LastMember, Members )
import           Sampler
import           Util ( uncurry3 )
import           Model
import           Inference.MC.SIM as SIM
import           Data.Bifunctor


{- | The @Resample@ effect for resampling according to collection of particle contexts.
-}
data Resample p a where
  Resample
    -- | (particles, contexts)
    :: ([Model es b], [p])
    -- | (resampled programs, resampled ss)
    -> Resample p [(Model es b, p)]

{- | A @ParticleHandler@  runs a particle to the next @Observe@ break point.
-}
type ParticleHandler es p = forall a. Model es a -> p -> Sampler (Model es a, p)

{- | A top-level template for sequential importance sampling.
-}
sis :: (Members [Resample p, Sampler] fs)
  => Int                                                        -- ^ number of particles
  -> ParticleHandler es p                                        -- ^ handler for running particles
  -> p
  -> Model es a                                                 -- ^ initial probabilistic program
  -> Comp fs [(a, p)]                        -- ^ (final particle output, final particle context)
sis n_prts exec ρ_0 prog_0  = do
  -- | Create an initial population of particles and contexts
  let population = replicate n_prts (prog_0, ρ_0)
  -- | Execute the population until termination
  pfilter exec population

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
pfilter :: (Members [Resample p, Sampler] fs)
  => ParticleHandler es p                                 -- ^ handler for running particles
  -> [(Model es a, p)]                               -- ^ input particles and corresponding contexts
  -> Comp fs [(a, p)]                          -- ^ final particle results and corresponding contexts
pfilter exec wprts = do
  -- | Run particles to next checkpoint and accumulate their contexts
  wprts' <- call ((mapM . uncurry) exec wprts)
  -- ρs'   <- call (Accum ρs partialρs)
  -- | Check termination status of particles
  case collapse wprts' of
    -- | If all particles have finished, return their results and contexts
    Just vals  -> Val vals
    -- | Otherwise, pick the particles to continue with
    Nothing    -> call (Resample (unzip wprts')) >>= pfilter exec

{- | Check whether a list of programs have all terminated.
     If at least one program is unfinished, return all programs.
     If all programs have finished, return a single program that returns all results.
-}
collapse :: [(Model es a, p)] -> Maybe [(a, p)]
collapse ((Val v, p) : progs) = do
  vs <- collapse progs
  pure ((v, p):vs)
collapse []    = pure []
collapse progs = Nothing
