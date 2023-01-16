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
import           Effects.Lift ( handleM)
import           LogP ( LogP, logMeanExp )
import           Prog ( Prog (..), weakenProg, Member, discharge, call, weaken, LastMember, Members )
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
    -> Resample p ([Model es b], [p])
  Accum
    :: [p] -> [p] -> Resample p [p]

{- | A @ParticleHandler@  runs a particle to the next @Observe@ break point.
-}
type ParticleHandler es p = forall a. Model es a -> Sampler (Model es a, p)

{- | A top-level template for sequential importance sampling.
-}
sis :: (Members [Resample p, Sampler] fs)
  => Int                                                        -- ^ number of particles
  -> ParticleHandler es p                                        -- ^ handler for running particles
  -> p
  -> Model es a                                                 -- ^ initial probabilistic program
  -> Prog fs [(a, p)]                        -- ^ (final particle output, final particle context)
sis n_prts hdlParticle ρ_0 prog_0  = do
  -- | Create an initial population of particles and contexts
  let population = unzip $ replicate n_prts (prog_0, ρ_0)
  -- | Execute the population until termination
  pfilter hdlParticle population

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
pfilter :: (Members [Resample p, Sampler] fs)
  => ParticleHandler es p                                 -- ^ handler for running particles
  -> ([Model es a], [p])                               -- ^ input particles and corresponding contexts
  -> Prog fs [(a, p)]                          -- ^ final particle results and corresponding contexts
pfilter hdlParticle  (prts, ρs) = do
  -- | Run particles to next checkpoint and accumulate their contexts
  (prts', partialρs) <- call (mapAndUnzipM hdlParticle prts)
  ρs'                <- call (Accum ρs partialρs)
  -- | Check termination status of particles
  case collapse prts' of
    -- | If all particles have finished, return their results and contexts
    Right vals  -> (`zip` ρs') <$> vals
    -- | Otherwise, pick the particles to continue with
    Left  _     -> call (Resample (prts', ρs')) >>= pfilter hdlParticle

{- | Check whether a list of programs have all terminated.
     If at least one program is unfinished, return all programs.
     If all programs have finished, return a single program that returns all results.
-}
collapse :: [Model es a] -> Either [Model es a] (Prog fs [a])
collapse (Val v : progs) = do
  vs <- collapse progs
  pure ((v:) <$> vs)
collapse []    = pure (Val [])
collapse progs = Left progs
