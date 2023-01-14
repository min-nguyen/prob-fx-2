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
    :: ([ProbProg es a], [p])
    -- | initial probabilistic program
    -> ProbProg es a
    -- | (resampled programs, resampled ss)
    -> Resample p ([ProbProg es a], [p])
  Accum
    -- | (particles, contexts)
    :: [p]
    -- | initial probabilistic program
    -> [p]
    -- | (resampled programs, resampled ss)
    -> Resample p [p]

{- | A @ParticleHandler@  runs a particle to the next @Observe@ break point.
-}
type ParticleHandler p = forall es a. ProbProg es a -> Sampler (ProbProg es a, p)

{- | A @ResampleHandler@ decides which of the current particles to continue execution with.
-}
type ResampleHandler fs p = forall a. Prog (Resample p : fs) a -> Prog fs a

{- | A top-level template for sequential importance sampling.
-}
sis :: (Members [Resample p, Sampler] fs)
  => Int                                                        -- ^ number of particles
  -> ParticleHandler p                                        -- ^ handler for running particles
  -> p
  -> ProbProg es a                                                 -- ^ initial probabilistic program
  -> Prog fs [(a, p)]                        -- ^ (final particle output, final particle context)
sis n_prts hdlParticle ρ_0 prog_0  = do
  -- | Create an initial population of particles and contexts
  let population = unzip $ replicate n_prts (prog_0, ρ_0)
  -- | Execute the population until termination
  pfilter hdlParticle prog_0 population

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
pfilter :: (Members [Resample p, Sampler] fs)
  => ParticleHandler p                                 -- ^ handler for running particles
  -> ProbProg es a                                          -- ^ initial probabilistic program
  -> ([ProbProg es a], [p])                               -- ^ input particles and corresponding contexts
  -> Prog fs [(a, p)]                          -- ^ final particle results and corresponding contexts
pfilter hdlParticle prog_0 (prts, ρs) = do
  -- | Run particles to next checkpoint and accumulate their contexts
  (prts', partialρs) <- call (mapAndUnzipM hdlParticle prts)
  ρs'                <- call (Accum ρs partialρs)
  -- | Check termination status of particles
  case foldVals prts' of
    -- | If all particles have finished, return their results and contexts
    Right vals  -> (`zip` ρs') <$> vals
    -- | Otherwise, pick the particles to continue with
    Left  _     -> call (Resample (prts', ρs') prog_0) >>= pfilter hdlParticle prog_0

{- | Check whether a list of programs have all terminated.
     If at least one program is unfinished, return all programs.
     If all programs have finished, return a single program that returns all results.
-}
foldVals :: [ProbProg es a] -> Either [ProbProg es a] (Prog es' [a])
foldVals (Val v : progs) = do
  vs <- foldVals progs
  pure ((v:) <$> vs)
foldVals []    = pure (Val [])
foldVals progs = Left progs
