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
    :: ([ProbProg a], [p])
    -- | initial probabilistic program
    -> ProbProg a
    -- | (resampled programs, resampled ss)
    -> Resample p ([ProbProg a], [p])
  Accum
    -- | (particles, contexts)
    :: [p]
    -- | initial probabilistic program
    -> [p]
    -- | (resampled programs, resampled ss)
    -> Resample p [p]

{- | A @ParticleHandler@  runs a particle to the next @Observe@ break point.
-}
type ParticleHandler p = forall a. ProbProg a -> Sampler (ProbProg a, p)

{- | A @ResampleHandler@ decides which of the current particles to continue execution with.
-}
type ResampleHandler fs p = forall a. Prog (Resample p : fs) a -> Prog fs a

{- | A top-level template for sequential importance sampling.
-}
sis :: (Members [Resample p, Sampler] fs)
  => Int                                                        -- ^ number of particles
  -> ParticleHandler p                                        -- ^ handler for running particles
  -> p
  -> ProbProg a                                                 -- ^ initial probabilistic program
  -> Prog fs [(a, p)]                        -- ^ (final particle output, final particle context)
sis n_prts hdlParticle ??_0 prog_0  = do
  -- | Create an initial population of particles and contexts
  let population = unzip $ replicate n_prts (prog_0, ??_0)
  -- | Execute the population until termination
  pfilter hdlParticle prog_0 population

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
pfilter :: (Members [Resample p, Sampler] fs)
  => ParticleHandler p                                 -- ^ handler for running particles
  -> ProbProg a                                          -- ^ initial probabilistic program
  -> ([ProbProg a], [p])                               -- ^ input particles and corresponding contexts
  -> Prog fs [(a, p)]                          -- ^ final particle results and corresponding contexts
pfilter hdlParticle prog_0 (prts, ??s) = do
  -- | Run particles to next checkpoint and accumulate their contexts
  (prts', partial??s) <- call (mapAndUnzipM hdlParticle prts)
  ??s'                <- call (Accum ??s partial??s)
  -- | Check termination status of particles
  case foldVals prts' of
    -- | If all particles have finished, return their results and contexts
    Right vals  -> (`zip` ??s') <$> vals
    -- | Otherwise, pick the particles to continue with
    Left  _     -> call (Resample (prts', ??s') prog_0) >>= pfilter hdlParticle prog_0

{- | Check whether a list of programs have all terminated.
     If at least one program is unfinished, return all programs.
     If all programs have finished, return a single program that returns all results.
-}
foldVals :: [ProbProg a] -> Either [ProbProg a] (Prog es [a])
foldVals (Val v : progs) = do
  vs <- foldVals progs
  pure ((v:) <$> vs)
foldVals []    = pure (Val [])
foldVals progs = Left progs
