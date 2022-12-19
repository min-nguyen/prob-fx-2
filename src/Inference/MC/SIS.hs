{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

{- An infrastructure for Sequential Importance (Re)Sampling.
-}

module Inference.MC.SIS where

import           Control.Monad ( mapAndUnzipM )
import           Data.Map (Map)
import qualified Data.Map as Map
import           Effects.Dist ( Addr, Observe (Observe), Sample, pattern ObsPrj )
import           Effects.Lift ( Lift, handleLift, lift, HasSampler )
import           LogP ( LogP, logMeanExp )
import           Prog ( Prog (..), weakenProg, Member, discharge, call, weaken, LastMember, Members )
import           Sampler
import           Util ( uncurry3 )
import           Model
import           Inference.MC.SIM as SIM
import           Data.Bifunctor


{- | The @Resample@ effect for resampling according to collection of particle contexts.
-}
data Resample s a where
  Resample
    -- | (particles, contexts)
    :: ([ProbProg a], [s])
    -- | initial probabilistic program
    -> ProbProg a
    -- | (resampled programs, resampled ss)
    -> Resample s ([ProbProg a], [s])
  Accum
    -- | (particles, contexts)
    :: [s]
    -- | initial probabilistic program
    -> [s]
    -- | (resampled programs, resampled ss)
    -> Resample s [s]

{- | A @ParticleHandler@  runs a particle to the next @Observe@ break point.
-}
type ParticleHandler s = forall a. ProbProg a -> Sampler (ProbProg a, s)

{- | A @ResampleHandler@ decides which of the current particles to continue execution with.
-}
type ResampleHandler fs s = forall a. Prog (Resample s : fs) a -> Prog fs a

{- | A top-level template for sequential importance sampling.
-}
sis :: HasSampler fs
  => Int                                                        -- ^ number of particles
  -> ParticleHandler s                                        -- ^ handler for running particles
  -> s
  -> ProbProg a                                                 -- ^ initial probabilistic program
  -> Prog (Resample s : fs) [(a, s)]                        -- ^ (final particle output, final particle context)
sis n_prts hdlParticle s_0 prog_0  = do
  -- | Create an initial population of particles and contexts
  let population = unzip $ replicate n_prts (prog_0, s_0)
  -- | Execute the population until termination
  loopSIS hdlParticle prog_0 population

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
loopSIS :: forall s fs a. HasSampler fs
  => ParticleHandler s                                 -- ^ handler for running particles
  -> ProbProg a                                          -- ^ initial probabilistic program
  -> ([ProbProg a], [s])                               -- ^ input particles and corresponding contexts
  -> Prog (Resample s  : fs) [(a, s)]                -- ^ final particle results and corresponding contexts
loopSIS hdlParticle prog_0 = loop where
  loop :: ([ProbProg a], [s]) -> Prog (Resample s : fs) [(a, s)]
  loop (prts, ss) = do
    -- | Run particles to next checkpoint and accumulate their contexts
    -- particles' <- mapM (lift . hdlParticle) particles
    (prts', partial_ss) <- mapAndUnzipM (lift . hdlParticle) prts
    ss' <- call (Accum ss partial_ss)
    -- | Check termination status of particles
    case foldVals prts' of
      -- | If all particles have finished, return their results and contexts
      Right vals  -> (`zip` ss') <$> vals
      -- | Otherwise, pick the particles to continue with
      Left  _     -> call (Resample (prts', ss') prog_0) >>= loop

{- | Check whether a list of programs have all terminated.
     If at least one program is unfinished, return all programs.
     If all programs have finished, return a single program that returns all results.
-}
foldVals :: [Prog es a] -> Either [Prog es a] (Prog es' [a])
foldVals (Val v : progs) = do
  vs <- foldVals progs
  pure ((v:) <$> vs)
foldVals []    = pure (Val [])
foldVals progs = Left progs
