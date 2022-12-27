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
sis :: HasSampler fs
  => Int                                                        -- ^ number of particles
  -> ParticleHandler p                                        -- ^ handler for running particles
  -> p
  -> ProbProg a                                                 -- ^ initial probabilistic program
  -> Prog (Resample p : fs) [(a, p)]                        -- ^ (final particle output, final particle context)
sis n_prts hdlParticle ρ_0 prog_0  = do
  -- | Create an initial population of particles and contexts
  let population = unzip $ replicate n_prts (prog_0, ρ_0)
  -- | Execute the population until termination
  loopSIS hdlParticle prog_0 population

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
loopSIS :: forall p fs a. HasSampler fs
  => ParticleHandler p                                 -- ^ handler for running particles
  -> ProbProg a                                          -- ^ initial probabilistic program
  -> ([ProbProg a], [p])                               -- ^ input particles and corresponding contexts
  -> Prog (Resample p  : fs) [(a, p)]                -- ^ final particle results and corresponding contexts
loopSIS hdlParticle prog_0 = loop where
  loop :: ([ProbProg a], [p]) -> Prog (Resample p : fs) [(a, p)]
  loop (prts, ρs) = do
    -- | Run particles to next checkpoint and accumulate their contexts
    -- particles' <- mapM (lift . hdlParticle) particles
    (prts', partialρs) <- mapAndUnzipM (lift . hdlParticle) prts
    ρs' <- call (Accum ρs partialρs)
    -- | Check termination status of particles
    case foldVals prts' of
      -- | If all particles have finished, return their results and contexts
      Right vals  -> (`zip` ρs') <$> vals
      -- | Otherwise, pick the particles to continue with
      Left  _     -> call (Resample (prts', ρs') prog_0) >>= loop

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
