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
type ParticleHandler es p = forall a. p -> Model es a -> Sampler (Model es a, p)

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
pfilter :: forall fs es a p. (Members [Resample p, Sampler] fs)
  => Int
  -> ParticleHandler es p                                 -- ^ handler for running particles
  -> p
  -> Model es a                               -- ^ input particles and corresponding contexts
  -> Comp fs [(a, p)]                          -- ^ final particle results and corresponding contexts
pfilter n exec w model  = do
  let pfStep :: [(Model es a, p)] -> Comp fs [(a, p)]
      pfStep wprts = do
        -- | Run particles to next checkpoint and accumulate their contexts
        wprts' <- call (mapM (\(prt, w) -> exec w prt) wprts)
        -- ρs'   <- call (Accum ρs partialρs)
        -- | Check termination status of particles
        case collapse wprts' of
          -- | If all particles have finished, return their results and contexts
          Just vals  -> Val vals
          -- | Otherwise, pick the particles to continue with
          Nothing    -> call (Resample (unzip wprts')) >>= pfStep
  pfStep (replicate n (model, w))

{- | Check whether a list of programs have all terminated.
     If at least one program is unfinished, return all programs.
     If all programs have finished, return a single program that returns all results.
-}
collapse :: [(Model es a, p)] -> Maybe [(a, p)]
collapse ((Val v, p) : progs) = collapse progs >>= return . ((v, p):)
collapse []    = Just []
collapse progs = Nothing
