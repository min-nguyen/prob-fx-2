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
data Resample s a where
  Resample
    -- | (particles, contexts)
    :: ([Model es b], [s])
    -- | (resampled programs, resampled ss)
    -> Resample s [(Model es b, s)]

{- | A @ParticleHandler@  runs a particle to the next @Observe@ break point.
-}
type ParticleHandler es s a = s -> Model es a -> Sampler (Model es a, s)

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
pfilter :: forall fs es a s. (Members [Resample s, Sampler] fs)
  => Int
  -> s
  -> ParticleHandler es s a                                 -- ^ handler for running particles
  -> Model es a                               -- ^ input particles and corresponding contexts
  -> Comp fs [(a, s)]                          -- ^ final particle results and corresponding contexts
pfilter n w exec  model  = do
  let pfStep :: [(Model es a, s)] -> Comp fs [(a, s)]
      pfStep wprts = do
        -- | Run particles to next checkpoint and accumulate their contexts
        wprts' <- call (mapM (\(prt, w) -> exec w prt) wprts)
        -- ρs'   <- call (Accum ρs partialρs)
        -- | Check termination status of particles
        case done wprts' of
          -- | If all particles have finished, return their results and contexts
          Just vals  -> Val vals
          -- | Otherwise, pick the particles to continue with
          Nothing    -> call (Resample (unzip wprts')) >>= pfStep
  pfStep (replicate n (model, w))

{- | Check whether a list of programs have all terminated.
     If at least one program is unfinished, return all programs.
     If all programs have finished, return a single program that returns all results.
-}
done :: [(Model es a, s)] -> Maybe [(a, s)]
done ((Val v, w) : progs) = done progs >>= Just . ((v, w):)
done []    = Just []
done progs = Nothing
