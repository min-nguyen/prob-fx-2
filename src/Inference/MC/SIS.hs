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
data Resample w a where
  Resample
    -- | (particles, contexts)
    :: [(Model es b, w)]
    -- | (resampled programs, resampled ss)
    -> Resample w [(Model es b, w)]

{- | A @ModelStep@  runs a particle to the next @Observe@ break point.
-}
type ModelStep es w a = (Model es a, w) -> Sampler (Model es a, w)

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
pfilter :: forall fs es a w. (Members [Resample w, Sampler] fs)
  => Int
  -> w
  -> ModelStep es w a                                 -- ^ handler for running particles
  -> Model es a                               -- ^ input particles and corresponding contexts
  -> Comp fs [(a, w)]                          -- ^ final particle results and corresponding contexts
pfilter n w exec model  = do
  let pfStep :: [(Model es a, w)] -> Comp fs [(a, w)]
      pfStep pws = do
        -- | Run particles to next checkpoint and accumulate their contexts
        pws' <- call (mapM exec pws)
        -- ρs'   <- call (Accum ρs partialρs)
        -- | Check termination status of particles
        case done pws' of
          -- | If all particles have finished, return their results and contexts
          Just vals  -> Val vals
          -- | Otherwise, pick the particles to continue with
          Nothing    -> call (Resample pws') >>= pfStep
  pfStep (replicate n (model, w))

{- | Check whether a list of programs have all terminated.
     If at least one program is unfinished, return all programs.
     If all programs have finished, return a single program that returns all results.
-}
done :: [(Model es a, w)] -> Maybe [(a, w)]
done ((Val v, w) : progs) = done progs >>= Just . ((v, w):)
done []    = Just []
done progs = Nothing
