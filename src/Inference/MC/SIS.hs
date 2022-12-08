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
import           Effects.Lift ( Lift, handleLift, lift )
import           LogP ( LogP, logMeanExp )
import           Prog ( Prog (..), weakenProg, Member, discharge, call, weaken, LastMember, Members )
import           Sampler
import           Util ( uncurry3 )
import           Model
import           Inference.MC.SIM as SIM
import           Data.Bifunctor


{- | The @Resample@ effect for resampling according to collection of particle contexts.
-}
data Resample ctx a where
  Resample
    -- | (particles, contexts)
    :: ([ProbProg a], [ctx])
    -- | initial probabilistic program
    -> ProbProg a
    -- | (resampled programs, resampled ctxs)
    -> Resample ctx ([ProbProg a], [ctx])
  Accum
    -- | (particles, contexts)
    :: [ctx]
    -- | initial probabilistic program
    -> [ctx]
    -- | (resampled programs, resampled ctxs)
    -> Resample ctx [ctx]

{- | A @ParticleHandler@  runs a particle to the next @Observe@ break point.
-}
type ParticleHandler ctx = forall a. ProbProg a -> Sampler (ProbProg a, ctx)

{- | A @ResampleHandler@ decides which of the current particles to continue execution with.
-}
type ResampleHandler fs ctx = forall a. Prog (Resample ctx : fs) a -> Prog fs a

{- | A top-level template for sequential importance sampling.
-}
sis :: (LastMember (Lift Sampler) fs)
  => Int                                                        -- ^ number of particles
  -> ParticleHandler ctx                                        -- ^ handler for running particles
  -> ctx
  -> ProbProg a                                                 -- ^ initial probabilistic program
  -> Prog (Resample ctx : fs) [(a, ctx)]                        -- ^ (final particle output, final particle context)
sis n_prts hdlParticle ctx_0 prog_0  = do
  -- | Create an initial population of particles and contexts
  let population = unzip $ replicate n_prts (prog_0, ctx_0)
  -- | Execute the population until termination
  loopSIS hdlParticle prog_0 population

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
loopSIS :: forall ctx fs a. (LastMember (Lift Sampler) fs)
  => ParticleHandler ctx                                 -- ^ handler for running particles
  -> ProbProg a                                          -- ^ initial probabilistic program
  -> ([ProbProg a], [ctx])                               -- ^ input particles and corresponding contexts
  -> Prog (Resample ctx  : fs) [(a, ctx)]                -- ^ final particle results and corresponding contexts
loopSIS hdlParticle prog_0 = loop where
  loop :: ([ProbProg a], [ctx]) -> Prog (Resample ctx : fs) [(a, ctx)]
  loop (prts, ctxs) = do
    -- | Run particles to next checkpoint and accumulate their contexts
    -- particles' <- mapM (lift . hdlParticle) particles
    (prts', partial_ctxs) <- mapAndUnzipM (lift . hdlParticle) prts
    ctxs' <- call (Accum ctxs partial_ctxs)
    -- | Check termination status of particles
    case foldVals prts' of
      -- | If all particles have finished, return their results and contexts
      Right vals  -> (`zip` ctxs') <$> vals
      -- | Otherwise, pick the particles to continue with
      Left  _     -> call (Resample (prts', ctxs') prog_0) >>= loop

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
