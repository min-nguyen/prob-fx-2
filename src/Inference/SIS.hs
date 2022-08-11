{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

{- An infrastructure for Sequential Importance Sampling (particle filter).
-}

module Inference.SIS where

import Data.Map (Map)
import qualified Data.Map as Map
import Effects.Dist ( Addr, Observe (Observe), Sample, pattern ObsPrj )
import Effects.Lift ( Lift, handleLift )
import Effects.NonDet ( foldVals, weakenNonDet, NonDet, asum, branchWeaken, handleNonDet )
import Effects.Resample
import LogP ( LogP, logMeanExp )
import Prog ( Prog (..), weakenProg, Member, discharge, call, weaken, LastMember )
import Sampler ( Sampler )
import Util ( uncurry3 )
import Inference.SIM as SIM
import Data.Bifunctor

{- | A @ParticleCtx@ contains data about the execution of a particular particle.
-}
class ParticleCtx ctx where
  -- | For each particle, accumulate its incremental context and its previous context
  paccum  :: [ctx] -- ^ previously acccumulated context
          -> [ctx] -- ^ incremental context
          -> [ctx]
  -- | Initialise the contexts for n particles
  pempty :: ctx

{- | A @ParticleResampler@ decides which of the current particles and contexts to continue execution with.
-}
type ParticleHandler ctx
  = forall es a. Member Observe es
  -- | particles as a single program
  => Prog es a
  -- | (particles suspended at the next step, corresponding contexts)
  -> Prog es (Prog es a, ctx)

{- | A @ParticleResampler@ decides which of the current particles and contexts to continue execution with.
-}
type ParticleResampler ctx
  =  forall es a. LastMember (Lift Sampler) es
  => Prog (Resample ctx : es) a
  -> Prog es a

{- | A top-level template for sequential importance sampling.
-}
sis :: forall ctx a es. ParticleCtx ctx
  => Int                                                                    -- ^ num of particles
  -> ParticleHandler ctx                                                    -- ^ handler of particles
  -> ParticleResampler ctx
  -> (forall b. Prog '[Sample, Lift Sampler] b -> Prog '[Lift Sampler] b)   -- ^ sample handler
  -> Prog [Resample ctx, Observe, Sample, Lift Sampler] a                   -- ^ model
  -> Sampler [(a, ctx)]
sis n_particles particleHdlr particleResampler sampHdlr prog = do

  let population_0 = unzip $ replicate n_particles (weakenNonDet prog, pempty)

  -- Execute the population until termination
  (handleLift
    . sampHdlr
    . SIM.handleObs
    . particleResampler) -- A dummy handler that removes the Observe effect
      (loopSIS particleHdlr  population_0)

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
loopSIS
  :: (ParticleCtx ctx, Member Observe es, Member (Resample ctx) es, LastMember (Lift Sampler) es)
  => ParticleHandler   ctx
  -> ([Prog (NonDet : es) a], [ctx])  -- ^ particles and corresponding contexts
  -> Prog es [(a, ctx)]
loopSIS particleHdlr (particles, ctxs) = do
  -- Run particles to next checkpoint
  (particles', ctxs') <- second (ctxs `paccum`) . unzip <$> (handleNonDet . particleHdlr . asum) particles
  case foldVals particles' of
    -- If all programs have finished, return their results along with their accumulated contexts
    Right vals  -> (`zip` ctxs') <$> vals
    -- Otherwise, pick the programs to continue with
    Left  _     -> call (Resample (particles', ctxs')) >>= loopSIS particleHdlr