{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

{- An infrastructure for Sequential Importance Sampling (particle filter).
-}

module Inference.SIS where

import Data.Map (Map)
import qualified Data.Map as Map
import Effects.Dist ( Addr, Observe (Observe), Sample, pattern ObsPrj )
import Effects.Lift ( Lift, handleLift )
import Effects.NonDet ( accumNonDet, weakenNonDet, NonDet, asum, branchWeaken )
import LogP ( LogP, logMeanExp )
import Prog ( Prog (..), weakenProg, Member, discharge, call, weaken )
import Sampler ( Sampler )
import Util ( uncurry3 )
import Inference.SIM as SIM

{- | A @ParticleCtx@ contains data about the execution of a particular particle.
-}
class ParticleCtx ctx where
  -- | For each particle, accumulate its incremental context and its previous context
  paccum  :: [ctx] -- ^ previously acccumulated context
          -> [ctx] -- ^ incremental context
          -> [ctx]
  -- | Initialise the contexts for n particles
  pempty :: ctx

type ParticleHandler ctx es a
  -- | particles as a single program
  =  Prog (NonDet :  es) a
  -- | (particles suspended at the next step, corresponding contexts)
  -> Prog es ([Prog (NonDet : es) a], [ctx])

{- | A @ParticleResampler@ decides which of the current particles and contexts to continue execution with.
-}
type ParticleResampler ctx es a
  -- | the accumulated contexts of particles of the previous step
  =  [ctx]
  -- | (the collection of current particles that produced the incremental contexts, the incremental contexts of particles since the previous step)
  -> ([Prog (NonDet : es) a], [ctx])
  -- | (resampled particles, resampled particle contexts)
  -> Prog es (Prog (NonDet : es) a, [ctx])

{- | A top-level template for sequential importance sampling.
-}
sis :: forall ctx a es. ParticleCtx ctx
  => Int                                                                    -- ^ num of particles
  -> ParticleHandler ctx (Observe : Sample : Lift Sampler : '[]) a          -- ^ handler of particles
  -> ParticleResampler ctx (Observe : Sample : Lift Sampler : '[])  a       -- ^ particle resampler
  -> (forall b. Prog '[Sample, Lift Sampler] b -> Prog '[Lift Sampler] b)   -- ^ sample handler
  -> Prog [Observe, Sample, Lift Sampler] a                                 -- ^ model
  -> Sampler [(a, ctx)]
sis n_particles particleHdlr particleResamplr sampHdlr prog = do

  let (particles_0, ctxs_0) = (branchWeaken n_particles prog, replicate n_particles pempty)

  -- Execute the population until termination
  (handleLift
    . sampHdlr
    . SIM.handleObs) -- A dummy handler that removes the Observe effect
      (loopSIS particleHdlr particleResamplr (particles_0, ctxs_0))

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
loopSIS :: ParticleCtx ctx =>
     ParticleHandler ctx es a
  -> ParticleResampler ctx es  a
  -> (Prog (NonDet : es) a, [ctx])   -- ^ particles and corresponding contexts
  -> Prog es [(a, ctx)]
loopSIS particleHdlr particleResamplr (particles, ctxs) = do
  -- Run particles to next checkpoint
  (particles', ctxs') <- particleHdlr particles
  case accumNonDet particles' of
    -- If all programs have finished, return their results along with their accumulated contexts
    Right vals  -> (`zip` paccum ctxs ctxs') <$> vals
    -- Otherwise, pick the programs to continue with
    Left  _     -> particleResamplr ctxs (particles', ctxs')
                     >>= loopSIS particleHdlr particleResamplr

