{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- An infrastructure for Sequential Importance Sampling (particle filter).
-}

module Inference.SIS where

import Data.Map (Map)
import qualified Data.Map as Map
import Effects.Dist ( Addr, Observe, Sample )
import Effects.Lift ( Lift, handleLift )
import Effects.NonDet ( accumNonDet, weakenNonDet, NonDet )
import LogP ( LogP, logMeanExp )
import Prog ( Prog, weakenProg )
import Sampler ( Sampler )
import Util ( uncurry3 )

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
type ParticleResampler ctx es a
  -- | the accumulated contexts of particles of the previous step
  =  [ctx]
  -- | the incremental contexts of particles since the previous step
  -> [ctx]
  -- | the collection of current particles that produced the incremental contexts
  -> [Prog (NonDet : es) a]
  -- | the resampled particles and corresponding contexts
  -> Prog es [(Prog (NonDet : es) a, ctx)]

{- | A @ParticleHandler@ runs a collection of particles to the next step in the computation.
-}
type ParticleHandler ctx es a
  -- | a list of particles
  = [Prog (NonDet : es) a]
  -- | a list of particles at the next step and their corresponding contexts
  -> Prog es [(Prog (NonDet : es) a, ctx)]

{- | A top-level template for sequential importance sampling.
-}
sis :: forall ctx a es. ParticleCtx ctx
  => Int                                                                    -- ^ num of particles
  -> ParticleHandler ctx (Observe : Sample : Lift Sampler : '[]) a          -- ^ handler of particles
  -> ParticleResampler ctx (Observe : Sample : Lift Sampler : '[])  a       -- ^ particle resampler
  -> (forall b es. Prog (Observe : es) b -> Prog es b)                      -- ^ observe handler
  -> (forall b. Prog '[Sample, Lift Sampler] b -> Prog '[Lift Sampler] b)   -- ^ sample handler
  -> Prog [Observe, Sample, Lift Sampler] a                                 -- ^ model
  -> Sampler [(a, ctx)]
sis n_particles  particleHdlr particleResamplr obsHdlr sampHdlr prog = do
  -- Initialise a population of particles and contexts
  let population :: [(Prog '[NonDet, Observe, Sample, Lift Sampler] a, ctx)]
      population  = replicate n_particles (weakenNonDet prog, pempty)
  -- Execute the population until termination
  (handleLift . sampHdlr . obsHdlr) (loopSIS particleHdlr particleResamplr population)

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
loopSIS :: ParticleCtx ctx =>
     ParticleHandler ctx es a
  -> ParticleResampler ctx es  a
  -> [(Prog (NonDet : es) a, ctx)]   -- ^ particles and corresponding contexts
  -> Prog es [(a, ctx)]
loopSIS particleHdlr particleResamplr  population = do
  -- Separate population into particles (programs) and their contexts
  let (particles, ctxs) = unzip population
  -- Run particles to next checkpoint
  (particles', ctxs') <- unzip <$> particleHdlr particles
  case accumNonDet particles' of
    -- If all programs have finished, return their results along with their accumulated contexts
    Right vals  -> (`zip` paccum ctxs ctxs') <$> vals
    -- Otherwise, pick the programs to continue with
    Left  _     -> do resampled_population <- particleResamplr ctxs ctxs' particles'
                      loopSIS particleHdlr particleResamplr resampled_population


