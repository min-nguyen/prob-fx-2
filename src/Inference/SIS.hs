{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
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

{- | The class @Accum@ defines how the contexts of particles should be accumulated.
-}
class Accum ctx where
  -- | Initialise the contexts for n particles
  aempty :: Int -> [ctx]
  -- | Merge the incremental contexts with the previously accumulated contexts
  accum
    :: [ctx] -- ^ previously acccumulated contexts
    -> [ctx] -- ^ incremental context
    -> [ctx]

instance {-# OVERLAPPING #-} Accum LogP where
  aempty n = replicate n 0
  accum  logps_0 logps_1sub0 =
    let logZ = logMeanExp logps_0
    in  map (+ logZ) logps_1sub0

sis :: forall ctx a es. (Accum ctx, Show ctx, Show a)
  => Int                                                                    -- ^ num of particles
  -> ParticleResampler ctx (Observe : Sample : Lift Sampler : '[])  a       -- ^ particle resampler
  -> ParticleHandler ctx (Observe : Sample : Lift Sampler : '[]) a          -- ^ handler of particles
  -> (forall es b. Prog (Observe : es) b -> Prog es b)                      -- ^ observe handler
  -> (forall b. Prog '[Sample, Lift Sampler] b -> Prog '[Lift Sampler] b)   -- ^ sample handler
  -> Prog [Observe, Sample, Lift Sampler] a                                 -- ^ model
  -> Sampler [(a, ctx)]
sis n_particles resampler particleHdlr obsHdlr sampHdlr prog = do
  -- Initialise a population of particles and contexts
  let population :: [(Prog '[NonDet, Observe, Sample, Lift Sampler] a, ctx)]
      population  = zip (replicate n_particles (weakenNonDet prog)) (aempty n_particles)
  -- Execute the population until termination
  (handleLift . sampHdlr . obsHdlr) (loopSIS resampler particleHdlr population)

loopSIS :: (Show a, Show ctx, Accum ctx)
  => ParticleResampler ctx es  a
  -> ParticleHandler ctx es a
  -> [(Prog (NonDet : es) a, ctx)]   -- ^ particles and corresponding contexts
  -> Prog es [(a, ctx)]
loopSIS particleResamplr particleHdlr population = do
  -- Separate population into particles (programs) and their contexts
  let (particles, ctxs) = unzip population
  -- Run particles to next checkpoint
  (particles', ctxs') <- unzip <$> particleHdlr particles
  case accumNonDet particles' of
    -- If all programs have finished, return with accumulated context
    Right vals  -> (`zip` accum ctxs ctxs') <$> vals
    -- Otherwise, pick programs to continue with
    Left  _     -> do resampled_population <- particleResamplr ctxs ctxs' particles'
                      loopSIS particleResamplr particleHdlr resampled_population


