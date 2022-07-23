{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

{- An infrastructure for Sequential Importance Sampling (particle filter).
-}

module Inference.SIS where


import Data.Map (Map)
import qualified Data.Map as Map
import Effects.Dist ( Addr, Observe, Sample )
import Effects.Lift ( Lift, handleLift )
import Effects.NonDet ( accumNonDet, NonDet )
import LogP ( LogP, logMeanExp )
import Prog ( Prog, weakenProg )
import Sampler ( Sampler )

{- | A @Resampler@ decides which of the current particles and contexts to continue execution with.
-}
type Resampler ctx es a
  -- | the accumulated contexts of particles of the previous step
  =  [ctx]
  -- | the incremental contexts of particles since the previous step
  -> [ctx]
  -- | the collection of current particles
  -> [Prog (NonDet : es) a]
  -- | the resampled particles and corresponding contexts
  -> Prog es ([Prog (NonDet : es) a], [ctx])

{- | A @ParticleHandler@ runs a collection of particles to the next step in the computation.
-}
type ParticleHandler ctx es a
  -- | a list of particles
  = [Prog (NonDet : es) a]
  -- | a list of particles at the next step and their corresponding contexts
  -> Prog es [(Prog (NonDet : es) a, ctx)]

{- | The class @Accum@ is similar in spirit to @Monoid@, and defines how the contexts of particles
     should be accumulated.
-}
class Accum ctx where
  -- | Initialise the contexts for n particles
  aempty :: Int -> [ctx]
  -- | Merge the incremental contexts with the previously accumulated contexts
  accum  :: [ctx] -> [ctx] -> [ctx]

instance {-# OVERLAPPING #-} Accum LogP where
  aempty n = replicate n 0
  accum logps_1sub0 logps_0  =
    let logZ = logMeanExp logps_0
    in  map (+ logZ) logps_1sub0

instance Ord k => Accum (Map k a) where
  aempty n = replicate n  Map.empty
  accum  = zipWith Map.union

instance {-# OVERLAPPING #-} Accum [Addr] where
  aempty n = replicate n []
  accum    = zipWith (++)

instance (Accum a, Accum b) => Accum (a, b) where
  aempty n = zip (aempty n) (aempty n)
  accum xys xys' = let (x, y)    = unzip xys
                       (x', y') = unzip xys'
                   in  zip (accum x x') (accum y y')

instance (Accum a, Accum b, Accum c) => Accum (a, b, c) where
  aempty n = zip3 (aempty n) (aempty n) (aempty n)
  accum xyzs xyzs' = let (x, y, z)    = unzip3 xyzs
                         (x', y', z') = unzip3 xyzs'
                     in  zip3 (accum x x') (accum y y') (accum z z')

sis :: forall a env ctx es.
     (Accum ctx, Show ctx, Show a)
  => Int                                                                    -- num of particles
  -> Resampler       ctx (Observe : Sample : Lift Sampler : '[])  a         -- resampler
  -> ParticleHandler ctx (Observe : Sample : Lift Sampler : '[]) a          -- handler of particles
  -> (forall es b. Prog (Observe : es) b -> Prog es b)                      -- observe handler
  -> (forall b. Prog '[Sample, Lift Sampler] b -> Prog '[Lift Sampler] b)   -- sample handler
  -> Prog [Observe, Sample, Lift Sampler] a                                 -- model
  -> Sampler [(a, ctx)]
sis n_particles resampler pophdl handleObs handleSamp prog = do
  let particles_0   = replicate n_particles (weakenProg prog)
      ctxs_0        = aempty n_particles
  (handleLift . handleSamp . handleObs) (loopSIS n_particles resampler pophdl (particles_0, ctxs_0))

loopSIS :: (Show a, Show ctx, Accum ctx)
  => Int
  -> Resampler       ctx es  a
  -> ParticleHandler ctx es a
  -> ([Prog (NonDet : es) a], [ctx])   -- Particles and corresponding contexts
  -> Prog es [(a, ctx)]
loopSIS n_particles resampler populationHandler (particles_0, ctxs_0)  = do
  -- Run particles to next checkpoint
  (particles_1, ctxs_1) <- unzip <$> populationHandler particles_0
  case accumNonDet particles_1 of
  -- if all programs have finished, return with accumulated context
    Right vals  -> do let ctxs' =  accum ctxs_1 ctxs_0
                      (`zip` ctxs') <$> vals
  -- otherwise, pick programs to continue with
    Left  particles_1 -> do (resampledParticles_1, resampledCtxs_1) <- resampler ctxs_0 ctxs_1 particles_1
                            loopSIS n_particles resampler populationHandler (resampledParticles_1, resampledCtxs_1)


