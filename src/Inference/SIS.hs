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

{- | A @ParticleResampler@ decides which of the current particles and contexts to continue execution with.
-}
type ParticleResampler es a
  -- | the accumulated contexts of particles of the previous step
  =  [ParticleCtx]
  -- | the incremental contexts of particles since the previous step
  -> [ParticleCtx]
  -- | the collection of current particles that produced the incremental contexts
  -> [Prog (NonDet : es) a]
  -- | the resampled particles and corresponding contexts
  -> Prog es [(Prog (NonDet : es) a, ParticleCtx)]

{- | A @ParticleHandler@ runs a collection of particles to the next step in the computation.
-}
type ParticleHandler es a
  -- | a list of particles
  = [Prog (NonDet : es) a]
  -- | a list of particles at the next step and their corresponding contexts
  -> Prog es [(Prog (NonDet : es) a, ParticleCtx)]

{- | A @ParticleCtx@ contains data about the execution of a particular particle.
-}
data ParticleCtx = ParticleCtx {
    particleLogProb  :: LogP    -- ^ associated log-probability
  , particleObsAddrs :: [Addr]  -- ^ addresses of @Observe@ operations encountered so far
  }

-- | Initialise the context for a particle
emptyParticleCtx :: ParticleCtx
emptyParticleCtx = ParticleCtx 0 []

-- | Merge the incremental contexts with the previously accumulated contexts
accumParticleCtx :: [ParticleCtx] -- ^ previously acccumulated context
                 -> [ParticleCtx] -- ^ incremental context
                 -> [ParticleCtx]
accumParticleCtx ctxs ctxs' =
  --  Compute normalised accumulated log weights
  let logprobs = let logZ = logMeanExp (map particleLogProb ctxs)
                 in  map ((+ logZ) . particleLogProb) ctxs'
  --  Update the Observe operations encountered
      obsaddrs = zipWith (++) (map particleObsAddrs ctxs') (map particleObsAddrs ctxs)
  in  zipWith ParticleCtx logprobs obsaddrs

{- | A top-level template for sequential importance sampling.
-}
sis :: forall a es.
     Int                                                                    -- ^ num of particles
  -> ParticleHandler (Observe : Sample : Lift Sampler : '[]) a              -- ^ handler of particles
  -> ParticleResampler (Observe : Sample : Lift Sampler : '[])  a           -- ^ particle resampler
  -> (forall b es . Prog (Observe : es) b -> Prog es b)                     -- ^ observe handler
  -> (forall b. Prog '[Sample, Lift Sampler] b -> Prog '[Lift Sampler] b)   -- ^ sample handler
  -> Prog [Observe, Sample, Lift Sampler] a                                 -- ^ model
  -> Sampler [(a, ParticleCtx)]
sis n_particles  particleHdlr particleResamplr obsHdlr sampHdlr prog = do
  -- Initialise a population of particles and contexts
  let population :: [(Prog '[NonDet, Observe, Sample, Lift Sampler] a, ParticleCtx)]
      population  = replicate n_particles (weakenNonDet prog, emptyParticleCtx)
  -- Execute the population until termination
  (handleLift . sampHdlr . obsHdlr) (loopSIS particleHdlr particleResamplr population)

{- | Incrementally execute and resample a population of particles through the course of the program.
-}
loopSIS ::
     ParticleHandler es a
  -> ParticleResampler es  a
  -> [(Prog (NonDet : es) a, ParticleCtx)]   -- ^ particles and corresponding contexts
  -> Prog es [(a, ParticleCtx)]
loopSIS particleHdlr particleResamplr  population = do
  -- Separate population into particles (programs) and their contexts
  let (particles, ctxs) = unzip population
  -- Run particles to next checkpoint
  (particles', ctxs') <- unzip <$> particleHdlr particles
  case accumNonDet particles' of
    -- If all programs have finished, return their results along with their accumulated contexts
    Right vals  -> (`zip` accumParticleCtx ctxs ctxs') <$> vals
    -- Otherwise, pick the programs to continue with
    Left  _     -> do resampled_population <- particleResamplr ctxs ctxs' particles'
                      loopSIS particleHdlr particleResamplr resampled_population


