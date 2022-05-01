{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Inference.SIS where
-- import Data.Extensible hiding (Member)
import qualified Data.Map as Map
import Data.Maybe
import Data.Bifunctor
import Data.Map (Map)
import ModelEnv
import Control.Monad
import Control.Applicative
import Effects.Dist
import Freer
import Effects.Lift
import Model
import Effects.NonDet
import Sampler
import Effects.ObsReader
import Effects.State
import Trace
import Sampler
import Effects.Writer
import qualified OpenSum as OpenSum
import OpenSum (OpenSum)
import Util
import Language.Haskell.TH.Lens (_Overlappable)

{- Takes previous contexts of particles, the new contexts of particles since previous execution, and the current particles,
  , and decides which particles and contexts to continue with. -}
type Resampler       ctx es a = [ctx] -> [ctx] -> [Prog (NonDet : es) a] -> Prog es ([Prog (NonDet : es) a], [ctx])
{- Takes list of particles and runs them to the next step, producing a list of particles and their yielded contexts -}
type ParticleHandler ctx es a = [Prog (NonDet : es) a] -> Prog es [(Prog (NonDet : es) a, ctx)]

class Accum ctx where
  -- For each particle, accumulate its incremental context, and its previous context
  accum  :: [ctx] -> [ctx] -> [ctx]
  -- Initialise the contexts for n particles
  aempty :: Int -> [ctx]

newtype LogP = LogP { logP :: Double } deriving (Show, Num, Eq, Ord, Fractional)

logMeanExp :: [LogP] -> LogP
logMeanExp logps =
  let logws = map logP logps
      c = maximum logws
  in  if isInfinite c -- to avoid "-Infinity - (-Infinity)"
      then (-1/0)
      else LogP $ c + log ((1.0/fromIntegral (length logws)) * sum (map (\logw -> exp (logw - c)) logws))

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
  accum addrs_1sub0 addrs_0 = zipWith (++) addrs_1sub0 addrs_0

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
sis n_particles resampler pophdl runObserve runSample prog = do
  let particles_0   = replicate n_particles (weaken' prog)
      ctxs_0        = aempty n_particles
  (runLift . runSample . runObserve) (loopSIS n_particles resampler pophdl (particles_0, ctxs_0))

loopSIS :: (Show a, Show ctx, Accum ctx)
  => Int
  -> Resampler       ctx es  a
  -> ParticleHandler ctx es a
  -> ([Prog (NonDet : es) a], [ctx])   -- Particles and corresponding contexts
  -> Prog es [(a, ctx)]
loopSIS n_particles resampler populationHandler (particles_0, ctxs_0)  = do
  -- Run particles to next checkpoint
  (particles_1, ctxs_1) <- unzip <$> populationHandler particles_0
  case foldVals particles_1 of
  -- if all programs have finished, return with accumulated context
    Right vals  -> do let ctxs' =  accum ctxs_1 ctxs_0
                      (`zip` ctxs') <$> vals
  -- otherwise, pick programs to continue with
    Left  particles_1 -> do (resampledParticles_1, resampledCtxs_1) <- resampler ctxs_0 ctxs_1 particles_1
                            loopSIS n_particles resampler populationHandler (resampledParticles_1, resampledCtxs_1)


