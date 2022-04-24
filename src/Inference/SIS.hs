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
import STrace
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

newtype LogP = LogP { logP :: Double } deriving (Show, Num)

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
     (Accum ctx, Show ctx, FromSTrace env, Show a)
  => Int
  -> Resampler       ctx (Observe : Sample : Lift Sampler : '[])  a
  -> ParticleHandler ctx (Observe : Sample : Lift Sampler : '[]) a
  -> Model env [ObsReader env, Dist, Lift Sampler] a
  -> ModelEnv env
  -> Sampler [(a, ctx)]
sis n_particles resampler pophdl model env = do
  let prog_0  = (runDist . runObsReader env) (runModel model)
      progs   = replicate n_particles (weaken' prog_0)
      ctxs    = aempty n_particles
  printS $ show ctxs
  (runLift . runSample . runObserve) (loopSIS n_particles resampler pophdl (progs, ctxs))

loopSIS :: (Show a, Show ctx, Accum ctx)
  => Int
  -> Resampler       ctx es  a
  -> ParticleHandler ctx es a
  -> ([Prog (NonDet : es) a], [ctx])   -- Particles and corresponding contexts
  -> Prog es [(a, ctx)]
loopSIS n_particles resampler populationHandler (progs_0, ctxs_0)  = do
  -- Run particles to next checkpoint
  (progs_1, ctxs_1) <- unzip <$> populationHandler progs_0
  case foldVals progs_1 of
  -- if all programs have finished, return with accumulated context
    Right vals  -> do let ctxs' =  accum ctxs_1 ctxs_0
                      (`zip` ctxs') <$> vals
  -- otherwise, pick programs to continue with
    Left  progs -> do (progs', ctxs') <- resampler ctxs_0 ctxs_1 progs_1
                      loopSIS n_particles resampler populationHandler (progs', ctxs')

runSample :: Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
runSample = loop
  where
  loop :: Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
  loop (Val x) = return x
  loop (Op u k) =
    case  u of
      SampPatt d α ->
        lift (sample d) >>= \x -> loop (k x)
      PrintPatt s  ->
        lift (liftS (putStrLn s)) >>= loop . k
      DecompLeft u' ->
        Op u' (loop . k)

runObserve :: Prog (Observe : es) a -> Prog es a
runObserve  (Val x) = return x
runObserve  (Op op k) = case op of
      ObsPatt d y α -> do
        runObserve (k y)
      Other op -> Op op (runObserve . k)

logMeanExp :: [LogP] -> LogP
logMeanExp logps =
  let logws = map logP logps
      c = maximum logws
      l = length logws
  in  LogP $ c + log ((1.0/fromIntegral l) * sum (map (\logw -> exp (logw - c)) logws))
