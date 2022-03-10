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

module Extensible.Inference.SIS where
-- import Data.Extensible hiding (Member)
import qualified Data.Map as Map
import Data.Maybe
import Data.Bifunctor
import Data.Map (Map)
import Extensible.ModelEnv
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Extensible.Dist
import Extensible.Freer
import Extensible.Model
import Extensible.NonDet
import Extensible.Sampler
import Extensible.ObsReader
import Extensible.State
import Extensible.STrace
import Extensible.Sampler
import Extensible.Writer
import qualified Extensible.OpenSum as OpenSum
import Extensible.OpenSum (OpenSum)
import Util

{- Takes previous contexts of particles, the contexts of particles since previous execution, and the current particles,
  , and decides which particles and contexts to continue with. -}
type Resampler       ctx es a = [ctx] -> [ctx] -> [Prog (NonDet : es) a] -> Prog es ([Prog (NonDet : es) a], [ctx])
{- Takes list of particles and runs them to the next step, producing a list of particles and their yielded contexts -}
type ParticleHandler ctx es a = [Prog (NonDet : es) a] -> Prog es [(Prog (NonDet : es) a, ctx)]

class Accum ctx where
  accum  :: ctx -> ctx -> ctx
  aempty :: ctx

instance (Accum ctx1, Accum ctx2) => Accum (ctx1, ctx2) where
  aempty = (aempty, aempty)
  accum (xs, ys) (xs', ys') = (accum xs xs', accum ys ys')

sis :: forall a env ctx es'.
     (Accum ctx, Show ctx, FromSTrace env, Show a)
  -- => Member NonDet es'
  => Int
  -> Resampler       ctx '[Observe, Sample]  a
  -> ParticleHandler ctx '[Observe, Sample] a
  -> Model env [ObsReader env, Dist] a
  -> ModelEnv env
  -> Sampler [(a, ctx)]
sis n_particles resampler pophdl model env = do
  let prog_0  = (runDist . runObsReader env) (runModel model)
      progs   = replicate n_particles (weaken' prog_0)
      ctxs    = replicate n_particles aempty
  (runSample . runObserve ) (loopSIS n_particles resampler pophdl (progs, ctxs))

loopSIS :: (Show a, Show ctx, Accum ctx)
  => Int
  -> Resampler       ctx es  a
  -> ParticleHandler ctx es a
  -> ([Prog (NonDet:es) a], [ctx])   -- Particles and corresponding contexts
  -> Prog es [(a, ctx)]
loopSIS n_particles resampler populationHandler (progs_0, ctxs_0)  = do
  -- Run particles to next checkpoint
  (progs_1, ctxs_1) <- unzip <$> populationHandler progs_0
  case foldVals progs_1 of
  -- if all programs have finished, return with accumulated context
    Right vals  -> do let ctxs' = zipWith accum ctxs_1 ctxs_0
                      (`zip` ctxs') <$> vals
  -- otherwise, pick programs to continue with
    Left  progs -> do (progs', ctxs') <- resampler ctxs_0 ctxs_1 progs_1
                      loopSIS n_particles resampler populationHandler (progs', ctxs')

runSample :: Prog '[Sample] a -> Sampler a
runSample = loop
  where
  loop :: Prog '[Sample] a -> Sampler a
  loop (Val x) = return x
  loop (Op u k) =
    case u of
      SampPatt d α ->
        sample d >>= \x -> -- printS ("Sampled " ++ show x ++ " from " ++ show d) >>
                    loop (k x)
      PrintPatt s  ->
        liftS (putStrLn s) >>= loop . k
      _         -> error "Impossible: Nothing cannot occur"

runObserve :: Prog (Observe : es) a -> Prog es a
runObserve  (Val x) = return x
runObserve  (Op op k) = case op of
      ObsPatt d y α -> do
        runObserve (k y)
      Other op -> Op op (runObserve . k)