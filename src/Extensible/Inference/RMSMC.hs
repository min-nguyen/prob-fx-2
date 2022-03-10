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

module Extensible.Inference.RMSMC where
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
import Extensible.Inference.MH
import Extensible.Inference.SIS (sis, Accum(..), Resampler, ParticleHandler)
import qualified Extensible.OpenSum as OpenSum
import Extensible.OpenSum (OpenSum)
import Util

loopSIS :: (Show a, Show ctx, Accum ctx) => Members [Sample, NonDet] es' => Member Sample es
  => Int
  -> Resampler       ctx es es' a
  -> ParticleHandler ctx es es' a
  -> ([Prog es' a], [ctx])   -- Particles and corresponding contexts
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

-- type Resampler       ctx es es' a = [ctx] -> [ctx] -> [Prog es' a] -> Prog es ([Prog es' a], [ctx])

rmsmcResampler :: Members [Observe, Sample] es0 => es ~ ObsReader env : Dist : es0 =>
  Model env es a -> ModelEnv env -> [(Addr, STrace, Double)] -> [(Addr, STrace, Double)] -> [Prog es' a] -> Prog es ([Prog es' a], [ctx])
rmsmcResampler model env ctx_0 ctx_1 progs_1 = do
  let breakpoint = fst3 (head ctx_0)
      model'     = Model $ (runDist . runObsReader env) (runModel model)
  let f = mhStep env model'
  undefined


insertBreakpoint :: Members [Observe, Sample] es =>
  Addr -> Prog es a -> Prog es (Prog es a)
insertBreakpoint α_break (Val x) = return (Val x)
insertBreakpoint α_break (Op op k) = case op of
      ObsPatt d y α -> do
        if α_break == α
          then Val (k y)
          else Op op (insertBreakpoint α_break . k)
      _ -> Op op (insertBreakpoint α_break . k)
