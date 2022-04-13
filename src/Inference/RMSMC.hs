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

module Inference.RMSMC where
-- import Data.Extensible hiding (Member)
import qualified Data.Map as Map
import Data.Maybe
import Data.Bifunctor
import Data.Map (Map)
import ModelEnv
import Control.Monad
import Control.Applicative
import Dist
import Freer
import Model
import NonDet
import Sampler
import IO
import ObsReader
import State
import STrace
import Sampler
import Writer
import Inference.MH
import Inference.SIS (sis, Accum(..), Resampler, ParticleHandler)
import qualified OpenSum as OpenSum
import OpenSum (OpenSum)
import Util



loopSIS :: (Show a, Show ctx, Accum ctx) => Member Sample es
  => Int
  -> Resampler       ctx es a
  -> ParticleHandler ctx es a
  -> ([Prog (NonDet : es) a], [ctx])   -- Particles and corresponding contexts
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

rmsmcPopulationHandler :: Members [Observe, Sample] es
  =>         [Prog (NonDet:es) a]
  -> Prog es [(Prog (NonDet:es) a, (Addr, Double, SDTrace))]
rmsmcPopulationHandler progs = do
  -- Merge particles into single non-deterministic program using 'asum', and run to next checkpoint
  progs_ctxs <- (runNonDet . runState Map.empty . traceSamples . breakObserves ) (asum progs)
  let progs_ctxs' = map (\((prog, α, p), strace) -> (prog, (α, p,  strace))) progs_ctxs
  return progs_ctxs'

rmsmcResampler :: forall es' a ctx.
     Prog '[Observe, Sample] a -- the initial program, representing the entire unevaluated model execution (having already provided a model environment)
  -> [(Addr, Double, SDTrace)] 
  -> [(Addr, Double, SDTrace)]
  -> [Prog es' a]
  -> Prog '[Observe, Sample, Lift Sampler] ([Prog es' a], [ctx])
rmsmcResampler model ctx_0 ctx_1 progs_1 = do
  let α_break       = fst3 (head ctx_0)
  let partial_model = insertBreakpoint α_break model
      straces_accum = zipWith Map.union (map thrd3 ctx_0) (map thrd3 ctx_1)
  f <- lift $ mapM (\sdtrace -> mh' 10 partial_model sdtrace []) straces_accum
  -- let f = mhStep env model'
  undefined

traceSamples :: (Member Sample es) => Prog es a -> Prog (State SDTrace : es) a
traceSamples  (Val x)  = return x
traceSamples  (Op u k) = case u of
    SampPatt d α ->  Op (weaken u) (\x -> do updateSDTrace α d x
                                             traceSamples (k x))
    _   -> Op (weaken u) (traceSamples . k)

breakObserves :: Member Observe es => Prog es a -> Prog es (Prog es a, Addr, Double)
breakObserves  (Val x) = return (Val x, ("", 0), 0)
breakObserves  (Op op k) = case op of
      ObsPatt d y α -> do
        let logp = logProb d y
        -- prinT $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show logp
        Val (k y, α, logp)
      _ -> Op op (breakObserves . k)

insertBreakpoint :: Members [Observe, Sample] es =>
  Addr -> Prog es a -> Prog es (Prog es a)
insertBreakpoint α_break (Val x) = return (Val x)
insertBreakpoint α_break (Op op k) = case op of
      ObsPatt d y α -> do
        if α_break == α
          then Val (k y)
          else Op op (insertBreakpoint α_break . k)
      _ -> Op op (insertBreakpoint α_break . k)
