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



-- loopSIS :: (Show a, Show ctx, Accum ctx) => Member Sample es
--   => Int
--   -> Resampler       ctx es a
--   -> ParticleHandler ctx es a
--   -> ([Prog (NonDet : es) a], [ctx])   -- Particles and corresponding contexts
--   -> Prog es [(a, ctx)]
-- loopSIS n_particles resampler populationHandler (progs_0, ctxs_0)  = do
--   -- Run particles to next checkpoint
--   (progs_1, ctxs_1) <- unzip <$> populationHandler progs_0
--   case foldVals progs_1 of
--   -- if all programs have finished, return with accumulated context
--     Right vals  -> do let ctxs' = zipWith accum ctxs_1 ctxs_0
--                       (`zip` ctxs') <$> vals
--   -- otherwise, pick programs to continue with
--     Left  progs -> do (progs', ctxs') <- resampler ctxs_0 ctxs_1 progs_1
--                       loopSIS n_particles resampler populationHandler (progs', ctxs')

-- rmsmc :: forall env es' a. (FromSTrace env, Show a) =>
--   (es' ~ [ObsReader env, Dist, Lift Sampler]) =>
--   Int -> Model env es' a -> ModelEnv env -> Sampler [(a, Double, ModelEnv env)]
-- rmsmc n_particles model env = do
--   let model_0 = (runDist . runObsReader env) (runModel model)
--   as_ps_straces <- sis n_particles (rmsmcResampler model_0) rmsmcPopulationHandler model env
--   undefined
--   return $ map (\(a, (p, strace)) -> (a, p, fromSTrace @env strace)) as_ps_straces

-- type Resampler       ctx es a = [ctx] -> [ctx] -> [Prog (NonDet : es) a] -> Prog es ([Prog (NonDet : es) a], [ctx])

rmsmcPopulationHandler :: Members [Observe, Sample] es
  =>         [Prog (NonDet:es) a]
  -> Prog es [(Prog (NonDet:es) a, (Addr, Double, SDTrace))]
rmsmcPopulationHandler progs = do
  -- Merge particles into single non-deterministic program using 'asum', and run to next checkpoint
  progs_ctxs <- (runNonDet . runState Map.empty . traceSamples . breakObserves ) (asum progs)
  -- List of particles that can be resumed, their observe breakpoint address, the log probability at that break point, and an accumulated sample trace
  let progs_ctxs' = map (\((prog, α, p), strace) -> (prog, (α, p,  strace))) progs_ctxs
  return progs_ctxs'

-- Members '[Sample, Lift Sampler] es =>  Prog '[Observe, Sample] a -> Resampler (Addr, Double, SDTrace) es a
rmsmcResampler :: forall es a ctx.
     Prog '[Observe, Sample, Lift Sampler] a -- the initial program, representing the entire unevaluated model execution (having already provided a model environment)
  -> [(Addr, Double, SDTrace)]
  -> [(Addr, Double, SDTrace)]
  -> [Prog es a]
  -> Prog '[Observe, Sample, Lift Sampler] ([Prog es a], [(Addr, Double, SDTrace)])
rmsmcResampler model_0 ctx_0 ctx_1sub0 progs_1 = do
  let α_break       = fst3 (head ctx_1sub0)
  prinT $ "α_break" ++ show α_break
  let -- insert break point to perform MH up to
      partial_model = insertBreakpoint α_break model_0
      -- for each particle, merge their incremental sample trace with their sample trace up until the previous break point
      straces_accum = zipWith Map.union (map thrd3 ctx_0) (map thrd3 ctx_1sub0)
  -- perform metropolis-hastings on each particle's sample trace
  mhTraces <- lift $ mapM (\sdtrace -> mh' 10 partial_model sdtrace []) straces_accum
  let -- get the continuations of each particle from the break point
      particles'     = map (fst3 . last) mhTraces
      -- get the sample traces of each particle up until the break point
      straces_accum' = map (snd3 . last) mhTraces
      {- To do: decide how weightings of particles work-}

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
