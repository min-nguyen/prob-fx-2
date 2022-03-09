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

module Extensible.Inference.SMC_SIS where
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
import Extensible.Inference.SIS (sis, Accum(..), Resampler)
import qualified Extensible.OpenSum as OpenSum
import Extensible.OpenSum (OpenSum)
import Util

instance Accum Double where
  aempty = 0
  accum  = (+)

instance Ord k => Accum (Map k a) where
  aempty = Map.empty
  accum  = Map.union

logMeanExp :: [Double] -> Double
logMeanExp logWₙₛ₁ = let _L = length logWₙₛ₁
                     in   log ( (1.0/fromIntegral _L) * (sum (map exp logWₙₛ₁)))

smc :: forall env es' a. (FromSTrace env, Show a) =>
  (es' ~ (ObsReader env : Dist : Observe : State STrace : NonDet : Sample : '[])) =>
  Int -> Model env es' a -> ModelEnv env -> Sampler [(a, Double, ModelEnv env)]
smc n_particles model env = do
  as_ps_straces <- sis n_particles smcResampler smcPopulationHandler model env
  return $ map (\(a, (p, strace)) -> (a, p, fromSTrace @env strace)) as_ps_straces

smcPopulationHandler :: Member Sample es
  =>         [Prog (Observe : State STrace : NonDet : es) a]
  -> Prog es [(Prog (Observe : State STrace : NonDet : es) a, (Double, STrace))]
smcPopulationHandler progs = do
  -- Merge particles into single non-deterministic program using 'asum', and run to next checkpoint
  progs_ctxs <- (runNonDet . runState Map.empty . runObserve . traceSamples) (asum progs)
  let progs_ctxs' = map (\((prog, p), strace) -> (prog, (p, strace))) progs_ctxs
  return progs_ctxs'

smcResampler :: Member Sample es => Resampler (Double, STrace) es es' a
smcResampler logWs_straces_0 logWs_straces_1sub0 progs = do
  let -- Get log weights and sample traces from previous particle run
      (logWs_0    , straces_0)      = unzip logWs_straces_0
      -- Get log weights and sample traces since previous particle run
      (logWs_1sub0, straces_1sub0)  = unzip logWs_straces_1sub0
      -- Compute log mean exp of previous probabilities
      logZ  = logMeanExp logWs_0
      -- Compute normalized log probabilities of current particles
      logWs_1 = map (+ logZ) logWs_1sub0
      -- Accumulate sample traces
      straces_1 = zipWith Map.union straces_1sub0 straces_0
      n_particles = length progs
  prinT $ "LogWs " ++ show logWs_1
  -- Select particles to continue with
  particle_idxs :: [Int] <- replicateM n_particles $ send (Sample (DiscreteDist (map exp logWs_1) Nothing Nothing) undefined)
  prinT $ "particle idx " ++ show particle_idxs
  let progs'         = map (progs !!) particle_idxs
      logWs'         = map (logWs_1 !!) particle_idxs
      straces'       = map (straces_1 !!) particle_idxs
  return (progs', zip logWs' straces')

traceSamples :: (Member Sample es, Member (State STrace) es) => Prog es a -> Prog (es) a
traceSamples  (Val x)  = return x
traceSamples  (Op u k) = case u of
    SampPatt d α ->  Op u (\x -> do updateSTrace α x
                                    traceSamples (k x))
    _   -> Op u (traceSamples . k)

-- When discharging Observe, return the rest of the program, and the log probability
runObserve :: Member Sample es => Prog (Observe : es) a -> Prog es (Prog (Observe : es) a, Double)
runObserve  (Val x) = return (Val x, 0)
runObserve  (Op op k) = case op of
      ObsPatt d y α -> do
        let logp = logProb d y
        -- prinT $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show logp
        Val (k y, logp)
      Other op -> Op op (runObserve . k)