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

type Resampler ctx es es' a = [ctx] -> ([Prog es' a], [ctx]) -> Prog es ([Prog es' a], [ctx])

logMeanExp :: [Double] -> Double
logMeanExp logWₙₛ₁ = let _L = length logWₙₛ₁
                     in   log ( (1.0/fromIntegral _L) * (sum (map exp logWₙₛ₁)))

sis :: forall a env ctx es'. (Monoid ctx,Show ctx,  FromSTrace env, Show a)
  => Members [Observe, Sample, NonDet] es'
  => Int
  -> Resampler ctx '[Sample] es' a
  -> (Prog es' a -> Prog '[Sample] [(Prog es' a, ctx)])
  -> Model env (ObsReader env : Dist : es') a
  -> ModelEnv env
  -> Sampler [(a, ctx)]
sis n_particles resampler pophdl model env = do
  let prog_0  = (runDist . runObsReader env) (runModel model)
      progs   = replicate n_particles prog_0
      ctxs :: [ctx]   = replicate n_particles mempty
  runSample (loopSIS n_particles resampler pophdl (progs, ctxs))

loopSIS :: (Show a, Show ctx, Monoid ctx) => Members [Sample, NonDet] es' => Member Sample es
  => Int
  -> Resampler ctx es es' a
  -> (Prog es' a -> Prog es [(Prog es' a, ctx)])
  -> ([Prog es' a], [ctx])   -- Particles and corresponding contexts
  -> Prog es [(a, ctx)]
loopSIS n_particles resampler populationHandler (progs_0, ctxs_0)  = do
  -- Run to next checkpoint
  progs_ctxs_1 <- populationHandler (asum progs_0)
  let (progs_1, ctxs_1) = unzip progs_ctxs_1
  case foldVals progs_1 of
  --   -- if all programs have finished, return with accumulated context
    Right vals  -> do let ctxs' = zipWith mappend ctxs_1 ctxs_0
                      (`zip` ctxs') <$> vals
  --   -- otherwise, pick programs to continue with
    Left  progs -> do (progs', ctxs') <- resampler ctxs_0 (progs_1, ctxs_1)
                      loopSIS n_particles resampler populationHandler (progs', ctxs')

smcPopulationHandler :: Member Sample es =>
     Prog (Observe : State STrace : NonDet : es) a
  -> Prog es [(Prog (Observe : State STrace : NonDet : es) a, (Double, STrace))]
smcPopulationHandler prog = do
  progs_ctxs <- (runNonDet . runState Map.empty . runObserve . traceSamples) prog
  let progs_ctxs' = map (\((prog, p), strace) -> (prog, (p, strace))) progs_ctxs
  return progs_ctxs'

smcResampler :: Member Sample es => Int -> Resampler (Double, STrace) es es' a
smcResampler n_particles probs_straces_0 (progs_1, probs_straces_1) = do
  let (probs_0, straces_0) = unzip probs_straces_0
      (probs_1, straces_1) = unzip probs_straces_1
      -- Merge sample traces
      straces = zipWith mappend straces_1 straces_0
      -- Compute log mean exp of previous probabilities
      logZ  = logMeanExp probs_0
      -- Compute normalized log probabilities of particles
      logWs = map (+ logZ) probs_1
  prinT $ "LogWs " ++ show logWs
  -- Select particles to continue with
  particle_idxs :: [Int] <- replicateM n_particles $ send (Sample (DiscreteDist (map exp logWs) Nothing Nothing) undefined)
  prinT $ "particle idx " ++ show particle_idxs
  let progs'         = map (progs_1 !!) particle_idxs
      logWs'         = map (logWs !!) particle_idxs
      straces'       = map (straces !!) particle_idxs
  return (progs', zip logWs' straces')

smc :: forall env es' a. (FromSTrace env, Show a) =>
  (es' ~ (ObsReader env : Dist : Observe : State STrace : NonDet : Sample : '[])) =>
  Int -> Model env es' a -> ModelEnv env -> Sampler [(a, Double, ModelEnv env)]
smc n_particles model env = do
  as_ps_straces <- sis n_particles (smcResampler n_particles) smcPopulationHandler model env
  return $ map (\(a, (p, strace)) -> (a, p, fromSTrace @env strace)) as_ps_straces

instance Semigroup Double where
  (<>) = (+)

instance Monoid Double where
  mempty = 0
  mappend = (<>)

traceSamples :: (Member Sample es, Member (State STrace) es) => Prog es a -> Prog (es) a
traceSamples  (Val x)  = return x
traceSamples  (Op u k) = case u of
    SampPatt d α ->  Op ( u) (\x -> do updateSTrace α x
                                       traceSamples (k x))
    _   -> Op ( u) (traceSamples . k)

-- When discharging Observe, return the rest of the program, and the log probability
runObserve :: Member Sample es => Prog (Observe : es) a -> Prog es (Prog (Observe : es) a, Double)
runObserve  (Val x) = return (Val x, 0)
runObserve  (Op op k) = case op of
      ObsPatt d y α -> do
        let logp = logProb d y
        -- prinT $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show logp
        Val (k y, logp)
      Other op -> Op op (runObserve . k)

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

-- transformLW :: (Member Sample ts) => Freer ts a -> Freer (State STrace : ts) a
-- transformLW = install return
--   (\x tx k -> case tx of
--       Sample d α -> case distDict d of
--         Dict -> do updateTrace α x
--                    k x
--       Printer s  -> k ()
--   )