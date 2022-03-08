{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Extensible.Inference.SMC where
-- import Data.Extensible hiding (Member)
import qualified Data.Map as Map
import Data.Maybe
import Data.Foldable
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

logMeanExp :: [Double] -> Double
logMeanExp logWₙₛ₁ = let _L = length logWₙₛ₁
                     in  log ( (1.0/fromIntegral _L) * (sum (map exp logWₙₛ₁)))

smc :: (es ~ '[ObsReader env, Dist, Observe, NonDet, Sample]) => Model env es a -> ModelEnv env -> Sampler [(a, Double)]
smc model env = (runSample . runNonDet . runObserve . runTwice . runDist . runObsReader env) (runModel model)

runTwice :: Member NonDet es => Prog es a -> Prog es a
runTwice m = do
  m <|> m

runLoopSMC :: Model env '[ObsReader env, Dist, Observe, NonDet, Sample] a -> ModelEnv env -> Sampler [(a, Double)]
runLoopSMC m env = runSample (loopSMC m env)

loopSMC :: (es ~ '[ObsReader env, Dist, Observe, NonDet, Sample]) => Model env es a -> ModelEnv env -> Prog '[Sample] [(a, Double)]
loopSMC model env = do
  let prog = (runTwice . runDist . runObsReader env) (runModel model)
  progs_probs <- (runNonDet . runObserveSMC 0) prog
  let probs = map snd progs_probs
  prinT (show probs)
  return []

runLoopSMC' :: Model env '[ObsReader env, Dist, Observe, NonDet, Sample] a -> ModelEnv env -> Sampler [(a, Double)]
runLoopSMC' m env = do
  let prog = (runTwice . runDist . runObsReader env) (runModel m)
  runSample (loopSMC' 2 (prog, 0))

loopSMC' :: forall es a. Member Sample es => Int -> (Prog (Observe : NonDet : es) a, Double) -> Prog es [(a, Double)]
loopSMC' n_particles (Val x, p) = Val [(x, p)]
loopSMC' n_particles (prog, logZ)  = do
  -- The first iteration will have two results, but because we don't apply 'runTwice' again, every subsequent iteration will have one result (because there are no more non-deterministic operations).
  progs_probs <- (runNonDet . runObserveSMC 0) prog
  let progs = map fst progs_probs
      logWs = map snd progs_probs
      logZ' = logZ + logMeanExp logWs
  prinT (show logZ')
  particle_idxs :: [Int] <- replicateM n_particles $ send (Sample (DiscreteDist (map exp logWs) Nothing Nothing) undefined)
  -- prinT ("alphas: " ++ show particle_idxs)
  prinT ("length: " ++ show (length progs_probs))
  let progs' = asum (map (\idx -> fst $ progs_probs !! idx) particle_idxs)
  -- concat <$> (sequence $ map (\prog' -> loopSMC' n_particles ( prog', logZ')) progs')
  progs_probs <- (runNonDet . runObserveSMC 0) progs'
  prinT ("length: " ++ show (length progs_probs))
  undefined

  -- undefined
  -- let progs' = map (\idx -> fst $ progs_probs' !! idx) particle_idx
  --     prog' = asum progs'
  -- prinT ("alphas: " ++ show particle_idxs)
  -- loopSMC' n_particles (prog', logZ')

-- When discharging Observe, return the rest of the program, and the log probability
runObserveSMC :: Member Sample es => Double -> Prog (Observe : es) a -> Prog es (Prog (Observe : es) a, Double)
runObserveSMC logp (Val x) = return (Val x, exp logp)
runObserveSMC logp (Op op k) = case op of
      ObsPatt d y α -> do
        let logp' = logProb d y
        prinT $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show logp'
        Val (k y, (logp + logp'))
      Other op -> Op op (runObserveSMC logp . k)

runObserve :: Member Sample es => Prog (Observe : es) a -> Prog es (a, Double)
runObserve = loop 0
  where
  loop :: Member Sample es => Double -> Prog (Observe : es) a -> Prog es (a, Double)
  loop logp (Val x) = return (x, exp logp)
  loop logp (Op op k) = case op of
      ObsPatt d y α -> do
        let logp' = logProb d y
        loop (logp + logp') (k y)
      Other op -> Op op (loop logp . k)

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