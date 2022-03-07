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

runLoopSMC' :: Model env '[ObsReader env, Dist, Observe, NonDet, Sample] a -> ModelEnv env -> Sampler [a]
runLoopSMC' m env = do
  let prog = (runTwice . runDist . runObsReader env) (runModel m)
  runSample (loopSMC' prog)

loopSMC' :: Prog '[Observe, NonDet, Sample] a -> Prog '[Sample] [a]
loopSMC' (Val x) = Val [x]
loopSMC' prog    = do
  progs_probs <- (runNonDet . runObserveSMC 0) prog
  let progs = map fst progs_probs
  let probs = map snd progs_probs
  prinT (show probs)
  concat <$> (sequence $ map loopSMC' progs)

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
        sample d >>= loop . k
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