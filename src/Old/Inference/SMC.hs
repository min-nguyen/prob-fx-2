{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Inference.SMC where
-- import Data.Extensible hiding (Member)
import qualified Data.Map as Map
import Data.Maybe
import Data.Bifunctor
import Data.Map (Map)
import ModelEnv
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Dist
import Freer
import Model
import NonDet
import Sampler
import ObsReader
import State
import STrace
import Sampler
import Writer
import qualified OpenSum as OpenSum
import OpenSum (OpenSum)
import Util


logMeanExp :: [Double] -> Double
logMeanExp logWₙₛ₁ = let _L = length logWₙₛ₁
                     in   log ( (1.0/fromIntegral _L) * (sum (map exp logWₙₛ₁)))

smc :: forall a env. (FromSTrace env, Show a) => -- , Observe, State STrace, NonDet, Sample
  Int -> Model env '[ObsReader env, Dist] a -> ModelEnv env -> Sampler [(a, ModelEnv env, Double)]
smc n_particles model env = do
  let prog = (traceSamples . branchWeaken n_particles . runDist . runObsReader env) (runModel model)
  particles <- (runSample . runObserve) (loopSMC n_particles [0] (repeat Map.empty) prog)
  let particles' = map (\(a, strace, prob) -> (a, fromSTrace @env strace, prob)) particles
  return particles'

{- Text book version of SMC that uses log mean exp: this causes the log weights to tend to -infinity too fast -}

loopSMC :: Show a => Members [Observe, Sample] es
  => Int -> [Double] -> [STrace] -> Prog (State STrace : NonDet : es) a -> Prog es [(a, STrace, Double)]
loopSMC n_particles logWs_prev straces_accum prog  = do
  progs_probs <- (runNonDet . runState Map.empty . breakObserve) prog
  let -- get log probabilities of each particle since between previous observe operation
      (progs, ps, straces) = (unzip3 . untuple3) progs_probs
      -- compute normalized importance weights of each particle
      logZ  = logMeanExp logWs_prev
      logWs = map (+ logZ) ps
      -- accumulate sample traces of each particle
      straces_accum'      = zipWith Map.union straces straces_accum
  prinT $ "logZ': "  ++  show logZ
  prinT $ "logWs: "  ++  show logWs
  -- prinT $ show straces_accum'
  case foldVals progs of
    -- if all programs have finished, return with their normalized importance weights
    Right vals  -> (\val -> zip3 val straces_accum' logWs) <$> vals
    -- otherwise, pick programs to continue with
    Left  progs -> do particle_idxs :: [Int] <- replicateM n_particles $ send (Sample (DiscreteDist (map exp logWs) Nothing Nothing) undefined)
                      -- prinT ("particle indexes: " ++ show particle_idxs ++ " ")
                      let -- set logZ to be log mean exp of all particle's normalized importance weights
                          logWs'          = map (logWs !!) particle_idxs
                          prog'           = asum (map (progs !!) particle_idxs)
                          straces_accum'' = map (straces_accum' !!) particle_idxs
                      -- prinT $ "continuing with" ++ show   straces_accum''
                      loopSMC n_particles logWs' straces_accum'' prog'

traceSamples :: (Member Sample es) => Prog es a -> Prog (State STrace : es) a
traceSamples  (Val x)  = return x
traceSamples  (Op u k) = case u of
    SampPatt d α ->  Op (weaken u) (\x -> do updateSTrace α x
                                             traceSamples (k x))
    _   -> Op (weaken u) (traceSamples . k)

-- When discharging Observe, return the rest of the program, and the log probability
breakObserve :: Member Observe es => Prog es a -> Prog es (Prog es a, Double)
breakObserve  (Val x) = return (Val x, 0)
breakObserve  (Op op k) = case op of
      ObsPatt d y α -> do
        let logp = logProb d y
        -- prinT $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show logp
        Val (k y, logp)
      _ -> Op op (breakObserve . k)

runObserve :: Prog (Observe : es) a -> Prog es a
runObserve  (Val x) = return x
runObserve  (Op op k) = case op of
      ObsPatt d y α -> do
        runObserve (k y)
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