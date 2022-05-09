{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Old.Inference.SMC where
-- import Data.Extensible hiding (Member)
import qualified Data.Map as Map
import Data.Maybe
import Data.Bifunctor
import Data.Map (Map)
import Env
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Effects.Dist
import Prog
import Model
import Effects.NonDet
import Sampler
import Effects.ObsReader
import Effects.State
import Old.Trace
import Sampler
import Effects.Writer
import qualified OpenSum as OpenSum
import OpenSum (OpenSum)
import Util

smc :: forall a env. (FromSTrace env, Show a) => -- , Observe, State Trace, NonDet, Sample
  Int -> Model env '[ObsReader env, Dist] a -> Env env -> Sampler [(a, Env env, Double)]
smc n_particles model env = do
  let prog = (storeSamples . branchWeaken n_particles . handleDist . handleObsRead env) (runModel model)
  particles <- (runSample . runObserve) (loopSMC n_particles [0] (repeat Map.empty) prog)
  let particles' = map (\(a, strace, prob) -> (a, fromSTrace @env strace, prob)) particles
  return particles'

{- Text book version of SMC that uses log mean exp -}
logMeanExp :: [Double] -> Double
logMeanExp logws =
  let c = maximum logws
      l = length logws
  in  c + log ((1.0/fromIntegral l) * sum (map (\logw -> exp (logw - c)) logws))

logSumExp :: [Double] -> Double
logSumExp logws =
  let c = maximum logws
      l = length logws
  in  c + log (sum (map (\logw -> exp (logw - c)) logws))

loopSMC :: Show a => Members [Observe, Sample] es
  => Int -> [Double] -> [STrace] -> Prog (State STrace : NonDet : es) a -> Prog es [(a, STrace, Double)]
loopSMC n_particles logWs_prev straces_accum prog  = do
  progs_probs <- (runNonDet . handleState Map.empty . breakObserve) prog
  let -- get log probabilities of each particle since between previous observe operation
      (progs, ps, straces) = (unzip3 . untuple3) progs_probs
      -- compute normalized importance weights of each particle
      logZ  = logMeanExp logWs_prev
      logWs = map (+ logZ) ps
      -- accumulate sample traces of each particle
      straces_accum' = zipWith Map.union straces straces_accum
  prinT $ "logZ': "  ++  show logZ
  prinT $ "ps: "  ++  show ps
  prinT $ "logWs: "  ++  show logWs
  prinT $ "sample ps: " ++ show (map exp logWs)
  -- prinT $ show straces_accum'
  case foldVals progs of
    -- if all programs have finished, return with their normalized importance weights
    Right vals  -> (\val -> zip3 val straces_accum' logWs) <$> vals
    -- otherwise, pick programs to continue with
    Left  progs -> do particle_idxs :: [Int] <- replicateM n_particles $ call (Sample (DiscreteDist (map exp logWs) Nothing Nothing) undefined)
                      -- prinT ("particle indexes: " ++ show particle_idxs ++ " ")
                      let -- set logZ to be log mean exp of all particle's normalized importance weights
                          logWs'          = map (logWs !!) particle_idxs
                          prog'           = asum (map (progs !!) particle_idxs)
                          straces_accum'' = map (straces_accum' !!) particle_idxs
                      -- prinT $ "continuing with" ++ show   straces_accum''
                      loopSMC n_particles logWs' straces_accum'' prog'

storeSamples :: (Member Sample es) => Prog es a -> Prog (State STrace : es) a
storeSamples  (Val x)  = return x
storeSamples  (Op u k) = case u of
    SampPatt d α ->  Op (weaken u) (\x -> do updateSTrace α x
                                             storeSamples (k x))
    _   -> Op (weaken u) (storeSamples . k)

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

-- loopSMC :: Show a => Members [Observe, Sample] es
--   => Int -> [Double] -> [Trace] -> Prog (State Trace : NonDet : es) a -> Prog es [(a, Trace, Double)]
-- loopSMC n_particles logWs_prev straces_accum prog  = do
--   progs_probs <- (runNonDet . handleState Map.empty . breakObserve) prog
--   let -- get log probabilities of each particle since between previous observe operation
--       (progs, ps, straces) = (unzip3 . untuple3) progs_probs
--       -- accumulate sample traces of each particle
--       straces_accum'      = zipWith Map.union straces straces_accum
--       -- compute normalized importance weights of each particle
--       logWs = map (log . ((/ sum (map exp logWs_prev)) . exp)) ps
--   prinT $ "logWs: "  ++  show logWs
--   -- prinT $ show straces_accum'
--   case foldVals progs of
--     -- if all programs have finished, return with their normalized importance weights
--     Right vals  -> (\val -> zip3 val straces_accum' logWs) <$> vals
--     -- otherwise, pick programs to continue with
--     Left  progs -> do particle_idxs :: [Int] <- replicateM n_particles $ call (Sample (DiscreteDist (map exp logWs) Nothing Nothing) undefined)
--                       -- prinT ("particle indexes: " ++ show particle_idxs ++ " ")
--                       let -- set logZ to be log mean exp of all particle's normalized importance weights
--                           logWs'          = map (logWs !!) particle_idxs
--                           prog'           = asum (map (progs !!) particle_idxs)
--                           straces_accum'' = map (straces_accum' !!) particle_idxs
--                       -- prinT $ "continuing with" ++ show   straces_accum''
--                       loopSMC n_particles logWs' straces_accum'' prog'
