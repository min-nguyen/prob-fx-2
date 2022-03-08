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
import qualified Extensible.OpenSum as OpenSum
import Extensible.OpenSum (OpenSum)

updateTrace :: forall es x. (Member (State STrace) es, OpenSum.Member x PrimVal) => Addr -> x -> Prog es ()
updateTrace α x = modify (Map.insert α (OpenSum.inj x) :: STrace -> STrace)

logMeanExp :: [Double] -> Double
logMeanExp logWₙₛ₁ = let _L = length logWₙₛ₁
                     in  log ( (1.0/fromIntegral _L) * (sum (map exp logWₙₛ₁)))

smc :: Show a => Int -> Model env '[ObsReader env, Dist, Observe, NonDet, Sample] a -> ModelEnv env -> Sampler [(a, Double)]
smc n_particles model  env = do
  let prog = (branch n_particles . runDist . runObsReader env) (runModel model)
  runSample (loopSMC n_particles (prog, 0))

loopSMC :: forall es a. Show a => Member Sample es => Int -> (Prog (Observe : NonDet : es) a, Double) -> Prog es [(a, Double)]
loopSMC n_particles (Val x, p) = Val [(x, p)]
loopSMC n_particles (prog, logZ)  = do
  progs_probs <- (runNonDet . runObserve) prog
  let -- get log probabilities of each particle since between previous observe operation
      (progs, ps) = unzip progs_probs
      -- compute normalized importance weights of each particle
      logWs       = map (+ logZ) ps
      -- set logZ to be log mean exp of all particle's normalized importance weights
      logZ'       = logZ + logMeanExp logWs
  case foldVals progs of
    -- if all programs have finished, return with their normalized importance weights
    Right vals  -> (`zip` logWs) <$> vals
    Left  progs -> do particle_idxs :: [Int] <- replicateM n_particles $ send (Sample (DiscreteDist (map exp logWs) Nothing Nothing) undefined)
                      prinT ("alphas: " ++ show particle_idxs)
                      let prog' = asum (map (progs !!) particle_idxs)
                      loopSMC n_particles (prog', logZ')

traceSamples :: (Member Sample es, Member (State STrace) es) => Prog es a -> Prog es a
traceSamples  (Val x)  = return x
traceSamples  (Op u k) = case u of
    SampPatt d α ->  Op u (\x -> do updateTrace α x
                                    traceSamples (k x))
    _   -> Op u (traceSamples . k)


-- When discharging Observe, return the rest of the program, and the log probability
runObserve :: Member Sample es => Prog (Observe : es) a -> Prog es (Prog (Observe : es) a, Double)
runObserve  (Val x) = return (Val x, 0)
runObserve  (Op op k) = case op of
      ObsPatt d y α -> do
        let logp = logProb d y
        prinT $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show logp
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