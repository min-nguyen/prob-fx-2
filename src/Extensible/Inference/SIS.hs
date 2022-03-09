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

type Resampler ctx es a = ([Prog es a], [ctx]) -> ([Prog es a], [ctx])

logMeanExp :: [Double] -> Double
logMeanExp logWₙₛ₁ = let _L = length logWₙₛ₁
                     in   log ( (1.0/fromIntegral _L) * (sum (map exp logWₙₛ₁)))

-- sis :: forall a env ctx. (Monoid ctx, FromSTrace env, Show a) =>
--   Int -> Resampler  (Observe : State STrace : NonDet : Sample : '[]) a
--   -> PopulationHdl ctx (Observe : State STrace : NonDet : Sample : '[]) '[Sample] a
--   -> Model env '[ObsReader env, Dist, Observe, State STrace, NonDet, Sample] a -> ModelEnv env -> Sampler [(ctx, a)]
-- sis n_particles resampler pophdl model env = do
--   let prog    = (branch n_particles . traceSamples . runDist . runObsReader env) (runModel model)
--   runSample (loopSIS  n_particles resampler pophdl (prog, repeat mempty ))

-- resampleSMC :: Resampler es a
-- resampleSMC  ctxs_progs = do
--   particle_idxs :: [Int] <- replicateM n_particles $ send (Sample (DiscreteDist (map exp logWs) Nothing Nothing) undefined)
--   undefined

loopSIS :: Show a => Member Sample es => Member NonDet es' => Monoid ctx
  => Int
  -> Resampler ctx es' a
  -> (Prog es' a -> Prog es [(Prog es' a, ctx)])
  -> ([Prog es' a], [ctx]) -> Prog es [(a, ctx)]
loopSIS n_particles resampler populationHandler (progs_0, ctxs_0)  = do
  progs_ctxs <- populationHandler (asum progs_0)
  let (progs', ctxs) = unzip progs_ctxs
      -- merge sample traces
      ctxs' = zipWith mappend ctxs ctxs_0
      -- -- compute mean of previous log weights
      -- logZ     = logMeanExp logWs_0
      -- -- compute normalized log weights
      -- logWs'   = map (+ logZ) ps
      progs_ctxs' =  zip progs' ctxs'
      -- get log probabilities of each particle since between previous observe operation
      -- (ps, straces, progs) = (unzip3 . untuple3) progs_probs
      -- compute normalized importance weights of each particle
      -- logWs'  = map (+ logZ) ps
      -- accumulate sample traces of each particle
      -- straces_accum'      = zipWith Map.union straces straces_accum
  -- prinT $ "logWs: "  ++  show logZ
  -- prinT $ show straces_accum'
  undefined
  case foldVals progs' of
  --   -- if all programs have finished, return with their normalized importance weights
    Right vals  -> (`zip` ctxs') <$> vals
  --   -- otherwise, pick programs to continue with
    Left  progs -> do let (progs'', ctxs'') = resampler (progs', ctxs)
                      loopSIS n_particles resampler populationHandler (progs'', ctxs'')

smcPopulationHandler :: Member Sample es =>
  Prog (Observe : State STrace : NonDet : es) a -> Prog es [(Prog (Observe : State STrace : NonDet : es) a, (Double, STrace))]
smcPopulationHandler prog = do
  let prog'  = traceSamples prog
  progs_ctxs <- (runNonDet . runState Map.empty . runObserve) prog'
  let progs_ctxs' = map (\((prog, p), strace) -> (prog, (p, strace))) progs_ctxs
  return progs_ctxs'

f :: Show a => (es' ~ (Observe : State STrace : NonDet : es)) => Member Sample es =>
      Resampler (Double, STrace) es' a ->
     ([Prog es' a], [(Double, STrace)]) -> Prog es [(a, (Double, STrace))]
f resampler = loopSIS 0 resampler smcPopulationHandler

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


data Break a where
  Break :: Break a

injResample :: Prog es a -> Prog es a
injResample = undefined

-- runNonDet :: Prog (NonDet ': es) a -> Prog es [a]
-- runNonDet (Val x) = return [x]
-- runNonDet (Op op k) = case op of
--    Choose' -> (<|>) <$> runNonDet (k True) <*> runNonDet (k False)
--    Empty'  -> Val []
--    Other op  -> Op op (runNonDet . k)

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