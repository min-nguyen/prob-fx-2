{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Inference.LW where

import qualified Data.Map as Map
import Data.Map (Map)
import Env
import Effects.ObsReader
import Control.Monad
import Control.Monad.Trans.Class
import Unsafe.Coerce
import Effects.Dist
import qualified Example as Example
import Prog
import Model hiding (runModelFree)
import Sampler
import Effects.State ( modify, handleState, State )
import Trace
import qualified OpenSum as OpenSum
import OpenSum (OpenSum(..))
import Util

type LWTrace a = [(a, STrace, Double)]

-- | Run LW n times
lwTopLevel :: forall env es a b. (FromSTrace env, es ~ '[ObsReader env, Dist])
   => Int                                   -- Number of lw iterations
   -> Model env es a                        -- Model
   -> Env env                          -- List of model observed variables
   -> Sampler [(a, Env env, Double)]   -- List of n likelihood weightings for each data point
lwTopLevel n model env = do
  let prog = (handleDist . handleObsRead env) (runModel model)
  lwTrace <- lw n prog
  return (map (mapsnd3 (fromSTrace @env)) lwTrace)

lw :: forall env es a b. (es ~ '[Observe, Sample])
   => Int                   -- Number of lw iterations
   -> Prog es a             -- Model
   -> Sampler (LWTrace a)   -- List of n likelihood weightings for each data point
lw n prog = replicateM n (runLW prog)

-- | Run LW once
runLW :: es ~ '[Observe, Sample]
  => Prog es a                    -- Model
  -> Sampler (a, STrace, Double)
runLW prog = do
  ((x, samples), p) <- (runSample . runObserve . traceSamples) prog
  return (x, samples, p)

runObserve :: Member Sample es => Prog (Observe : es) a -> Prog es (a, Double)
runObserve = loop 0
  where
  loop :: Member Sample es => Double -> Prog (Observe : es) a -> Prog es (a, Double)
  loop logp (Val x) = return (x, exp logp)
  loop logp (Op u k) = case  u of
      ObsPatt d y α -> do
        let logp' = logProb d y
        -- prinT $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show logp'
        loop (logp + logp') (k y)
      DecompLeft u' -> Op u' (loop logp . k)

runSample :: Prog '[Sample] a -> Sampler a
runSample = loop
  where
  loop :: Prog '[Sample] a -> Sampler a
  loop (Val x) = return x
  loop (Op u k) =
    case u of
      SampPatt d α ->
        -- liftS (putStrLn $ ">> : " ++ show α) >>
        sample d >>= loop . k
      PrintPatt s  ->
        liftS (putStrLn s) >>= loop . k
      _         -> error "Impossible: Nothing cannot occur"
