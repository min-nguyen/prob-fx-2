{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
module Extensible.Inference.SimulateTrace where

-- import Data.Extensible hiding (Member)
import qualified Data.Map as Map
import Data.Map (Map)
import Extensible.OpenProduct
import Control.Monad
import Control.Monad.Trans.Class
import Extensible.Dist
import Extensible.Freer
import Extensible.Model
import Extensible.Sampler
import Extensible.AffineReader
import Extensible.State
import Extensible.Example as Example
import qualified Extensible.OpenSum as OpenSum
import Extensible.OpenSum (OpenSum)
import Unsafe.Coerce (unsafeCoerce)
import Util

type SampleMap = Map Addr (OpenSum PrimVal)

type Trace a = [(a, SampleMap)]

extractSamples ::  forall a. (Eq a, OpenSum.Member a PrimVal) => (Tag, Proxy a) -> SampleMap -> [a]
extractSamples (x, typ)  =
    map (unsafeCoerce . snd)
  . Map.toList
  . Map.filterWithKey  (\(tag, idx) _ -> tag == x)

simulate :: (ts ~ '[Dist, Observe, AffReader env, Sample])
  => Int                             -- Number of iterations per data point
  -> (b -> Model env ts a)           -- Model awaiting input variable
  -> [b]                             -- List of model input variables
  -> [LRec env]                      -- List of model observed variables
  -> Sampler [(a, SampleMap)]
simulate n model xs envs = do
  let runN (x, env) = replicateM n (runSimulate env (model x))
  concat <$> mapM runN (zip xs envs)

runSimulate :: (ts ~ '[Dist, Observe, AffReader env, Sample])
 => LRec env -> Model env ts a -> Sampler (a, SampleMap)
runSimulate ys m
  = (runSample Map.empty . runAffReader ys . runObserve . runDist) (runModel m)

simulateWith :: (ts ~ '[Dist, Observe, AffReader env, Sample])
  => Int                             -- Number of iterations per data point
  -> (b -> Model env (t:ts) a)       -- Model awaiting input variable
  -> [b]                             -- List of model input variables
  -> [LRec env]                      -- List of model observed variables
  -> (Model env (t:ts) a -> Model env ts c)
  -> Sampler [(c, SampleMap)]
simulateWith n model xs envs h = do
  let runN (x, env) = replicateM n (runSimulateWith env (model x) h)
  concat <$> mapM runN (zip xs envs)

runSimulateWith :: (ts ~ '[Dist, Observe, AffReader env, Sample])
 => LRec env
 -> Model env (t:ts) a
 -> (Model env (t:ts) a -> Model env ts c)
 -> Sampler (c, SampleMap)
runSimulateWith ys m h
  = (runSample Map.empty . runAffReader ys . runObserve . runDist) (runModel $ h m)

runObserve :: Freer (Observe : ts) a -> Freer ts  a
runObserve (Pure x) = return x
runObserve (Free u k) = case u of
  ObsPatt d y α ->
    let p = logProb d y
    in  runObserve (k y)
  DecompLeft u' ->
    Free u' (runObserve . k)

runSample :: SampleMap -> Freer '[Sample] a -> Sampler (a, SampleMap)
runSample sampleMap (Pure x)  = return (x, sampleMap)
runSample sampleMap (Free u k) = case u of
    PrintPatt s ->
      liftS (putStrLn s) >> runSample sampleMap (k ())
    SampPatt d α -> do
      x <- sample d
      let sampleMap' = Map.insert α (OpenSum.inj x) sampleMap
      (runSample sampleMap' . k) x
    _        -> error "Impossible: Nothing cannot occur"
