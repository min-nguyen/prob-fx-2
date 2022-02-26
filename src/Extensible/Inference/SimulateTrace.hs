{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Extensible.Inference.SimulateTrace where

-- import Data.Extensible hiding (Member)
import qualified Data.Map as Map
import Data.Maybe
import Data.Map (Map)
import Extensible.ModelEnv
import Control.Monad
import Control.Monad.Trans.Class
import Extensible.Dist
import Extensible.Freer
import Extensible.Model
import Extensible.Sampler
import Extensible.ObsReader
import Extensible.State
import Extensible.Example as Example
import qualified Extensible.OpenSum as OpenSum
import Extensible.OpenSum (OpenSum)
import Unsafe.Coerce (unsafeCoerce)
import Util
import GHC.TypeLits

type SampleMap = Map Addr (OpenSum PrimVal)

type Trace a = [(a, SampleMap)]

class SMapEnv a  where
  sMapToEnv :: SampleMap -> ModelEnv a

instance SMapEnv '[] where
  sMapToEnv _ = nil

instance (KnownSymbol x, Eq v, OpenSum.Member v PrimVal, SMapEnv env) => SMapEnv ((x := v : env) :: [Assoc Symbol *]) where
  sMapToEnv sMap = HCons (extractSamples (ObsVar @x, Proxy @v) sMap) (sMapToEnv sMap)

extractSamples ::  forall a x. (Eq a, OpenSum.Member a PrimVal) => (ObsVar x, Proxy a) -> SampleMap -> [a]
extractSamples (x, typ)  =
    map (fromJust . OpenSum.prj @a . snd)
  . Map.toList
  . Map.filterWithKey (\(tag, idx) _ -> tag == varToStr x)

simulate :: forall env ts b a. (SMapEnv env, ts ~ '[Dist, Observe, ObsReader env, Sample])
  => Int                             -- Number of iterations per data point
  -> (b -> Model env ts a)           -- Model awaiting input variable
  -> [b]                             -- List of model input variables
  -> [ModelEnv env]                      -- List of model observed variables
  -> Sampler [(a, ModelEnv env)]
simulate n model xs envs = do
  let runN (x, env) = replicateM n (runSimulate env (model x))
  outputs_smaps <- concat <$> mapM runN (zip xs envs)
  let outputs_envs = map (fmap (sMapToEnv @env)) outputs_smaps
  return outputs_envs

runSimulate :: (ts ~ '[Dist, Observe, ObsReader env, Sample])
 => ModelEnv env -> Model env ts a -> Sampler (a, SampleMap)
runSimulate ys m
  = (runSample Map.empty . runObsReader ys . runObserve . runDist) (runModel m)

simulateWith :: (ts ~ '[Dist, Observe, ObsReader env, Sample])
  => Int                             -- Number of iterations per data point
  -> (b -> Model env (t:ts) a)       -- Model awaiting input variable
  -> [b]                             -- List of model input variables
  -> [ModelEnv env]                      -- List of model observed variables
  -> (Model env (t:ts) a -> Model env ts c)
  -> Sampler [(c, SampleMap)]
simulateWith n model xs envs h = do
  let runN (x, env) = replicateM n (runSimulateWith env (model x) h)
  concat <$> mapM runN (zip xs envs)

runSimulateWith :: (ts ~ '[Dist, Observe, ObsReader env, Sample])
 => ModelEnv env
 -> Model env (t:ts) a
 -> (Model env (t:ts) a -> Model env ts c)
 -> Sampler (c, SampleMap)
runSimulateWith ys m h
  = (runSample Map.empty . runObsReader ys . runObserve . runDist) (runModel $ h m)

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
