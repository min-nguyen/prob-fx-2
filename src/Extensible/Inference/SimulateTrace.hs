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
import Extensible.STrace
import Extensible.Example as Example
import qualified Extensible.OpenSum as OpenSum
import Extensible.OpenSum (OpenSum)
import Unsafe.Coerce (unsafeCoerce)
import Util
import GHC.TypeLits

simulate :: forall env es b a. (FromSTrace env, es ~ '[ObsReader env, Dist])
  => Int                             -- Number of iterations per data point
  -> (b -> Model env es a)           -- Model awaiting input variable
  -> [b]                             -- List of model input variables
  -> [ModelEnv env]                      -- List of model observed variables
  -> Sampler [(a, ModelEnv env)]
simulate n model xs envs = do
  let runN (x, env) = replicateM n (runSimulate env (model x))
  outputs_smaps <- concat <$> mapM runN (zip xs envs)
  let outputs_envs = map (fmap (fromSTrace @env)) outputs_smaps
  return outputs_envs

runSimulate :: (es ~ '[ObsReader env, Dist])
 => ModelEnv env -> Model env es a -> Sampler (a, STrace)
runSimulate ys m
  = (runSample Map.empty . runObserve . runDist . runObsReader ys) (runModel m)

simulateWith :: (es ~ '[ObsReader env, Dist])
  => Int                             -- Number of iterations per data point
  -> (b -> Model env (e:es) a)       -- Model awaiting input variable
  -> [b]                             -- List of model input variables
  -> [ModelEnv env]                      -- List of model observed variables
  -> (Model env (e:es) a -> Model env es c)
  -> Sampler [(c, STrace)]
simulateWith n model xs envs h = do
  let runN (x, env) = replicateM n (runSimulateWith env (model x) h)
  concat <$> mapM runN (zip xs envs)

runSimulateWith :: (es ~ '[ObsReader env, Dist])
 => ModelEnv env
 -> Model env (e:es) a
 -> (Model env (e:es) a -> Model env es c)
 -> Sampler (c, STrace)
runSimulateWith ys m h
  = (runSample Map.empty . runObserve . runDist . runObsReader ys  ) (runModel $ h m)

runObserve :: Prog (Observe : es) a -> Prog es  a
runObserve (Val x) = return x
runObserve (Op u k) = case u of
  ObsPatt d y α ->
    let p = logProb d y
    in  runObserve (k y)
  DecompLeft u' ->
    Op u' (runObserve . k)

runSample :: STrace -> Prog '[Sample] a -> Sampler (a, STrace)
runSample sTrace (Val x)  = return (x, sTrace)
runSample sTrace (Op u k) = case u of
    PrintPatt s ->
      liftS (putStrLn s) >> runSample sTrace (k ())
    SampPatt d α -> do
      x <- sample d
      let sTrace' = Map.insert α (OpenSum.inj x) sTrace
      (runSample sTrace' . k) x
    _        -> error "Impossible: Nothing cannot occur"
