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
module Inference.Simulate where

-- import Data.Extensible hiding (Member)
import qualified Data.Map as Map
import Data.Maybe
import Data.Map (Map)
import ModelEnv
import Control.Monad
import Dist
import Freer
import Model
import Sampler
import ObsReader
import State
import STrace
import IO
import qualified OpenSum as OpenSum
import OpenSum (OpenSum)
import Unsafe.Coerce (unsafeCoerce)
import Util
import GHC.TypeLits

simulate :: forall env es b a. (FromSTrace env, es ~ '[ObsReader env, Dist])
  => Int                             -- Number of iterations per data point
  -> (b -> Model env es a)           -- Model awaiting input variable
  -> b                               -- List of model input variables
  -> ModelEnv env                    -- List of model observed variables
  -> Sampler [(a, ModelEnv env)]
simulate n model x env = do
  outputs_smaps <- replicateM n (runSimulate env (model x))
  let outputs_envs = map (fmap (fromSTrace @env)) outputs_smaps
  return outputs_envs

simulateOnce :: forall env es b a. (FromSTrace env, es ~ '[ObsReader env, Dist])
  => (b -> Model env es a)           -- Model awaiting input variable
  -> ModelEnv env                      -- List of model observed variables
  -> b                             -- List of model input variables
  -> Sampler (a, ModelEnv env)
simulateOnce model env x  = do
  outputs_smaps <- runSimulate env (model x)
  let outputs_env = fmap (fromSTrace @env) outputs_smaps
  return outputs_env

runSimulate :: (es ~ '[ObsReader env, Dist])
 => ModelEnv env -> Model env es a -> Sampler (a, STrace)
runSimulate ys m
  = (runLift . runSample Map.empty . runObserve . runDist . runObsReader ys) (runModel m)

runObserve :: Prog (Observe : es) a -> Prog es  a
runObserve (Val x) = return x
runObserve (Op u k) = case u of
  ObsPatt d y α ->
    let p = logProb d y
    in  runObserve (k y)
  DecompLeft u' ->
    Op u' (runObserve . k)

runSample :: STrace -> Prog '[Sample] a -> Prog '[Lift Sampler] (a, STrace)
runSample sTrace (Val x)  = return (x, sTrace)
runSample sTrace (Op u k) = case u of
    PrintPatt s -> do
      (lift . liftS) (putStrLn s)
      runSample sTrace (k ())
    SampPatt d α -> do
      x <- lift (sample d)
      let sTrace' = Map.insert α (OpenSum.inj x) sTrace
      (runSample sTrace' . k) x
    _        -> error "Impossible: Nothing cannot occur"
