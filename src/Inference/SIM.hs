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
module Inference.SIM where

-- import Data.Extensible hiding (Member)
import qualified Data.Map as Map
import Data.Maybe
import Data.Map (Map)
import Env
import Control.Monad
import Effects.Dist
import Prog
import Model
import Sampler
import Effects.ObsReader
import Effects.State
import Trace
import Effects.Lift
import qualified OpenSum as OpenSum
import OpenSum (OpenSum)
import Unsafe.Coerce (unsafeCoerce)
import Util
import GHC.TypeLits

simulateMany :: forall env es b a. (FromSTrace env, es ~ '[ObsReader env, Dist])
  => Int                             -- Number of iterations per data point
  -> (Model env es a)           -- Model awaiting input variable
  -> Env env                    -- List of model observed variables
  -> Sampler [(a, Env env)]
simulateMany n model env  = replicateM n (simulate model env)

simulate :: forall env es b a. (FromSTrace env, es ~ '[ObsReader env, Dist])
  => Model env es a           -- Model awaiting input variable
  -> Env env                      -- List of model observed variables
  -> Sampler (a, Env env)
simulate model env   = do
  outputs_smaps <- runSimulate env model
  let outputs_env = fmap (fromSTrace @env) outputs_smaps
  return outputs_env

runSimulate :: (es ~ '[ObsReader env, Dist])
 => Env env -> Model env es a -> Sampler (a, STrace)
runSimulate ys m
  = (runLift . runSample Map.empty . runObserve . handleDist . handleObsRead ys) (runModel m)

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
      let sTrace' = Map.insert α (PrimDist d, OpenSum.inj x) sTrace
      (runSample sTrace' . k) x
    _        -> error "Impossible: Nothing cannot occur"
