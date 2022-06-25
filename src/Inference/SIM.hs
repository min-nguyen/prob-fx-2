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
import PrimDist as PrimDist
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
  = (handleLift . handleSamp . handleObs . traceSamples . handleDist . handleObsRead ys) (runModel m)

handleObs :: Prog (Observe : es) a -> Prog es  a
handleObs (Val x) = return x
handleObs (Op op k) = case discharge op of
  Right (Observe d y α) -> handleObs (k y)
  Left op' -> Op op' (handleObs . k)

handleSamp :: Prog '[Sample] a -> Prog '[Lift Sampler] a
handleSamp (Val x)  = return x
handleSamp (Op u k) = case u of
    PrintPatt s -> do
      (lift . liftIOSampler) (putStrLn s)
      handleSamp  (k ())
    SampPatt d α -> do
      x <- lift (sample d)
      (handleSamp . k) x
    _        -> error "Impossible: Nothing cannot occur"
