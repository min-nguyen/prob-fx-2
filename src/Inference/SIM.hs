{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Inference.SIM where

import Data.Map (Map)
import Data.Maybe
import Effects.Dist
import Effects.ObsReader
import Effects.State
import Effects.Lift
import Env
import GHC.TypeLits
import Model
import OpenSum (OpenSum)
import PrimDist
import Prog
import qualified Data.Map as Map
import qualified OpenSum as OpenSum
import Sampler
import Trace
import Unsafe.Coerce (unsafeCoerce)

-- ||| (Section 6.1) Simulation
simulate :: forall env es b a. (FromSTrace env, es ~ '[ObsReader env, Dist, Lift Sampler])
  => 
  -- | A model 
     Model env es a
  -- | A model environment
  -> Env env                 
  -- | Model output and output environment  
  -> Sampler (a, Env env)   
simulate model env = do
  outputs_strace <- runSimulate env model
  return (fmap fromSTrace outputs_strace)

-- | SIM handler
runSimulate :: (es ~ '[ObsReader env, Dist, Lift Sampler])
 => Env env -> Model env es a -> Sampler (a, STrace)
runSimulate env 
  = handleLift . handleSamp . handleObs . traceSamples . handleCore env
 
handleObs :: Prog (Observe : es) a -> Prog es a
handleObs (Val x) = return x
handleObs (Op op k) = case discharge op of
  Right (Observe d y α) -> handleObs (k y)
  Left op' -> Op op' (handleObs . k)

handleSamp :: Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
handleSamp  (Val x)  = return x
handleSamp  (Op op k) = case discharge op of
  Right (Sample (PrimDistDict d) α) ->
    do  x <- lift $ sample d
        handleSamp (k x)
  _        -> error "Impossible: Nothing cannot occur"
