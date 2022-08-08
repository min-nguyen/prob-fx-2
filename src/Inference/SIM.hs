{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{- | Simulation.
-}

module Inference.SIM
  (-- * Inference wrapper functions
    simulate
   -- * Inference handlers
  , runSimulate
  , handleObs
  , handleSamp
  )
  where

import Effects.Dist ( Sample(..), Observe(..), Dist )
import Effects.Lift ( handleLift, Lift, lift )
import Effects.ObsRW ( ObsRW )
import Env ( Env )
import Model ( handleCore, Model )
import OpenSum (OpenSum)
import PrimDist ( sample, pattern PrimDistPrf )
import Prog ( discharge, Prog(..) )
import Sampler ( Sampler, liftIO )
import Unsafe.Coerce (unsafeCoerce)

-- | Simulate from a model under a given model environment
simulate
  -- | model
  :: Model env [ObsRW env, Dist, Lift Sampler] a
  -- | input model environment
  -> Env env
  -- | (model output, output environment)
  -> Sampler (a, Env env)
simulate model env_in = do
  let prog = handleCore env_in model
  runSimulate prog

-- | Handler for simulating once from a probabilistic program
runSimulate
  :: Prog [Observe, Sample, Lift Sampler] a
  -- | (model output, sample trace)
  -> Sampler a
runSimulate
  = handleLift . handleSamp . handleObs

-- | Handle @Observe@ operations by simply passing forward their observed value, performing no side-effects
handleObs :: Prog (Observe : es) a -> Prog es a
handleObs (Val x) = return x
handleObs (Op op k) = case discharge op of
  Right (Observe d y α) -> handleObs (k y)
  Left op' -> Op op' (handleObs . k)

-- | Handle @Sample@ operations by using the @Sampler@ monad to draw from primitive distributions
handleSamp :: Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
handleSamp (Val x) = return x
handleSamp (Op op k) = case discharge op of
  Right (Sample d α) ->
    do  x <- lift $ sample d
        handleSamp (k x)
  Left op' -> do
     Op op' (handleSamp  . k)