
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE PatternSynonyms #-}


{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}

{- | Simulation.
-}

module Inference.MC.SIM
  (-- * Inference wrapper functions
    simulate
   -- * Inference handlers
  , runSimulate
  , defaultObserve
  , defaultSample
  )
  where

import           Effects.Dist ( Sample(..), Observe(..), Dist )
import           Effects.Lift ( handleM )
import           Effects.EnvRW ( EnvRW )
import           Env ( Env )
import           Model ( handleCore, Model )
import           PrimDist ( drawWithSampler )
import           Prog ( handle, discharge, Prog(..), LastMember, discharge1, Handler )
import           Sampler ( Sampler, liftIO )
import           Unsafe.Coerce (unsafeCoerce)

-- | Simulate from a model under a given model environment
simulate
  -- | model
  :: Model env [EnvRW env, Dist] a
  -- | input model environment
  -> Env env
  -- | (model output, output environment)
  -> Sampler (a, Env env)
simulate model env_in = do
  let prog = handleCore env_in model
  runSimulate prog

-- | Handler for simulating once from a probabilistic program
runSimulate
  :: Prog [Observe, Sample] a
  -- | (model output, sample trace)
  -> Sampler a
runSimulate
  = defaultSample . defaultObserve

-- | Handle @Observe@ operations by simply passing forward their observed value, performing no side-effects
defaultObserve :: Handler Observe es b b
defaultObserve = handle () (const Val) (const hop)
  where
  hop :: Observe x -> (() -> x -> Prog es b) -> Prog es b
  hop (Observe d y α) k = k () y

-- | Handle @Sample@ operations by using the @Sampler@ monad to draw from primitive distributions
defaultSample
  :: Prog '[Sample] a
  -> Sampler a
defaultSample (Val x)   = return x
defaultSample (Op op k) = case discharge1 op of
  (Sample d α) -> drawWithSampler d >>= (defaultSample . k)