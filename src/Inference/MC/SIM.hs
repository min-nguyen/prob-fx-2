
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

import           Effects.MulDist ( Sample(..), Observe(..), MulDist )
import           Effects.EnvRW ( EnvRW )
import           Env ( Env )
import           Model ( conditionWith, MulModel )
import           Dist ( drawWithSampler )
import           Comp ( handleWith, discharge, Comp(..), LastMember, discharge1, Handler, Member, call )
import           Sampler ( Sampler, liftIO, handleImpure )
import           Unsafe.Coerce (unsafeCoerce)

-- | Simulate from a model under a given model environment
simulate
  -- | model
  :: MulModel env [EnvRW env, MulDist, Sampler] a
  -- | input model environment
  -> Env env
  -- | (model output, output environment)
  -> Sampler (a, Env env)
simulate gen_model env_in = do
  let model = conditionWith env_in gen_model
  runSimulate model

-- | Handler for simulating once from a probabilistic modelram
runSimulate
  :: Comp [Observe, Sample, Sampler] a
  -- | (model output, sample trace)
  -> Sampler a
runSimulate
  = handleImpure . defaultSample . defaultObserve

-- | Handle @Observe@ operations by simply passing forward their observed value, performing no side-effects
defaultObserve :: Handler Observe es b b
defaultObserve = handleWith () (const Val) (const hop) where
  hop :: Observe x -> (() -> x -> Comp es b) -> Comp es b
  hop (Observe d y α) k = k () y

-- | Handle @Sample@ operations by using the @Sampler@ monad to draw from primitive distributions
defaultSample ::  Member Sampler es => Handler Sample es a a
defaultSample = handleWith () (const Val) (const hop) where
  hop :: Member Sampler es => Sample x -> (() -> x -> Comp es b) -> Comp es b
  hop (Sample d α) k = do x <- call $ drawWithSampler d
                          k () x