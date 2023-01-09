
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE PatternSynonyms #-}


{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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
import           Effects.Lift ( handleLift, Lift, lift )
import           Effects.ObsRW ( ObsRW )
import           Env ( Env )
import           Model ( handleCore, Model )
import           PrimDist ( sample )
import           Prog ( discharge, Prog(..), LastMember, discharge1 )
import           Sampler ( Sampler, liftIO )
import           Unsafe.Coerce (unsafeCoerce)

-- | Simulate from a model under a given model environment
simulate
  -- | model
  :: Model env [ObsRW env, Dist] a
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
defaultObserve
  :: Prog (Observe : es) a
  -> Prog es a
defaultObserve (Val x) = return x
defaultObserve (Op op k) = case discharge op of
  Right (Observe d y α) -> defaultObserve (k y)
  Left op' -> Op op' (defaultObserve . k)

-- | Handle @Sample@ operations by using the @Sampler@ monad to draw from primitive distributions
defaultSample
  :: Prog '[Sample] a
  -> Sampler a
defaultSample (Val x)   = return x
defaultSample (Op op k) = case discharge1 op of
  (Sample d α) -> sample d >>= defaultSample . k