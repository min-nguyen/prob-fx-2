{-# LANGUAGE RankNTypes #-}


{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

{- Rejuvenate-Move Sequential Monte Carlo inference.
-}

module Inference.MC.RMPF where

import qualified Data.Map as Map
import           Data.Map (Map)
import           Env
import           Comp
import           Model
import           Sampler
import           Trace  (Trace, filterTrace)
import           LogP
import           Control.Monad
import           Control.Applicative
import           Effects.MulDist
import           Effects.EnvRW
import           Effects.NonDet
import qualified Inference.MC.SSMH as SSMH
import qualified Inference.MC.SMC as SMC
import qualified Inference.MC.SIM as SIM
import           Inference.MC.MH as MH
import qualified Inference.MC.SIS as SIS hiding  (particleLogProb)
import           Inference.MC.SIS (Resample(..), ModelStep, pfilter)

import           Dist
import           Data.Bifunctor
import           Unsafe.Coerce
import           Util
import Inference.MC.SIM (defaultSample)
import Inference.MC.SMC (advance)
import qualified Data.Vector as Vector

{- | The particle context for an MCMC trace.
-}
type PrtState =  (LogP, Trace)

{- | Call RMPF on a model.
-}
rmpf :: forall env a xs. (env `ContainsVars` xs)
  => Int                                          -- ^ number of SMC particles
  -> Int                                          -- ^ number of SSMH (rejuvenation) steps
  -> MulModel env [EnvRW env, MulDist, Sampler] a                -- ^ model
  -> Env env                                      -- ^ input model environment
  -> Vars xs                                      -- ^ optional observable variable names of interest
  -> Sampler [Env env]                            -- ^ output model environments of each particle
rmpf n_prts mh_steps gen_model env_in obs_vars = do
  -- | Handle model to probabilistic program
  let model   = conditionWith env_in gen_model
  -- | Convert observable variables to strings
      tags = varsToStrs @env obs_vars
  map (snd . fst) <$> rmpf' n_prts mh_steps tags model

{- | Call RMPF on a probabilistic program.
-}
rmpf' ::
     Int                                          -- ^ number of SMC particles
  -> Int                                          -- ^ number of SSMH (rejuvenation) steps
  -> [Tag]                                        -- ^ tags indicating variables of interest
  -> Model '[Sampler] a                                   -- ^ probabilistic program
  -> Sampler [(a, PrtState)]                      -- ^ final particle results and contexts
rmpf' n_prts mh_steps tags model = do
  -- let q =  pfilter exec model (prts, ps)
  (runImpure . handleResample mh_steps tags model . pfilter n_prts (0, Map.empty) exec ) model

{- | A handler that records the values generated at @Sample@ operations and invokes a breakpoint
     at the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation, its breakpoint address, and the particle's sample trace
-}
exec :: ModelStep '[Sampler] PrtState a
exec (p, (w, τ))  = (fmap asPrtTrace . runImpure . reuseTrace τ . advance w) p where
  asPrtTrace ((prt, w), τ) = (prt, (w, τ))

{- | A handler for resampling particles according to their normalized log-likelihoods, and then pertrubing their sample traces using SSMH.
-}
handleResample :: (Member Sampler fs)
  => Int                                          -- ^ number of SSMH (rejuvenation) steps
  -> [Tag]                                        -- ^ tags indicating variables of interest
  -> Model '[Sampler] a
  -> Handler (Resample PrtState) fs [(a, PrtState)] [(a, PrtState)]
handleResample mh_steps tags  m = handleWith 0 (const Val) hop where
  hop :: Member Sampler fs => Int -> Resample PrtState x -> (Int -> x -> Comp fs a) -> Comp fs a
  hop t (Resample pwτs) k = do
    let (ws, τs) = (unzip . map snd) pwτs
        -- | Compute the sum of all particles' probabilities (in LogP form, i.e. their logSumExp)
        z        = logSumExp ws
    if  -- | Require at least some particles' probabilities to be greater than zero
        not (isInfinite z)
      then do
        let -- | Normalise the particle weights (dividing by the total probability)
            ws_norm  = map (exp . \w -> w - z) ws
            n        = length ws
        idxs <- call $ (replicateM n . Sampler.sampleCategorical) (Vector.fromList ws_norm)
        let -- | Resample the traces.
            τs_res   = map (τs !!) idxs
            -- | Get average particle probability (in LogP form, i.e. their logMeanExp)
            w_avg    = z - log (fromIntegral n)
            -- | Insert break point to perform SSMH up to.
            model_t  = suspendAfter t m
        -- | For each resampled particle's trace
        pwτs_mov <- forM τs_res (\τ ->
          do  -- | Perform a series of SSMH-updates on the trace and get most recent moved trace.
              ((p_mov, _), τ_mov) <- fmap head (call $ SSMH.ssmh mh_steps τ tags model_t)
              -- | Set all particles to use the supposed pre-SSMH-move weight, following the same procedure as SMC
              return (p_mov, (w_avg, τ_mov)))
        k (t + 1) (unsafeCoerce pwτs_mov)
      else
        k (t + 1) pwτs

{- | A handler that invokes a breakpoint upon matching against the @Observe@ operation with a specific address.
     It returns the rest of the computation.
-}
suspendAfter :: (Member Observe es)
  => Int       -- ^ Address of @Observe@ operation to break at
  -> Comp es a
  -> Comp es (Comp es a)
suspendAfter t (Val x)   = pure (Val x)
suspendAfter t (Op op k) = case prj op of
  Just (Observe d y α) -> do
    if t <= 0
      then Val (k y)
      else Op op (suspendAfter (t - 1) . k)
  _ -> Op op (suspendAfter t . k)

