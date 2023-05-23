{-# LANGUAGE RankNTypes #-}


{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}


{- Sequential Monte Carlo^2 inference.
-}

module Inference.MC.SMC2 where

import qualified Data.Map as Map
import Data.Map (Map)
import Env
import Comp
import Model
import Sampler
import Effects.MulDist
import Effects.EnvRW
import Effects.NonDet
import qualified Inference.MC.SSMH as SSMH
import qualified Inference.MC.PMMH as PMMH
import           Inference.MC.RMPF as RMPF (PrtState(..), exec, suspendAfter)
import qualified Inference.MC.SMC as SMC
import qualified Inference.MC.SIM as SIM
import qualified Inference.MC.SIS as SIS hiding  (particleLogProb)
import Inference.MC.SIS (Resample(..), ModelStep)
import Inference.MC.MH as MH
import Data.Bifunctor
import Trace (filterTrace)
import LogP
import Unsafe.Coerce (unsafeCoerce)
import Inference.MC.SIM (defaultSample)
import qualified Data.Vector as Vector
import Control.Monad (replicateM)

{- | Top-level wrapper for SMC2 inference.
-}
smc2 :: forall env es a xs. (env `ContainsVars` xs)
  => Int                                            -- ^ number of outer SMC particles
  -> Int                                            -- ^ number of PMMH steps
  -> Int                                            -- ^ number of inner SMC particles
  -> MulModel env [EnvRW env, MulDist, Sampler] a                  -- ^ model
  -> Env env                                        -- ^ input environment
  -> Vars xs                                        -- ^ optional observable variable names of interest
  -> Sampler [Env env]                              -- ^ output environments
smc2 n_outer_prts mh_steps n_inner_prts gen_model env obs_vars = do
  -- | Handle model to probabilistic program
  let model = (handleMulDist . handleEnvRW env) (runModel gen_model)
  -- | Convert observable variables to strings
      tags = varsToStrs @env obs_vars
  -- | Run SMC2do
  smc2_trace <- runImpure (smc2Internal n_outer_prts mh_steps n_inner_prts tags  model)
  -- Return the accepted model environments
  pure (map (snd . fst) smc2_trace)

{- | Perform SMC2 on a probabilistic program.
-}
smc2Internal :: (Member Sampler fs)
  => Int                                          -- ^ number of outer SMC particles
  -> Int                                          -- ^ number of PMMH steps
  -> Int                                          -- ^ number of inner SMC particles
  -> [Tag]                                        -- ^ tags indicating variables of interest
  -> Model '[Sampler] a                                    -- ^ probabilistic program
  -> Comp fs [(a, PrtState)]                -- ^ final particle results and contexts
smc2Internal n_outer_prts mh_steps n_inner_prts tags  m  =
  (handleResample mh_steps n_inner_prts tags  m . SIS.pfilter n_outer_prts (0, Map.empty) RMPF.exec ) m

{- | A handler for resampling particles according to their normalized log-likelihoods,
     and then pertrubing their sample traces using PMMH.
-}
handleResample :: Member Sampler fs
  => Int                                           -- ^ number of PMMH steps
  -> Int                                           -- ^ number of inner SMC particles
  -> [Tag]                                      -- ^ tags indicating variables of interest
  -> Model '[Sampler] a
  -> Comp (Resample PrtState : fs) [(a, PrtState)]
  -> Comp fs [(a, PrtState)]
handleResample mh_steps n_inner_prts θ  m = loop (0 :: Int) where
  loop t (Val x) = Val x
  loop t (Op op k) = case discharge op of
    Right  (Resample pσs) ->
      do  -- | Resample the particles according to the indexes returned by the SMC resampler
          let (ws, τs) = (unzip . map snd) pσs
        -- | Compute the sum of all particles' probabilities (in LogP form, i.e. their logSumExp)
              z        = logSumExp ws
          if  -- | Require at least some particles' probabilities to be greater than zero
              not (isInfinite z)
          then do
            let -- | Normalise the particles' probabilities (by dividing by their total)
                ws_norm  = map (exp . \w -> w - z) ws
                n        = length ws
            idxs <- call $ (replicateM n . Sampler.sampleCategorical) (Vector.fromList ws_norm)
            -- | Get the parameter sample trace of each resampled particle
            let resampled_τs      = map (τs !! ) idxs
                resampled_τθs     = map (filterTrace θ) resampled_τs
            -- | Insert break point to perform SSMH up to
                partial_model     = suspendAfter t m
            -- | Perform PMMH using each resampled particle's sample trace and get the most recent PMMH iteration.
            pmmh_trace <- mapM ( fmap head
                              . call
                              . flip (PMMH.pmmh mh_steps n_inner_prts) (unsafeCoerce partial_model)
                              ) resampled_τθs
            {- | Get:
                1) the continuations of each particle from the break point
                2) the sample traces of each particle up until the break point -}
            let ((ps_mov, _), τs_mov) = first unzip (unzip pmmh_trace)
            -- | Get average particle probability (in LogP form, i.e. their logMeanExp)
                w_avg    = z - log (fromIntegral n)
            -- | Set all particles to use the supposed pre-SSMH-move weight, following the same procedure as SMC
                pσs_mov = zip ps_mov (map (w_avg, ) τs_mov)
            (loop (t + 1) . k) pσs_mov
          else
            loop (t + 1) . k $ pσs
    Left op' -> Op op' (loop t . k)