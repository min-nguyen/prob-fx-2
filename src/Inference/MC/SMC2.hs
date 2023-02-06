{-# LANGUAGE RankNTypes #-}


{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}


{- Sequential Monte Carlo^2 inference.
-}

module Inference.MC.SMC2 where

import qualified Data.Map as Map
import Data.Map (Map)
import Env
import Comp
import Model
import Sampler
import Effects.Dist
import Effects.EnvRW
import Effects.NonDet
import qualified Inference.MC.SSMH as SSMH
import qualified Inference.MC.PMMH as PMMH
import           Inference.MC.RMPF as RMPF (PrtState(..), exec, suspendAt, unpack, pack)
import qualified Inference.MC.SMC as SMC
import qualified Inference.MC.SIM as SIM
import qualified Inference.MC.SIS as SIS hiding  (particleLogProb)
import Inference.MC.SIS (Resample(..), ParticleHandler)
import Inference.MC.MH as MH
import Data.Bifunctor
import Trace (filterTrace)
import LogP
import Unsafe.Coerce (unsafeCoerce)
import Inference.MC.SIM (defaultSample)

{- | Top-level wrapper for SMC2 inference.
-}
smc2 :: forall env es a xs. (env `ContainsVars` xs)
  => Int                                            -- ^ number of outer SMC particles
  -> Int                                            -- ^ number of PMMH steps
  -> Int                                            -- ^ number of inner SMC particles
  -> GenModel env [EnvRW env, Dist, Sampler] a                  -- ^ model
  -> Env env                                        -- ^ input environment
  -> Vars xs                                        -- ^ optional observable variable names of interest
  -> Sampler [Env env]                              -- ^ output environments
smc2 n_outer_prts mh_steps n_inner_prts model env obs_vars = do
  -- | Handle model to probabilistic program
  let prog_0 = (handleDist . handleEnvRW env) (runModel model)
  -- | Convert observable variables to strings
      tags = varsToStrs @env obs_vars
  -- | Run SMC2do
  smc2_trace <- handleIO (smc2Internal n_outer_prts mh_steps n_inner_prts tags  prog_0)
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
  (handleResample mh_steps n_inner_prts tags  m . SIS.sis n_outer_prts RMPF.exec (PrtState (Addr "" 0) 0 Map.empty)) m

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
handleResample mh_steps n_inner_prts θ  m = loop  where
  loop  (Val x) = Val x
  loop  (Op op k) = case discharge op of
    Right  (Resample (prts, σs)) ->
      do  -- | Resample the particles according to the indexes returned by the SMC resampler
          let (α, ρs, τs ) = unpack σs
          idxs <- call $ SMC.resampleMul ρs
          let resampled_τs      = map (τs !! ) idxs
          -- | Get the parameter sample trace of each resampled particle
              resampled_τθs     = map (filterTrace θ) resampled_τs
          -- | Insert break point to perform SSMH up to
              partial_model     = suspendAt α m
          -- | Perform PMMH using each resampled particle's sample trace and get the most recent PMMH iteration.
          pmmh_trace <- mapM ( fmap head
                             . call
                             . flip (PMMH.pmmh' mh_steps n_inner_prts) (unsafeCoerce partial_model)
                             ) resampled_τθs
          {- | Get:
              1) the continuations of each particle from the break point (augmented with the non-det effect)
              2) the total log weights of each particle up until the break point
              3) the sample traces of each particle up until the break point -}
          let ((rejuv_prts, rejuv_lps), rejuv_traces) = first unzip (unzip pmmh_trace)
              rejuv_lps_normalised  = map (const (logMeanExp rejuv_lps)) rejuv_lps

              rejuv_ss    = pack (α, rejuv_lps_normalised, rejuv_traces)

          (loop . k) (zip rejuv_prts rejuv_ss)
    Left op' -> Op op' (loop  . k)