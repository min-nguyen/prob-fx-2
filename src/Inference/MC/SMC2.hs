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
  let prog_0 = handleCore env model
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
  -> Model '[Observe, Sample, Sampler] a                                    -- ^ probabilistic program
  -> Comp fs [(a, PrtState)]                -- ^ final particle results and contexts
smc2Internal n_outer_prts mh_steps n_inner_prts tags  m  =
  (handleResample mh_steps n_inner_prts tags  m . SIS.pfilter n_outer_prts (PrtState (Addr "" 0) 0 Map.empty) RMPF.exec ) m

{- | A handler for resampling particles according to their normalized log-likelihoods,
     and then pertrubing their sample traces using PMMH.
-}
handleResample :: Member Sampler fs
  => Int                                           -- ^ number of PMMH steps
  -> Int                                           -- ^ number of inner SMC particles
  -> [Tag]                                      -- ^ tags indicating variables of interest
  -> Model '[Observe, Sample, Sampler] a
  -> Comp (Resample PrtState : fs) [(a, PrtState)]
  -> Comp fs [(a, PrtState)]
handleResample mh_steps n_inner_prts θ  (Model m) = loop  where
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
          wprts_mov <- mapM (\τ -> do ((prt_mov, logp), τ_mov) <- fmap head (call $ PMMH.pmmh' mh_steps n_inner_prts τ (Model partial_model))
                                      let w_mov = logp
                                      return (Model prt_mov, PrtState α w_mov τ_mov) )
                  resampled_τθs

          {- | Get:
              1) the continuations of each particle from the break point (augmented with the non-det effect)
              2) the total log weights of each particle up until the break point
              3) the sample traces of each particle up until the break point -}
          let (prts_mov, (α, ps_mov, τs_mov)) = (second unpack . unzip) wprts_mov
              wprts_norm =  zip prts_mov (pack (α, repeat (logMeanExp ps_mov), τs_mov))

          loop (k (unsafeCoerce wprts_norm))
    Left op' -> Op op' (loop  . k)