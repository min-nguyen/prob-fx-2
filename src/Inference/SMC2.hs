{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{- Sequential Monte Carlo^2 inference.
-}

module Inference.SMC2 where

import qualified Data.Map as Map
import Data.Map (Map)
import Env
import Prog
import Model
import Sampler
import Effects.Dist
import Effects.ObsRW
import Effects.NonDet
import qualified Inference.MH as MH
import qualified Inference.PMMH as PMMH
import qualified Inference.RMSMC as RMSMC
import Inference.RMSMC ( TracedParticle(..) )
import qualified Inference.SMC as SMC
import qualified Inference.SIM as SIM
import qualified Inference.SIS as SIS hiding  (particleLogProb)
import Inference.SIS (Resample(..), ParticleResampler, ParticleHandler, ParticleCtx (..))
import Inference.SMC (Particle, pattern Particle)
import Inference.ARS as ARS
import Effects.Lift
import Data.Bifunctor

{- | Top-level wrapper for SMC2 inference.
-}
smc2 :: forall env es a xs. (env `ContainsVars` xs)
  => Int                                            -- ^ number of outer SMC particles
  -> Int                                            -- ^ number of MH steps
  -> Int                                            -- ^ number of inner SMC particles
  -> Model env [ObsRW env, Dist, Lift Sampler] a    -- ^ model
  -> Env env                                        -- ^ input environment
  -> Vars xs                                        -- ^ variable names of model parameters
  -> Sampler [Env env]                              -- ^ output environments
smc2 n_outer_particles mh_steps n_inner_particles model env obs_vars = do
  let prog = (handleDist . handleObsRW env) (runModel model)
  -- | Convert observable variables to strings
      tags = varsToStrs @env obs_vars
  -- | Run SMC2
  smc2_trace <- smc2Internal n_outer_particles mh_steps n_inner_particles tags prog
  -- Return the accepted model environments
  pure (map (snd . fst) smc2_trace)

{- | Perform SMC2 on a probabilistic program.
-}
smc2Internal
  :: Int                                          -- ^ number of outer SMC particles
  -> Int                                          -- ^ number of MH steps
  -> Int                                          -- ^ number of inner SMC particles
  -> [Tag]                                        -- ^ tags indicating model parameters
  -> Prog [Observe, Sample, Lift Sampler] a       -- ^ probabilistic program
  -> Sampler [(a, TracedParticle)]                  -- ^ final particle results and contexts
smc2Internal n_outer_particles mh_steps n_inner_particles tags = do
  handleLift . SIM.handleSamp . SIM.handleObs
    . SIS.sis n_outer_particles RMSMC.handleParticle (handleResample mh_steps n_inner_particles tags)

{- | A handler for resampling particles according to their normalized log-likelihoods, and then pertrubing their sample traces using PMMH.
-}
handleResample :: ProbSig es
  => Int -> Int -> [String] -> (Prog (Resample es TracedParticle : es) a -> Prog es a)
handleResample mh_steps n_inner_particles tags = loop where
  loop (Val x) = Val x
  loop (Op op k) = case discharge op of
    Right (Resample (prts, ctxs) prog_0) ->
      do  -- | Resample the particles according to the indexes returned by the SMC resampler
          idxs <- snd <$> SMC.handleResample (call (Resample ([], map (Particle . particleLogProb) ctxs) prog_0))
          let resampled_ctxs    = map (ctxs !! ) idxs
          -- | Get the observe address at the breakpoint (from the context of any arbitrary particle, e.g. by using 'head')
              resampled_α       = (particleObsAddr . head) resampled_ctxs
          -- | Get the sample trace of each resampled particle
              resampled_straces = map particleSTrace resampled_ctxs
          -- | Insert break point to perform MH up to
              partial_model     = RMSMC.breakObserve resampled_α prog_0
          -- | Perform PMMH using each resampled particle's sample trace and get the most recent PMMH iteration.
          pmmh_trace <- mapM ( fmap head
                             . PMMH.handleAccept tags
                             . ARS.arInternal mh_steps partial_model (PMMH.handleModel n_inner_particles tags)
                             ) resampled_straces
          {- | Get:
              1) the continuations of each particle from the break point (augmented with the non-det effect)
              2) the total log weights of each particle up until the break point
              3) the sample traces of each particle up until the break point -}
          let ((rejuv_prts, rejuv_lps), rejuv_straces) = first unzip (unzip pmmh_trace)

              rejuv_ctxs    = zipWith3 TracedParticle rejuv_lps (repeat resampled_α) rejuv_straces

          (loop . k) ((map weakenProg rejuv_prts, rejuv_ctxs), idxs)
    Left op' -> Op op' (loop . k)