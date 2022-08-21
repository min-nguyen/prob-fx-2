{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

{- Sequential Monte Carlo^2 inference.
-}

module Inference.SMC2 where

import qualified Data.Map as Map
import Data.Map (Map)
import Env
import Prog
import Model
import Sampler
import Trace
import LogP
import Control.Monad
import Control.Applicative
import Effects.Dist
import Effects.ObsRW
import Effects.NonDet
import qualified Inference.MH as MH
import qualified Inference.PMMH as PMMH
import qualified Inference.RMSMC as RMSMC
import Inference.RMSMC ( RMSMCParticle(..) )
import qualified Inference.SMC as SMC
import qualified Inference.SIM as SIM
import qualified Inference.SIS as SIS hiding  (particleLogProb)
import Inference.SIS (Resample(..), ParticleResampler, ParticleRunner, ParticleCtx (..))
import OpenSum (OpenSum)
import Inference.SMC (SMCParticle, pattern SMCParticle)
import Effects.Lift
import PrimDist
import Data.Bifunctor
import Unsafe.Coerce
import Util

type SMC2Particle = RMSMC.RMSMCParticle

particleResampler :: Int -> Int -> [String] -> ParticleResampler SMC2Particle
particleResampler mh_steps n_inner_particles tags = loop where
  loop (Val x) = Val x
  loop (Op op k) = case discharge op of
    Right (Resample (prts, ctxs, prog_0)) ->
      do  -- | Resample the particles according to the indexes returned by the SMC resampler
          idxs <- snd <$> SMC.particleResampler (call (Resample ([], map (SMCParticle . particleLogProb) ctxs, prog_0)))
          let resampled_prts   = map (prts !! ) idxs
              resampled_ctxs   = map (ctxs !! ) idxs

          -- | Get the trace of observe addresses up until the breakpoint
          --   (from the context of any arbitrary particle, e.g. by using 'head')
          let α_obs   = (particleObsAddrs . head) resampled_ctxs
          -- | Insert break point to perform MH up to
              partial_model = RMSMC.breakObserve α_obs prog_0
          -- | Perform PMMH using each resampled particle's sample trace and get the most recent MH iteration.
          pmmh_trace <- lift $ mapM ( fmap head
                                  . flip (PMMH.pmmhInternal mh_steps n_inner_particles partial_model) tags
                                  . particleSTrace) resampled_ctxs
          {- | Get:
              1) the continuations of each particle from the break point (augmented with the non-det effect)
              2) the total log weights of each particle up until the break point
              3) the sample traces of each particle up until the break point -}
          let ((rejuv_prts, rejuv_logps), rejuv_straces) = first unzip (unzip pmmh_trace)

              rejuv_ctxs    = zipWith3 RMSMCParticle rejuv_logps (repeat α_obs) rejuv_straces

          (loop . k) ((map (weakenProg @(Resample SMC2Particle)) rejuv_prts, rejuv_ctxs), idxs)

    Left op' -> Op op' (loop . k)