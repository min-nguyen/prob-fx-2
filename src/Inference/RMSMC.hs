{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module Inference.RMSMC where

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
import qualified Inference.SMC as SMC
import qualified Inference.SIM as SIM
import qualified Inference.SIS as SIS hiding  (particleLogProb)
import Inference.SIS (Resample(..), ParticleResampler, ParticleRunner, ParticleCtx)
import OpenSum (OpenSum)
import Inference.SMC (SMCParticle, pattern SMCParticle)
import Effects.Lift
import PrimDist
import Data.Bifunctor
import Unsafe.Coerce
import Util

{- | The particle context for RMSMC
-}
data RMSMCParticle = RMSMCParticle {
    particleLogProb   :: LogP
  , particleObsAddrs  :: [Addr]
  , particleTrace     :: InvSTrace
  }

instance ParticleCtx RMSMCParticle where
  pempty            = RMSMCParticle 0 [] Map.empty
  paccum ctxs ctxs' =
    -- | Compute normalised accumulated log weights
    let log_ps   = let logZ = logMeanExp (map particleLogProb ctxs)
                   in  map ((+ logZ) . particleLogProb) ctxs'
        α_obs    = uncurry (zipWith (++)) (bimap' (map particleObsAddrs) (ctxs', ctxs))
        straces  = zipWith Map.union (map particleTrace ctxs') (map particleTrace ctxs)
    in  zipWith3 RMSMCParticle log_ps α_obs straces

rmsmc
  :: Int                                          -- ^ number of SMC particles
  -> Int                                          -- ^ number of MH steps
  -> Model env [ObsRW env, Dist, Lift Sampler] a  -- ^ model
  -> Env env                                      -- ^ input model environment
  -> Sampler [Env env]                            -- ^ output model environments of each particle
rmsmc n_particles mh_steps model env = do
  let prog = (handleDist . handleObsRW env) (runModel model)
  rmsmcInternal n_particles mh_steps prog env >>= pure . map (snd . fst)

rmsmcInternal
  :: Int
  -> Int
  -> Prog [Observe, Sample, Lift Sampler] a
  -> Env env
  -> Sampler [(a, RMSMCParticle)]
rmsmcInternal n_particles mh_steps prog env = do
  SIS.sis n_particles (particleRunner Map.empty) (particleResampler mh_steps prog) (weakenProg @(Resample RMSMCParticle) prog)

{- | A handler for resampling particles according to their normalized log-likelihoods.
-}
particleResampler :: Int -> Prog [Observe, Sample, Lift Sampler] a -> ParticleResampler RMSMCParticle
particleResampler mh_steps prog_0 (Val x) = Val x
particleResampler mh_steps prog_0 (Op op k) = case discharge op of
  Right (Resample (prts, ctxs)) ->
    do  -- | Resample the RMSMC particles according to the indexes returned by the SMC resampler
        lift $ liftIO $ print ("before resampling" ++ show (map particleTrace ctxs))
        idxs <- snd <$> SMC.particleResampler (call (Resample (prts, map (SMCParticle . particleLogProb) ctxs)))
        let resampled_prts   = map (prts !! ) idxs
            resampled_ctxs   = map (ctxs !! ) idxs

        lift $ liftIO $ print ("after resampling, before mh" ++ show (map particleTrace resampled_ctxs))
        -- | Get the trace of observe addresses up until the breakpoint
        --   (from the context of any arbitrary particle, e.g. by using 'head')
        let α_obs   = (particleObsAddrs . head) resampled_ctxs
        -- | Get the observe address of the breakpoint
            α_break = head α_obs
        -- | Insert break point to perform MH up to
            partial_model = breakObserve α_break prog_0
        -- | Perform MH using each resampled particle's sample trace and get the most recent MH iteration.
        mh_trace <- lift $ mapM ( fmap head
                                . flip (MH.mhInternal mh_steps partial_model) []
                                . particleTrace) resampled_ctxs
        lift $ liftIO $ print ("after mh" ++ show (map snd mh_trace))
        {- | Get:
            1) the continuations of each particle from the break point (augmented with the non-det effect)
            2) the log prob traces of each particle up until the break point
            3) the sample traces of each particle up until the break point -}
        let ((rejuv_prts, lp_traces), rejuv_straces) = first unzip (unzip mh_trace)
            -- | Filter log probability traces to only include that for observe operations
            lp_traces_obs = map (filterByKey (`elem` α_obs)) lp_traces
            -- | Recompute the log weights of all particles up until the break point
            rejuv_lps     = map (sum . map snd . Map.toList) lp_traces_obs

            rejuv_ctxs    = zipWith3 RMSMCParticle rejuv_lps (repeat α_obs) rejuv_straces

        (particleResampler mh_steps prog_0 . k)
            ((resampled_prts -- | TO FIX
              , rejuv_ctxs), idxs)

  Left op' -> Op op' (particleResampler mh_steps prog_0 . k)

particleRunner :: InvSTrace -> ParticleRunner RMSMCParticle
particleRunner inv_strace (Val x) = pure (Val x, RMSMCParticle 0 [] Map.empty)
particleRunner inv_strace (Op op k) = case op of
  SampPrj d α  -> do r <- lift sampleRandom
                     y <- lift (sampleInv d r)
                     particleRunner (Map.insert α r inv_strace) (k y)
  ObsPrj d y α -> Val (k y, RMSMCParticle (logProb d y) [α] inv_strace)
  _            -> Op op (particleRunner inv_strace . k)

{- | A handler that invokes a breakpoint upon matching against the @Observe@ operation with a specific address.
     It returns the rest of the computation.
-}
breakObserve :: Member Observe es
  => Addr       -- ^ Address of @Observe@ operation to break at
  -> Prog es a
  -> Prog es (Prog es a)
breakObserve α_break (Val x) = pure (Val x)
breakObserve α_break (Op op k) = case prj op of
  Just (Observe d y α) -> do
    if α_break == α
      then Val (k y)
      else Op op (breakObserve α_break . k)
  _ -> Op op (breakObserve α_break . k)

