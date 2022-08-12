{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified Inference.SIS as SIS
import Inference.SIS (Resample(..), ParticleResampler, ParticleRunner)
import OpenSum (OpenSum)
import Inference.SMC (SMCParticle(..))
import Effects.Lift
import PrimDist
import Data.Bifunctor

{- | The particle context for RMSMC
-}
data RMSMCParticle = RMSMCParticle {
    smcParticle       :: SMCParticle
  , particleObsAddrs  :: [Addr]
  , particleTrace     :: InvSTrace
  }

-- rmsmcToplevel :: forall env es' a. (FromSTrace env, Show a) =>
--   (es' ~ [ObsReader env, Dist, Lift Sampler]) =>
--   Int -> Int -> Model env es' a -> Env env -> Sampler [(a, LogP, Env env)]
-- rmsmcToplevel n_particles mh_steps model env = do
--   let prog = (handleDist . handleObsRead env) (runModel model)
--   rmsmc n_particles mh_steps prog env

-- rmsmc :: forall env es' a. (FromSTrace env, Show a) =>
--   (es' ~ [Observe, Sample, Lift Sampler]) =>
--   Int -> Int -> Prog es' a -> Env env -> Sampler [(a, LogP, Env env)]
-- rmsmc n_particles mh_steps prog env = do
--   as_ps_straces <- SIS.sis n_particles (rmsmcResampler mh_steps prog) SMC.smcParticleHdlr SIM.handleObs SIM.handleSamp prog
--   pure $ map (\(a, (addr, p, strace)) -> (a, p, fromSTrace @env strace)) as_ps_straces

-- rmsmcResampler :: forall es a.
--      Int
--   -> Prog [Observe, Sample, Lift Sampler] a -- the initial program, representing the entire unevaluated model execution (having already provided a model environment)
--   -> SIS.ParticleResampler '[Observe, Sample, Lift Sampler] a
-- rmsmcResampler mh_steps prog ctx_0 ctx_1sub0 progs_1 = do
--   -- run SMC resampling, ignore log weights of particles
--   (obs_addrs, _, resampled_straces) <- unzip3 . snd <$> SMC.smcResampler ctx_0 ctx_1sub0 progs_1
--   let -- get most recent observe address
--       α_break       = (head . head) obs_addrs
--       -- insert break point to perform MH up to
--       partial_model = insertBreakpoint α_break prog

--   -- perform metropolis-hastings using each resampled particle's sample trace
--   mhTraces <- lift $ mapM (\strace -> MH.mh mh_steps partial_model strace []) resampled_straces
--   let -- get the continuations of each particle from the break point, and weaken with non-det effect
--       moved_particles = map (weakenNonDet . fst3 . head) mhTraces
--       -- get the sample traces of each particle up until the break point
--       moved_straces   = map (snd3 . head) mhTraces
--       -- get the log prob traces of each particle up until the break point
--       lptraces        = map (thrd3 . head) mhTraces
--       -- filter log probability traces to only include that for observe operations
--       obs_lptraces    = map (Map.filterWithKey (\k a -> k `elem` head obs_addrs)) lptraces
--       -- compute log weights of particles, that is, the total log probability of each particle up until the break point
--       moved_logWs     = map (LogP . sum . map snd . Map.toList) obs_lptraces

--   pure (moved_particles, zip3 obs_addrs moved_logWs moved_straces)

-- reduceContext :: (ctx -> ctx') -> Prog (Resample ctx : es) a -> Prog (Resample ctx' : es) a
-- reduceContext f (Val x)   = Val x
-- reduceContext f (Op op k) =

{- | A handler for resampling particles according to their normalized log-likelihoods.
-}
particleResampler :: Int -> Prog [Observe, Sample, Lift Sampler] a -> ParticleResampler RMSMCParticle
particleResampler mh_steps prog_0 (Val x) = Val x
particleResampler mh_steps prog_0 (Op op k) = case discharge op of
  Right (Resample (prts, ctxs)) ->
    do  -- | Resample the RMSMC particles according to the indexes returned by the SMC resampler
        idxs <- snd <$> SMC.particleResampler (call (Resample (prts, map smcParticle ctxs)))
        let resampled_prts   = map (prts !! ) idxs
            resampled_ctxs   = map (ctxs !! ) idxs
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

        {- | Get:
            1) the continuations of each particle from the break point (augmented with the non-det effect)
            2) the log prob traces of each particle up until the break point
            3) the sample traces of each particle up until the break point -}
        let ((rejuv_prts, lp_traces), rejuv_straces) = first unzip (unzip mh_trace)
            -- | Filter log probability traces to only include that for observe operations
            obs_lp_traces = map (Map.filterWithKey (\α _ -> α `elem` α_obs)) lp_traces
            -- | Recompute the log weights of all particles up until the break point
            rejuv_lps     = map (sum . map snd . Map.toList) obs_lp_traces

            rejuv_ctxs =  RMSMCParticle <$> map SMCParticle rejuv_lps <*> repeat α_obs <*> rejuv_straces

        (particleResampler mh_steps prog_0 . k)
            ((undefined, rejuv_ctxs), idxs)

  Left op' -> Op op' (particleResampler mh_steps prog_0 . k)

particleRunner :: InvSTrace -> ParticleRunner RMSMCParticle
particleRunner inv_strace (Val x) = pure (Val x, RMSMCParticle 0 [] Map.empty)
particleRunner inv_strace (Op op k) = case op of
  SampPrj d α  -> do r <- lift sampleRandom
                     y <- lift (sampleInv d r)
                     particleRunner (Map.insert α r inv_strace) (k y)
  ObsPrj d y α -> Val (k y, RMSMCParticle (SMCParticle $ logProb d y) [α] inv_strace)
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

