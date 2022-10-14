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
{-# LANGUAGE TypeOperators #-}

{- Rejuvenate-Move Sequential Monte Carlo inference.
-}

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
import Inference.SIS (Resample(..), ParticleResampler, ParticleRunner, ParticleCtx (..))
import OpenSum (OpenSum)
import Inference.SMC (Particle, pattern Particle)
import Effects.Lift
import PrimDist
import Data.Bifunctor
import Unsafe.Coerce
import Util

{- | The particle context for an MCMC trace.
-}
data TracedParticle = TracedParticle {
    particleLogProb   :: LogP
  , particleObsAddr   :: Addr
  , particleSTrace    :: STrace
  }

instance ParticleCtx TracedParticle where
  pempty            = TracedParticle 0 ("", 0) Map.empty
  paccum ctxs ctxs' =
    let log_ps   = uncurry paccum              (bimap' (particleLogProb <$>)  (ctxs, ctxs'))
        α_obs    = particleObsAddr <$> ctxs'
        straces  = uncurry (zipWith Map.union) (bimap' (particleSTrace <$>)   (ctxs', ctxs))
    in  zipWith3 TracedParticle log_ps α_obs straces

{- | Call RMSMC on a model.
-}
rmsmc
  :: Int                                          -- ^ number of SMC particles
  -> Int                                          -- ^ number of MH steps
  -> Model env [ObsRW env, Dist, Lift Sampler] a  -- ^ model
  -> Env env                                      -- ^ input model environment
  -> Sampler [Env env]                            -- ^ output model environments of each particle
rmsmc n_particles mh_steps model env_in = do
  let prog = (handleDist . handleObsRW env_in) (runModel model)
  rmsmcInternal n_particles mh_steps prog >>= pure . map (snd . fst)

{- | Call RMSMC on a probabilistic program.
-}
rmsmcInternal
  :: Int                                          -- ^ number of SMC particles
  -> Int                                          -- ^ number of MH steps
  -> Prog [Observe, Sample, Lift Sampler] a       -- ^ probabilistic program
  -> Sampler [(a, TracedParticle)]                 -- ^ final particle results and contexts
rmsmcInternal n_particles mh_steps   = do
  handleLift . SIM.handleSamp . SIM.handleObs . SIS.sis n_particles particleRunner (particleResampler mh_steps)

data Resample' es a where
  Resample'
    -- | ((particles, contexts), initial probabilistic program)
    -- :: ([SISProg ctx a], [ctx], ProbProg a)
    :: ([Prog (Resample' es : es) a], [Prog es a], [TracedParticle])
    -- | ((resampled particles, resampled contexts), idxs)
    -> Resample' es (([Prog (Resample' es : es) a], [TracedParticle]), [Int])

{- | A handler for resampling particles according to their normalized log-likelihoods, and then pertrubing their sample traces using MH.
-}
particleResampler :: (Members [Observe, Sample] es, LastMember (Lift Sampler) es)
  => Int -> (Prog (Resample' es : es) a -> Prog es a)
particleResampler  mh_steps = loop where
  loop (Val x) = Val x
  loop (Op op k) = case discharge op of
    Right (Resample' (prts, prts_0, ctxs)) ->
      do  -- | Resample the RMSMC particles according to the indexes returned by the SMC resampler
          idxs <- snd <$> SMC.particleResampler (call (Resample ([], map (Particle . particleLogProb) ctxs)))
          let resampled_prts   = map (prts !! ) idxs
              resampled_ctxs   = map (ctxs !! ) idxs

          -- | Get the observe address at the breakpoint (from the context of any arbitrary particle, e.g. by using 'head')
          let α_obs   = (particleObsAddr . head) (resampled_ctxs)

          -- | Insert break point to perform MH up to
              partial_models = map (breakObserve α_obs ) prts_0
          -- | Perform MH using each resampled particle's sample trace and get the most recent MH iteration.
          mh_trace <- mapM (\(partial_model, resampled_ctx) -> fmap head .
                                     MH.handleAccept
                                    $  (MH.mhInternal mh_steps partial_model (particleSTrace resampled_ctx) [] )
                                    ) (zip partial_models ( resampled_ctxs))
          {- | Get:
              1) the continuations of each particle from the break point (augmented with the non-det effect)
              2) the log prob traces of each particle up until the break point
              3) the sample traces of each particle up until the break point -}
          let ((rejuv_prts, lp_traces), rejuv_straces) = first unzip (unzip mh_trace)
              -- | Filter log probability traces to only include that for observe operations
              -- lp_traces_obs = map (filterByKey (`elem` α_obs)) lp_traces
              -- | Recompute the log weights of all particles up until the break point
              rejuv_lps     = map (sum . map snd . Map.toList) lp_traces

              rejuv_ctxs    = zipWith3 TracedParticle rejuv_lps (repeat α_obs) rejuv_straces

              rejuv_prts' = (map (weakenProg) rejuv_prts)

          (loop . k) ((rejuv_prts', rejuv_ctxs), idxs)

    Left op' -> Op op' (loop . k)

{- | A handler that records the values generated at @Sample@ operations and invokes a breakpoint at the first @Observe@ operation.
-}
particleRunner ::forall es a. (Members [Observe, Sample] es, LastMember (Lift Sampler) es)
  -- | a particle
  => Prog es a
  -- | (a particle suspended at the next step, corresponding context)
  -> Prog es (Prog es a, (Prog es a, TracedParticle))
particleRunner prog = do

  (prog_k, prt) <- loop Map.empty prog
  pure (prog_k, (prog, prt))
  where
  loop :: STrace ->  Prog es a -> Prog es (Prog es a, TracedParticle)
  loop inv_strace (Val x) = pure (Val x, TracedParticle 0 ("", 0) Map.empty)
  loop inv_strace (Op op k) = case op of
    SampPrj d α  -> do r <- lift sampleRandom
                       y <- lift (sampleInv d r)
                       let inv_strace' = (Map.insert α r inv_strace)
                       loop inv_strace' (k y)
    ObsPrj d y α -> Val (k y, TracedParticle (logProb d y) α inv_strace)
    _            -> Op op (loop inv_strace . k)


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

