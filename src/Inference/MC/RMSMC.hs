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
{-# LANGUAGE AllowAmbiguousTypes #-}

{- Rejuvenate-Move Sequential Monte Carlo inference.
-}

module Inference.MC.RMSMC where

import qualified Data.Map as Map
import           Data.Map (Map)
import           Env
import           Prog
import           Model
import           Sampler
import           Trace  (Trace, filterTrace)
import           LogP
import           Control.Monad
import           Control.Applicative
import           Effects.Dist
import           Effects.ObsRW
import           Effects.NonDet
import qualified Inference.MC.MH as MH
import qualified Inference.MC.SMC as SMC
import qualified Inference.MC.SIM as SIM
import           Inference.MC.Metropolis as Metropolis
import qualified Inference.MC.SIS as SIS hiding  (particleLogProb)
import           Inference.MC.SIS (Resample(..), ResampleHandler, ParticleHandler)
import           Effects.Lift
import           PrimDist
import           Data.Bifunctor
import           Unsafe.Coerce
import           Util

{- | The particle context for an MCMC trace.
-}
data TracedParticle = TracedParticle {
    particleLogProb   :: LogP
  , particleObsAddr   :: Addr
  , particleSTrace    :: Trace
  }

{- | Call RMSMC on a model.
-}
rmsmc :: forall env a xs. (env `ContainsVars` xs)
  => Int                                          -- ^ number of SMC particles
  -> Int                                          -- ^ number of MH (rejuvenation) steps
  -> Model env [ObsRW env, Dist] a                -- ^ model
  -> Env env                                      -- ^ input model environment
  -> Vars xs                                      -- ^ optional observable variable names of interest
  -> Sampler [Env env]                            -- ^ output model environments of each particle
rmsmc n_prts mh_steps model env_in obs_vars = do
  -- | Handle model to probabilistic program
  let prog_0   = handleCore env_in model
  -- | Convert observable variables to strings
      tags = varsToStrs @env obs_vars
  rmsmc_trace <- handleLift (rmsmcInternal n_prts mh_steps tags prog_0)
  pure (map (snd . fst) rmsmc_trace)

{- | Call RMSMC on a probabilistic program.
-}
rmsmcInternal :: (HasSampler fs)
  => Int                                          -- ^ number of SMC particles
  -> Int                                          -- ^ number of MH (rejuvenation) steps
  -> [Tag]                                        -- ^ tags indicating variables of interest
  -> ProbProg a                                    -- ^ probabilistic program
  -> Prog fs [(a, TracedParticle)]                -- ^ final particle results and contexts
rmsmcInternal n_prts mh_steps tags  =
  handleResample mh_steps tags . SIS.sis n_prts handleParticle (TracedParticle 0 (Addr 0 "" 0) Map.empty)

{- | A handler that records the values generated at @Sample@ operations and invokes a breakpoint
     at the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation, its breakpoint address, and the particle's sample trace
-}
handleParticle :: ProbProg a -> Sampler (ProbProg a, TracedParticle)
handleParticle = (asTracedParticle <$>) . reuseSamples Map.empty . suspendα where
  asTracedParticle ((prt, ρ, α), τ) = (prt, TracedParticle ρ α τ)

suspendα :: Prog (Observe : es) a -> Prog es (Prog (Observe : es) a, LogP, Addr)
suspendα (Val x)   = pure (Val x, 0, Addr 0 "" 0)
suspendα (Op op k) = case discharge op of
  Right (Observe d y α) -> Val (k y, logProb d y, α)
  Left op'              -> Op op' (suspendα . k)

{- | A handler for resampling particles according to their normalized log-likelihoods, and then pertrubing their sample traces using MH.
-}
handleResample :: (HasSampler fs)
  => Int                                          -- ^ number of MH (rejuvenation) steps
  -> [Tag]                                        -- ^ tags indicating variables of interest
  -> Prog (Resample TracedParticle : fs) a
  -> Prog fs a
handleResample mh_steps tags = loop where
  loop (Val x) = Val x
  loop (Op op k) = case discharge op of
    Right (Resample (prts, ρs) prog_0) ->
      do  -- | Resample the RMSMC particles according to the indexes returned by the SMC resampler
          idxs <- lift $ SMC.resampleMul (map particleLogProb ρs)
          let resampled_ρs   = map (ρs !! ) idxs
          -- | Get the observe address at the breakpoint (from the context of any arbitrary particle, e.g. by using 'head')
              resampled_α      = (particleObsAddr . head) resampled_ρs
          -- | Get the sample trace of each resampled particle
              resampled_τs = map particleSTrace resampled_ρs
          -- | Insert break point to perform MH up to
              partial_model = breakObserve resampled_α prog_0
          -- | Perform MH using each resampled particle's sample trace and get the most recent MH iteration.
          mh_trace <-  mapM ( fmap head
                            . flip (MH.mhInternal mh_steps tags) partial_model
                           ) resampled_τs
          {- | Get:
              1) the continuations of each particle from the break point
              2) the log prob traces of each particle up until the break point
              3) the sample traces of each particle up until the break point -}
          let ((rejuv_prts, lρs), rejuv_τs) = first unzip (unzip mh_trace)

              -- | Recompute the log weights of all particles up until the break point
              rejuv_lps     = map (sum . map snd . Map.toList) lρs

              rejuv_ss    = zipWith3 TracedParticle rejuv_lps (repeat resampled_α) rejuv_τs

          (loop . k) (rejuv_prts, rejuv_ss)
    Right (Accum ss ss') ->
      (loop . k) (normaliseParticles ss ss')
    Left op' -> Op op' (loop . k)

normaliseParticles :: [TracedParticle] -> [TracedParticle] -> [TracedParticle]
normaliseParticles ss ss' =
    let log_ps   = uncurry SMC.normaliseParticles (mapT2 (particleLogProb <$>)  (ss, ss'))
        α_obs    = particleObsAddr <$> ss'
        traces  = uncurry (zipWith Map.union) (mapT2 (particleSTrace <$>)   (ss', ss))
    in  zipWith3 TracedParticle log_ps α_obs traces

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

