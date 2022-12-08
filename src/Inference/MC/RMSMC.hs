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
import           Trace  (STrace, filterTrace)
import           LogP
import           Control.Monad
import           Control.Applicative
import           Effects.Dist
import           Effects.ObsRW
import           Effects.NonDet
import qualified Inference.MC.MH as MH
import qualified Inference.MC.SMC as SMC
import qualified Inference.MC.SIM as SIM
import qualified Inference.MC.Metropolis as Metropolis
import qualified Inference.MC.SIS as SIS hiding  (particleLogProb)
import           Inference.MC.SIS (Resample(..), ResampleHandler, ParticleHandler, ParticleCtx (..))
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
  , particleSTrace    :: STrace
  }

instance ParticleCtx TracedParticle where
  pempty            = TracedParticle 0 (Addr 0 "" 0) Map.empty
  paccum ctxs ctxs' =
    let log_ps   = uncurry paccum              (mapT2 (particleLogProb <$>)  (ctxs, ctxs'))
        α_obs    = particleObsAddr <$> ctxs'
        straces  = uncurry (zipWith Map.union) (mapT2 (particleSTrace <$>)   (ctxs', ctxs))
    in  zipWith3 TracedParticle log_ps α_obs straces

{- | Call RMSMC on a model.
-}
rmsmc :: forall env a xs. (env `ContainsVars` xs)
  => Int                                          -- ^ number of SMC particles
  -> Int                                          -- ^ number of MH (rejuvenation) steps
  -> Model env [ObsRW env, Dist] a                -- ^ model
  -> Env env                                      -- ^ input model environment
  -> Vars xs                                      -- ^ optional observable variable names of interest
  -> Sampler [Env env]                            -- ^ output model environments of each particle
rmsmc n_particles mh_steps model env_in obs_vars = do
  -- | Handle model to probabilistic program
  let prog_0   = handleCore env_in model
  -- | Convert observable variables to strings
      tags = varsToStrs @env obs_vars
  rmsmc_trace <- handleLift (rmsmcInternal n_particles mh_steps tags prog_0)
  pure (map (snd . fst) rmsmc_trace)

{- | Call RMSMC on a probabilistic program.
-}
rmsmcInternal :: (LastMember (Lift Sampler) fs)
  => Int                                          -- ^ number of SMC particles
  -> Int                                          -- ^ number of MH (rejuvenation) steps
  -> [Tag]                                        -- ^ tags indicating variables of interest
  -> ProbProg a                                    -- ^ probabilistic program
  -> Prog fs [(a, TracedParticle)]                -- ^ final particle results and contexts
rmsmcInternal n_particles mh_steps tags =
  handleResample mh_steps tags . SIS.sis n_particles handleParticle

{- | A handler that records the values generated at @Sample@ operations and invokes a breakpoint
     at the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation, its breakpoint address, and the particle's sample trace
-}
handleParticle :: ProbProg a -> Sampler (ProbProg a, TracedParticle)
handleParticle = (asTracedParticle <$>) . handleSamp . handleObs where
  asTracedParticle ((prt, logp, α), strace) = (prt, TracedParticle logp α strace)

handleSamp :: Prog '[Sample] a -> Sampler (a, STrace)
handleSamp = loop Map.empty where
  loop :: STrace ->  Prog '[Sample] a -> Sampler (a, STrace)
  loop strace (Val x)   = pure (x, strace)
  loop strace (Op op k) = case discharge1 op of
    (Sample d α) -> do r <- sampleRandom
                       let y = sampleInv d r
                       loop (Map.insert α r strace) (k y)

handleObs :: Prog (Observe : es) a -> Prog es (Prog (Observe : es) a, LogP, Addr)
handleObs (Val x)   = pure (Val x, 0, Addr 0 "" 0)
handleObs (Op op k) = case discharge op of
  Right (Observe d y α) -> Val (k y, logProb d y, α)
  Left op'              -> Op op' (handleObs . k)

{- | A handler for resampling particles according to their normalized log-likelihoods, and then pertrubing their sample traces using MH.
-}
handleResample :: (LastMember (Lift Sampler) fs)
  => Int                                          -- ^ number of MH (rejuvenation) steps
  -> [Tag]                                        -- ^ tags indicating variables of interest
  -> Prog (Resample TracedParticle : fs) a
  -> Prog fs a
handleResample mh_steps tags = loop where
  loop (Val x) = Val x
  loop (Op op k) = case discharge op of
    Right (Resample (prts, ctxs) prog_0) ->
      do  -- | Resample the RMSMC particles according to the indexes returned by the SMC resampler
          idxs <- lift $ SMC.resampleMul (map particleLogProb ctxs)
          let resampled_ctxs   = map (ctxs !! ) idxs
          -- | Get the observe address at the breakpoint (from the context of any arbitrary particle, e.g. by using 'head')
              resampled_α      = (particleObsAddr . head) resampled_ctxs
          -- | Get the sample trace of each resampled particle
              resampled_straces = map particleSTrace resampled_ctxs
          -- | Insert break point to perform MH up to
              partial_model = breakObserve resampled_α prog_0
          -- | Perform MH using each resampled particle's sample trace and get the most recent MH iteration.
          mh_trace <-  mapM ( fmap head
                            . flip (MH.mhInternal mh_steps tags) partial_model
                           ) resampled_straces
          {- | Get:
              1) the continuations of each particle from the break point
              2) the log prob traces of each particle up until the break point
              3) the sample traces of each particle up until the break point -}
          let (rejuv_prts, ((_, lp_traces), rejuv_straces)) = second (first unzip . unzip) (unzip mh_trace)

              -- | Recompute the log weights of all particles up until the break point
              rejuv_lps     = map (sum . map snd . Map.toList) lp_traces

              rejuv_ctxs    = zipWith3 TracedParticle rejuv_lps (repeat resampled_α) rejuv_straces

          (loop . k) (rejuv_prts, rejuv_ctxs)

    Left op' -> Op op' (loop . k)

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

