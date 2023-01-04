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
data TracedPrt = TracedPrt {
    particleObsAddr   :: Addr
  , particleLogProb   :: LogP
  , particleSTrace    :: Trace
  }

unzipPrts :: [TracedPrt] -> (Addr, [LogP], [Trace])
unzipPrts prts = (head αs, ps, τs)
  where (αs, ps, τs) = foldr (\(TracedPrt α p τ) (αs, ps, τs) -> (α:αs, p:ps, τ:τs) ) ([],[],[]) prts

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
  -> Prog fs [(a, TracedPrt)]                -- ^ final particle results and contexts
rmsmcInternal n_prts mh_steps tags  =
  handleResample mh_steps tags . SIS.sis n_prts handleParticle (TracedPrt (Addr 0 "" 0) 0  Map.empty)

{- | A handler that records the values generated at @Sample@ operations and invokes a breakpoint
     at the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation, its breakpoint address, and the particle's sample trace
-}
handleParticle :: ProbProg a -> Sampler (ProbProg a, TracedPrt)
handleParticle = (asTracedPrt <$>) . reuseSamples Map.empty . suspendα where
  asTracedPrt ((prt, ρ, α), τ) = (prt, TracedPrt ρ α τ)

suspendα :: Prog (Observe : es) a -> Prog es (Prog (Observe : es) a, Addr, LogP)
suspendα (Val x)   = pure (Val x, Addr 0 "" 0, 0)
suspendα (Op op k) = case discharge op of
  Right (Observe d y α) -> Val (k y, α, logProb d y)
  Left op'              -> Op op' (suspendα . k)

{- | A handler for resampling particles according to their normalized log-likelihoods, and then pertrubing their sample traces using MH.
-}
handleResample :: (HasSampler fs)
  => Int                                          -- ^ number of MH (rejuvenation) steps
  -> [Tag]                                        -- ^ tags indicating variables of interest
  -> Prog (Resample TracedPrt : fs) a
  -> Prog fs a
handleResample mh_steps tags = loop where
  loop (Val x) = Val x
  loop (Op op k) = case discharge op of
    Right (Resample (_, σs) prog_0) ->
      do  -- | Resample the RMSMC particles according to the indexes returned by the SMC resampler
          idxs <- lift $ SMC.resampleMul (map particleLogProb σs)
          let σs_res          = map (σs !! ) idxs
          -- | Get the observe address at the breakpoint (from the context of any arbitrary particle, e.g. by using 'head'), and get the sample trace of each resampled particle
              (α, _, τs_res)  = unzipPrts σs_res
          -- | Insert break point to perform MH up to
              partial_model   = suspendAt α prog_0
          -- | Perform MH using each resampled particle's sample trace and get the most recent MH iteration.
          pop_mov <-  forM τs_res
                            (\τ -> do ((prt_mov, lρ), τ_mov) <- fmap head (MH.ssmh mh_steps τ (Addr 0 "" 0) tags partial_model)
                                      let lρ_mov = (sum . map snd . Map.toList) lρ
                                      return (prt_mov, TracedPrt α lρ_mov τ_mov) )
          let (prts_mov, σs_mov) =  unzip pop_mov

          (loop . k) (prts_mov, σs_mov)
    Right (Accum ss ss') ->
      (loop . k) (normaliseParticles ss ss')
    Left op' -> Op op' (loop . k)

normaliseParticles :: [TracedPrt] -> [TracedPrt] -> [TracedPrt]
normaliseParticles ss ss' =
    let log_ps   = uncurry SMC.normaliseParticles (mapT2 (particleLogProb <$>)  (ss, ss'))
        α_obs    = particleObsAddr <$> ss'
        traces  = uncurry (zipWith Map.union) (mapT2 (particleSTrace <$>)   (ss', ss))
    in  zipWith3 TracedPrt α_obs log_ps  traces

{- | A handler that invokes a breakpoint upon matching against the @Observe@ operation with a specific address.
     It returns the rest of the computation.
-}
suspendAt ::
     Addr       -- ^ Address of @Observe@ operation to break at
  -> ProbProg  a
  -> ProbProg  (ProbProg  a)
suspendAt α_break (Val x) = pure (Val x)
suspendAt α_break (Op op k) = case prj op of
  Just (Observe d y α) -> do
    if α_break == α
      then Val (k y)
      else Op op (suspendAt α_break . k)
  _ -> Op op (suspendAt α_break . k)

