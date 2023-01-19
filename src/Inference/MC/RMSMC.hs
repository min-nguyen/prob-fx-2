{-# LANGUAGE RankNTypes #-}


{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

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
import           Effects.EnvRW
import           Effects.NonDet
import qualified Inference.MC.MH as MH
import qualified Inference.MC.SMC as SMC
import qualified Inference.MC.SIM as SIM
import           Inference.MC.Metropolis as Metropolis
import qualified Inference.MC.SIS as SIS hiding  (particleLogProb)
import           Inference.MC.SIS (Resample(..), ParticleHandler, pfilter)
import           Effects.Lift
import           PrimDist
import           Data.Bifunctor
import           Unsafe.Coerce
import           Util
import Inference.MC.SIM (defaultSample)
import Inference.MC.SMC (suspend)

{- | The particle context for an MCMC trace.
-}
type PrtState = (LogP, Trace)

{- | Call RMSMC on a model.
-}
rmsmc :: forall env a xs. (env `ContainsVars` xs)
  => Int                                          -- ^ number of SMC particles
  -> Int                                          -- ^ number of MH (rejuvenation) steps
  -> GenModel env [EnvRW env, Dist, Sampler] a                -- ^ model
  -> Env env                                      -- ^ input model environment
  -> Vars xs                                      -- ^ optional observable variable names of interest
  -> Sampler [Env env]                            -- ^ output model environments of each particle
rmsmc n_prts mh_steps model env_in obs_vars = do
  -- | Handle model to probabilistic program
  let prog_0   = handleCore env_in model
  -- | Convert observable variables to strings
      tags = varsToStrs @env obs_vars
  map (snd . fst) <$> rmpfilter n_prts mh_steps tags prog_0

{- | Call RMSMC on a probabilistic program.
-}
rmpfilter ::
     Int                                          -- ^ number of SMC particles
  -> Int                                          -- ^ number of MH (rejuvenation) steps
  -> [Tag]                                        -- ^ tags indicating variables of interest
  -> Model '[Sampler] a                                   -- ^ probabilistic program
  -> Sampler [(a, PrtState)]                      -- ^ final particle results and contexts
rmpfilter n_prts mh_steps tags model = do
  -- let q =  pfilter handleParticle model (prts, ps)
  obs_αs <- (handleM . defaultSample . observeAddresses) model
  (handleM . handleResample mh_steps tags obs_αs model . pfilter handleParticle) (prts, ps)
  where (prts, ps) = unzip $ replicate n_prts (model, (0, Map.empty))

{- | A handler that records the values generated at @Sample@ operations and invokes a breakpoint
     at the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation, its breakpoint address, and the particle's sample trace
-}
handleParticle :: ParticleHandler '[Sampler] PrtState
handleParticle = fmap asPrtTrace . handleM . reuseSamples Map.empty . suspend where
  asPrtTrace ((prt, ρ), τ) = (prt, (ρ, τ))

{- | A handler for resampling particles according to their normalized log-likelihoods, and then pertrubing their sample traces using MH.
-}
handleResample :: (Member Sampler fs)
  => Int                                          -- ^ number of MH (rejuvenation) steps
  -> [Tag]                                        -- ^ tags indicating variables of interest
  -> [Addr]
  -> Model '[Sampler] a
  -> Handler (Resample PrtState) fs [(a, PrtState)] [(a, PrtState)]
handleResample mh_steps tags obs_αs m = handle obs_αs (const Val) ( hop) where
  hop :: Member Sampler fs => [Addr] -> Resample PrtState x -> ([Addr] -> x -> Prog fs a) -> Prog fs a
  hop αs (Accum σs σs') k = do
    let (ρs , τs ) = unzip σs
        (ρs', τs') = unzip σs'
        ρs_accum  = map (+ logMeanExp ρs) ρs'
        τs_accum  = zipWith Map.union τs' τs
    k αs (zip ρs_accum τs_accum)
  hop αs (Resample (_, σs) ) k = do
    let (ρs , τs ) = unzip σs
  -- | Resample the RMSMC particles according to the indexes returned by the SMC resampler
    idxs <- call $ SMC.resampleMul ρs
    let τs_res    = map (τs !!) idxs
    -- | Insert break point to perform MH up to
        partial_model   = suspendAt (head αs) m
    -- | Perform MH using each resampled particle's sample trace and get the most recent MH iteration.
    (prts_mov, σs_mov) <- mapAndUnzipM
                              (\τ -> do ((prt_mov, lρ), τ_mov) <- fmap head (MH.ssmh mh_steps τ (Addr "" 0) tags (unsafeCoerce partial_model))
                                        let lρ_mov = (sum . map snd . Map.toList) lρ
                                        return (prt_mov, (lρ_mov, τ_mov)) )
                              τs_res
    k (tail αs) (prts_mov, σs_mov)

observeAddresses :: Handler Observe es a [Addr]
observeAddresses = handle [] (flip (const (Val . Prelude.reverse))) hop where
  hop :: [Addr] -> Observe x -> ([Addr] -> x -> Prog es [Addr]) -> Prog es [Addr]
  hop αs (Observe d y α) k = k (α:αs) y

{- | A handler that invokes a breakpoint upon matching against the @Observe@ operation with a specific address.
     It returns the rest of the computation.
-}
suspendAt :: (Member Observe es)
  => Addr       -- ^ Address of @Observe@ operation to break at
  -> Prog es a
  -> Prog es (Prog es a)
suspendAt α_break (Val x) = pure (Val x)
suspendAt α_break (Op op k) = case prj op of
  Just (Observe d y α) -> do
    if α_break == α
      then Val (k y)
      else Op op (suspendAt α_break . k)
  _ -> Op op (suspendAt α_break . k)

