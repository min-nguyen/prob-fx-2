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

module Inference.MC.RMPF where

import qualified Data.Map as Map
import           Data.Map (Map)
import           Env
import           Comp
import           Model
import           Sampler
import           Trace  (Trace, filterTrace)
import           LogP
import           Control.Monad
import           Control.Applicative
import           Effects.Dist
import           Effects.EnvRW
import           Effects.NonDet
import qualified Inference.MC.SSMH as SSMH
import qualified Inference.MC.SMC as SMC
import qualified Inference.MC.SIM as SIM
import           Inference.MC.MH as MH
import qualified Inference.MC.SIS as SIS hiding  (particleLogProb)
import           Inference.MC.SIS (Resample(..), ParticleHandler, pfilter)

import           PrimDist
import           Data.Bifunctor
import           Unsafe.Coerce
import           Util
import Inference.MC.SIM (defaultSample)
import Inference.MC.SMC (advance)

{- | The particle context for an MCMC trace.
-}
data PrtState = PrtState Addr LogP Trace

unpack :: [PrtState] -> (Addr, [LogP], [Trace])
unpack prts = (head αs, ps, τs)
  where (αs, ps, τs) = foldr (\(PrtState α p τ) (αs, ps, τs) -> (α:αs, p:ps, τ:τs) ) ([],[],[]) prts

pack :: (Addr, [LogP], [Trace]) -> [PrtState]
pack (α, ps, τs) = zipWith3 PrtState (repeat α) ps τs

{- | Call RMPF on a model.
-}
rmpf :: forall env a xs. (env `ContainsVars` xs)
  => Int                                          -- ^ number of SMC particles
  -> Int                                          -- ^ number of SSMH (rejuvenation) steps
  -> GenModel env [EnvRW env, Dist, Sampler] a                -- ^ model
  -> Env env                                      -- ^ input model environment
  -> Vars xs                                      -- ^ optional observable variable names of interest
  -> Sampler [Env env]                            -- ^ output model environments of each particle
rmpf n_prts mh_steps model env_in obs_vars = do
  -- | Handle model to probabilistic program
  let prog_0   = handleCore env_in model
  -- | Convert observable variables to strings
      tags = varsToStrs @env obs_vars
  map (snd . fst) <$> rmpf' n_prts mh_steps tags prog_0

{- | Call RMPF on a probabilistic program.
-}
rmpf' ::
     Int                                          -- ^ number of SMC particles
  -> Int                                          -- ^ number of SSMH (rejuvenation) steps
  -> [Tag]                                        -- ^ tags indicating variables of interest
  -> Model '[Sampler] a                                   -- ^ probabilistic program
  -> Sampler [(a, PrtState)]                      -- ^ final particle results and contexts
rmpf' n_prts mh_steps tags model = do
  -- let q =  pfilter exec model (prts, ps)
  (handleIO . handleResample mh_steps tags model . pfilter n_prts (PrtState (Addr "" 0) 0 Map.empty) exec ) model

{- | A handler that records the values generated at @Sample@ operations and invokes a breakpoint
     at the first @Observe@ operation, by returning:
       1. the rest of the computation
       2. the log probability of the @Observe operation, its breakpoint address, and the particle's sample trace
-}
exec :: ParticleHandler '[Sampler] PrtState a
exec (p, PrtState _ w τ)  = (fmap asPrtTrace . handleIO . reuseTrace τ . suspendα w) p where
  asPrtTrace ((prt, α, w), τ) = (prt, PrtState α w τ)

{- | A handler for resampling particles according to their normalized log-likelihoods, and then pertrubing their sample traces using SSMH.
-}
handleResample :: (Member Sampler fs)
  => Int                                          -- ^ number of SSMH (rejuvenation) steps
  -> [Tag]                                        -- ^ tags indicating variables of interest
  -> Model '[Sampler] a
  -> Handler (Resample PrtState) fs [(a, PrtState)] [(a, PrtState)]
handleResample mh_steps tags  m = handleWith () (const Val) (const hop) where
  hop :: Member Sampler fs => Resample PrtState x -> (() -> x -> Comp fs a) -> Comp fs a
  hop  (Resample pσs) k = do
    let (α, ws, τs) = (unpack . map snd) pσs
  -- | Resample the RMPF particles according to the indexes returned by the SMC resampler
    idxs <- call $ SMC.resampleMul ws
    let τs_res    = map (τs !!) idxs
    -- | Insert break point to perform SSMH up to
        partial_model   = suspendAt α m
    -- | Perform SSMH using each resampled particle's sample trace and get the most recent SSMH iteration.
    wprts_mov <- mapM (\τ -> do ((prt_mov, lwtrace), τ_mov) <- fmap head (call $ SSMH.ssmh' mh_steps τ tags partial_model)
                                let w_mov = (sum . Map.elems) lwtrace
                                return (prt_mov, PrtState α w_mov τ_mov) )
                      τs_res

    let (prts_mov, (α, ps_mov, τs_mov)) = (second unpack . unzip) wprts_mov
        wprts_norm =  zip prts_mov (pack (α, repeat (logMeanExp ps_mov), τs_mov))
    k () (unsafeCoerce wprts_norm)
    -- k () wprts_mov

suspendα :: LogP -> Handler Observe es a (Comp (Observe : es) a, Addr, LogP)
suspendα logp (Val x)   = pure (Val x, Addr "" 0, logp)
suspendα logp (Op op k) = case discharge op of
  Right (Observe d y α) -> Val (k y, α, logp + logProb d y)
  Left op'              -> Op op' (suspendα logp . k)

{- | A handler that invokes a breakpoint upon matching against the @Observe@ operation with a specific address.
     It returns the rest of the computation.
-}
suspendAt :: (Member Observe es)
  => Addr       -- ^ Address of @Observe@ operation to break at
  -> Comp es a
  -> Comp es (Comp es a)
suspendAt α_break (Val x) = pure (Val x)
suspendAt α_break (Op op k) = case prj op of
  Just (Observe d y α) -> do
    if α_break == α
      then Val (k y)
      else Op op (suspendAt α_break . k)
  _ -> Op op (suspendAt α_break . k)

