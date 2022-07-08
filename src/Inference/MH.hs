{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Inference.MH where

import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Kind (Type)
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import Data.Maybe
-- import Data.Extensible hiding (Member)
import Env
import Control.Monad
import Effects.Dist
import Prog
import Model hiding (runModelFree)
import Sampler
import PrimDist
import Trace
import Effects.Lift
import qualified OpenSum as OpenSum
import OpenSum (OpenSum(..))
import Effects.ObsReader
import Effects.State
import Unsafe.Coerce
import Inference.SIM (handleObs)


-- ||| (Section 6.2.2) Metropolis-Hastings
mh :: (FromSTrace env, es ~ '[ObsReader env, Dist, Lift Sampler])
  =>
  -- | Number of MH iterations
     Int
  -- | A model
  -> Model env es a
  -- | A model environment (containing observed values to condition on)
  -> Env env
  -- | An optional list of observable variable names (strings) to specify sample sites of interest (e.g. for interest in sampling #mu, provide "mu"). This causes other variables to not be resampled unless necessary.
  -> [Tag]
  -- | Trace of output environments, containing values sampled for each MH iteration
  -> Sampler [Env env]
mh n model env tags  = do
  let prog = (handleDist . handleObsRead env) (runModel model)
  mhTrace <- mhInternal n prog Map.empty tags
  pure (map (\((_, env_out), _) -> fromSTrace env_out) mhTrace)

mhInternal :: (es ~ [Observe, Sample, Lift Sampler])
   => Int                              -- Number of mhSteps
   -> Prog es a                        -- Model consisting of sample and observe operations
   -> STrace                           -- Initial sample trace
   -> [Tag]                            -- Tags indicated sample sites of interest
   -> Sampler (MHTrace LPTrace a)      -- Trace of all accepted outputs, samples, and logps
mhInternal n prog strace_0 tags = do
  -- Perform initial run of mh
  let α_0 = ("", 0)
  mhCtx_0 <- runMH strace_0 α_0 prog
  -- A function performing n mhSteps using initial mhCtx. 
  -- Note: most recent samples are at the front (head) of the trace
  foldl (>=>) pure (replicate n (mhStep prog tags acceptMH)) [mhCtx_0]

-- | Perform one step of MH
mhStep :: (es ~ '[Observe, Sample,  Lift Sampler])
  =>
  -- | Model environment
     Prog es a
  -- | Tags indicating sample sites of interest
  -> [Tag]
   -> Accept p a
  -- | Trace of previous MH outputs
  -> [((a, STrace), p)]
  -- | Updated trace of MH outputs
  -> Sampler [((a, STrace), p)]
mhStep model tags accepter trace = do
  let -- Get previous mh output
      ((x, samples), logps) = head trace

      sampleSites = if null tags then samples
                    else  Map.filterWithKey (\(tag, i) _ -> tag `elem` tags) samples

  α_samp_ind <- sample $ DiscrUniformDist 0 (Map.size sampleSites - 1)
  let (α_samp, _) = Map.elemAt α_samp_ind sampleSites

  ((x', samples'), logps') <- runMH samples α_samp model

  (mhCtx', acceptance_ratio) <- accepter α_samp ((x', samples'), logps') ((x, samples), logps)
  u <- sample (UniformDist 0 1)

  if u < acceptance_ratio
    then do return (mhCtx':trace)
    else do return trace

-- ||| MH handler
runMH :: (es ~ '[Observe, Sample,  Lift Sampler])
  =>
  -- | Sample trace of previous MH iteration
     STrace
  -- | Sample address of interest
  -> Addr
  -- | Model
  -> Prog es a
  -> Sampler ((a, STrace), LPTrace)
runMH strace α_samp = handleLift . handleSamp strace α_samp . handleObs . traceLPs . traceSamples

-- ||| Selectively sample
handleSamp :: STrace -> Addr -> Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
handleSamp strace α_samp (Op op k) = case discharge op of
  Right (Sample (PrimDistDict d) α) ->
        do  let maybe_y = if α == α_samp then Nothing else lookupSample strace d α
            case maybe_y of
                Nothing -> lift (sample d) >>= (handleSamp strace α_samp . k)
                Just x  -> (handleSamp strace α_samp . k) x
  _  -> error "Impossible: Nothing cannot occur"
handleSamp _ _ (Val x) = return x

lookupSample :: Show a => OpenSum.Member a PrimVal => STrace -> PrimDist a -> Addr -> Maybe a
lookupSample strace d α = do
    (ErasedPrimDist d', x) <- Map.lookup α strace
    if d == unsafeCoerce d'
      then OpenSum.prj x
      else Nothing

type MHTrace p a = [((a, STrace), p)]

type MHCtx p a = ((a, STrace), p)

type Accept p a = Addr                        -- proposal sample address
               -> MHCtx LPTrace a             -- proposed mh ctx, parameterised by log probability map
               -> MHCtx p a                   -- previous mh ctx, parameterised by arbitrary probability representation p
               -> Sampler (MHCtx p a, Double) -- (proposed mh ctx using probability representation p, acceptance ratio)

-- | Compute acceptance probability
acceptMH :: Accept LPTrace a
acceptMH x0 ((a, strace'), lptrace') ((_, strace), lptrace)  = do
  let sampled' = Set.singleton x0 `Set.union` (Map.keysSet strace' \\ Map.keysSet strace)
      sampled  = Set.singleton x0 `Set.union` (Map.keysSet strace \\ Map.keysSet strace')
      dom_logα = log (fromIntegral $ Map.size strace) - log (fromIntegral $ Map.size strace')
      logα     = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace))
                         0 (Map.keysSet lptrace \\ sampled)
      logα'    = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace'))
                         0 (Map.keysSet lptrace' \\ sampled')
  pure (((a, strace'), lptrace'), exp (dom_logα + logα' - logα))
