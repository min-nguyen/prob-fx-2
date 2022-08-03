{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{- | Metropolis-Hastings inference.
-}

module Inference.MH
  ( -- * Inference wrapper functions
      mh
    , mhInternal
    , mhStep
    -- * Inference handlers
    , runMH
    , handleSamp
    , handleObs
    -- * Auxiliary functions and definitions
    , lookupSample
    , MHCtx
    , Accept
    , acceptMH
  ) where

import Control.Monad ( (>=>) )
import Data.Kind (Type)
import Data.Map (Map)
import Data.Maybe ( fromJust )
import Data.Set (Set, (\\))
import Effects.Dist ( Addr, Sample(..), Observe, Dist, Tag )
import Effects.Lift ( handleLift, Lift, lift )
import Effects.ObsReader ( ObsReader )
import LogP
import Env ( Env, ContainsVars(..), Vars )
import Inference.SIM as SIM (handleObs)
import Model ( Model, handleCore )
import OpenSum (OpenSum(..))
import PrimDist
    ( ErasedPrimDist(..),
      PrimVal,
      PrimDist(Uniform, UniformD),
      sample,
      pattern PrimDistPrf )
import Prog ( discharge, Prog(..) )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified OpenSum
import Sampler ( Sampler, liftIO )
import Trace
    ( traceLogProbs,
      traceSamples,
      filterTrace,
      LPTrace,
      FromSTrace(..),
      STrace )
import Unsafe.Coerce ( unsafeCoerce )

-- | Top-level wrapper for Metropolis-Hastings (MH) inference
mh :: forall env es a xs. (FromSTrace env, env `ContainsVars` xs)
  -- | number of MH iterations
  => Int
  -- | model
  -> Model env [ObsReader env, Dist, Lift Sampler] a
  -- | input model environment
  -> Env env
  -- | optional list of observable variable names (strings) to specify sample sites of interest
  {- for example, for interest in sampling @#mu@, provide @#mu <#> vnil@ to cause other variables
     to not be resampled unless necessary. -}
  -> Vars xs
  -- | output model environments
  -> Sampler [Env env]
mh n model env obsvars  = do
  -- Handle model to probabilistic program
  let prog = handleCore env model
  -- Convert observable variables to strings
      tags = varsToStrs @env obsvars
  -- Run MH for n iterations
  mhTrace <- mhInternal n prog Map.empty tags
  -- Convert each iteration's sample trace to a model environment
  pure (map (fromSTrace . snd) mhTrace)

-- | Perform MH on a probabilistic program
mhInternal
   -- | number of MH iterations
   :: Int
   -> Prog [Observe, Sample, Lift Sampler] a
   -- | initial sample trace
   -> STrace
   -- | tags indicating sample sites of interest
   -> [Tag]
   -- | [(accepted outputs, logps), samples)]
   -> Sampler [((a, LPTrace), STrace)]
mhInternal n prog strace_0 tags = do
  -- Perform initial run of mh
  let α_0 = ("", 0)
  mhCtx_0 <- runMH strace_0 α_0 prog
  -- A function performing n mhSteps using initial mhCtx.
  -- Note: most recent samples are at the front (head) of the trace
  foldl (>=>) pure (replicate n (mhStep prog tags acceptMH)) [mhCtx_0]

-- | Perform one iteration of MH by drawing a new sample and then rejecting or accepting it.
mhStep
  :: Prog [Observe, Sample, Lift Sampler] a
  -- | tags indicating sample sites of interest
  -> [Tag]
  -- | a mechanism for accepting proposals
  -> Accept p a
  -- | trace of previous MH results
  -> [((a, p), STrace)]
  -- | updated trace of MH results
  -> Sampler [((a, p), STrace)]
mhStep prog tags accepter trace = do
  -- Get previous MH output
  let mhCtx@(_, samples) = head trace
  -- Get possible addresses to propose new samples for
  let sampleSites = if null tags then samples else filterTrace tags samples
  -- Draw a proposal sample address
  α_samp_ind <- sample $ UniformD 0 (Map.size sampleSites - 1)
  let (α_samp, _) = Map.elemAt α_samp_ind sampleSites
  -- Run MH with proposal sample address to get an MHCtx using LPTrace as its probability type
  mhCtx'_lp <- runMH samples α_samp prog
  -- Compute acceptance ratio to see if we use the proposed mhCtx' (which is mhCtx'_lp with 'LPTrace' converted to some type 'p')
  (mhCtx', acceptance_ratio) <- accepter α_samp mhCtx mhCtx'_lp
  u <- sample (Uniform 0 1)
  if u < acceptance_ratio
    then do return (mhCtx':trace)
    else do return trace

-- | Handler for one iteration of MH
runMH ::
  -- | sample trace of previous MH iteration
     STrace
  -- | sample address of interest
  -> Addr
  -> Prog [Observe, Sample, Lift Sampler] a
  -- | ((model output, log-probability trace), sample trace)
  -> Sampler ((a, LPTrace), STrace)
runMH strace α_samp = handleLift . handleSamp strace α_samp . SIM.handleObs . traceSamples . traceLogProbs

-- | Handler for @Sample@ that selectively reuses old samples or draws new ones
handleSamp ::
  -- | Sample trace
     STrace
  -- | Address of the proposal sample site for the current MH iteration
  -> Addr
  -> Prog [Sample, Lift Sampler] a
  -> Prog '[Lift Sampler] a
handleSamp strace α_samp (Op op k) = case discharge op of
    Right (Sample (PrimDistPrf d) α) ->
      let maybe_y = if α == α_samp then Nothing else lookupSample α d strace
      in  case maybe_y of
                  Nothing -> lift (sample d) >>= k'
                  Just x  -> k' x
    Left op' -> Op op' k'
  where k' = handleSamp strace α_samp . k
handleSamp _ _ (Val x) = return x

{- | For a given address, look up a sampled value from a sample trace, returning
     it only if the primitive distribution it was sampled from matches the current one. -}
lookupSample :: OpenSum.Member a PrimVal =>
  -- | Address of sample site
     Addr
  -- | Distribution to sample from
  -> PrimDist a
  -- | Sample trace
  -> STrace
  -- | Possibly a looked-up sampled value
  -> Maybe a
lookupSample α d strace   = do
    (ErasedPrimDist d', x) <- Map.lookup α strace
    if d == unsafeCoerce d'
      then OpenSum.prj x
      else Nothing

{- | The result of a single MH iteration, where @a@ is the type of model output and
     @p@ is some representation of probability.
-}
type MHCtx p a = ((a, p), STrace)

{- | An abstract mechanism for computing an acceptance probability, where @a@ is the
     type of model output and @p@ is some representation of probability.
-}
type Accept p a =
  -- | proposal sample address
    Addr
  -- | previous MH ctx, parameterised by @p@
  -> MHCtx p a
  -- | proposed MH ctx, parameterised by a log-probability map
  -> MHCtx LPTrace a
  -- | (proposed MH ctx using probability representation p, Acceptance ratio)
  -> Sampler (MHCtx p a, Double)

-- | An acceptance mechanism for MH
acceptMH :: Accept LPTrace a
acceptMH x0 ((_, lptrace), strace) ((a, lptrace'),  strace') = do
  let dom_logα = log (fromIntegral $ Map.size strace) - log (fromIntegral $ Map.size strace')
      sampled  = Set.singleton x0 `Set.union` (Map.keysSet strace \\ Map.keysSet strace')
      sampled' = Set.singleton x0 `Set.union` (Map.keysSet strace' \\ Map.keysSet strace)
      logα     = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace))
                         0 (Map.keysSet lptrace \\ sampled)
      logα'    = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace'))
                         0 (Map.keysSet lptrace' \\ sampled')
  pure (((a, lptrace'), strace'), (exp . unLogP) (dom_logα + logα' - logα))
