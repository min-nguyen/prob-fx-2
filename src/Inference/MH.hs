{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Inference.MH where

import Control.Monad
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Data.Maybe
import Prog
import Trace
import LogP
import PrimDist
import Model
import Effects.ObsReader
import OpenSum
import Env
import Effects.Dist
import Effects.Lift
import qualified Inference.SIM as SIM
import Sampler

-- | Top-level wrapper for Metropolis-Hastings (MH) inference
mh :: forall env es a xs. (env `ContainsVars` xs)
  -- | number of MH iterations
  => Int
  -- | model
  -> Model env [ObsReader env, ObsWriter env, Dist, Lift Sampler] a
  -- | input model environment
  -> Env env
  -- | optional list of observable variable names (strings) to specify sample sites of interest
  {- for example, for interest in sampling @#mu@, provide @#mu <#> vnil@ to cause other variables
     to not be resampled unless necessary. -}
  -> Vars xs
  -- | output model environments
  -> Sampler [Env env]
mh n model env_in obsvars  = do
  -- Handle model to probabilistic program
  let prog = handleCore env_in model
  -- Convert observable variables to strings
      tags = varsToStrs @env obsvars
  -- Run MH for n iterations
  mhTrace <- mhInternal n prog Map.empty tags
  -- Convert each iteration's sample trace to a model environment
  pure (map (snd . fst . fst) mhTrace)

-- | Perform MH on a probabilistic program
mhInternal
   -- | number of MH iterations
   :: Int
   -> Prog [Observe, Sample, Lift Sampler] a
   -- | initial sample trace
   -> InvSTrace
   -- | tags indicating sample sites of interest
   -> [Tag]
   -- | [(accepted outputs, logps), samples)]
   -> Sampler [((a, LPTrace), InvSTrace)]
mhInternal n prog strace_0 tags = do
  -- Perform initial run of mh
  mhCtx_0 <- runMH strace_0 prog
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
  -> [((a, p), InvSTrace)]
  -- | updated trace of MH results
  -> Sampler [((a, p), InvSTrace)]
mhStep prog tags accepter trace = do
  -- Get previous MH output
  let mhCtx@(_, inv_strace) = head trace
  -- Get possible addresses to propose new samples for
  let αs = Map.keys (if Prelude.null tags then inv_strace else filterTrace tags inv_strace)
  -- Draw a proposal sample address
  α_samp_idx <- sample $ UniformD 0 (length αs - 1)
  let α_samp = αs !! α_samp_idx
  -- Draw a new random value
  x0 <- sampleRandom
  -- Run MH with proposal sample address to get an MHCtx using LPTrace as its probability type
  mhCtx'_lp <- runMH (Map.insert α_samp x0 inv_strace) prog
  -- Compute acceptance ratio to see if we use the proposed mhCtx'
  -- (which is mhCtx'_lp with 'LPTrace' converted to some type 'p')
  (mhCtx', acceptance_ratio) <- accepter α_samp mhCtx mhCtx'_lp
  u <- sample (Uniform 0 1)
  if u < acceptance_ratio
    then do return (mhCtx':trace)
    else do return trace

-- | Handler for one iteration of MH
runMH ::
  -- | sample trace of previous MH iteration
     InvSTrace
  -- | sample address of interest
  -> Prog [Observe, Sample, Lift Sampler] a
  -- | ((model output, sample trace), log-probability trace)
  -> Sampler ((a, LPTrace), InvSTrace)
runMH inv_strace  = handleLift . handleSamp inv_strace . SIM.handleObs . traceLogProbs

handleSamp ::
  -- | Sample trace
     InvSTrace
  -> Prog  [Sample, Lift Sampler] a
  -> Prog '[Lift Sampler] (a, InvSTrace)
handleSamp inv_strace (Val x)   = pure (x, inv_strace)
handleSamp inv_strace (Op op k) = case discharge op of
    Right (Sample (PrimDistPrf d) α) ->
      case Map.lookup α inv_strace of
          Nothing -> do r <- lift sampleRandom
                        y <- lift (sampleInv d r)
                        k' (Map.insert α r inv_strace) y
          Just r  -> do y <- lift (sampleInv d r)
                        k' inv_strace  y
    Left op' -> Op op' (k' inv_strace )

  where k' inv_strace' = handleSamp inv_strace' . k

{- | The result of a single MH iteration, where @a@ is the type of model output and
     @p@ is some representation of probability.
-}
type MHCtx p a = ((a, p), InvSTrace)

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
acceptMH x0 ((_, lptrace ), inv_strace) ((a, lptrace'), inv_strace') = do
  let dom_logα = log (fromIntegral $ Map.size inv_strace) - log (fromIntegral $ Map.size inv_strace')
      sampled  = Set.singleton x0 `Set.union` (Map.keysSet inv_strace \\ Map.keysSet inv_strace')
      sampled' = Set.singleton x0 `Set.union` (Map.keysSet inv_strace' \\ Map.keysSet inv_strace)
      logα     = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace))
                         0 (Map.keysSet lptrace \\ sampled)
      logα'    = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace'))
                         0 (Map.keysSet lptrace' \\ sampled')
  pure (((a, lptrace'), inv_strace'), (exp . unLogP) (dom_logα + logα' - logα))
