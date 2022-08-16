{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module Inference.MH where

import Control.Monad ( (>=>) )
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Data.Maybe ( fromJust )
import Prog ( Prog(..), discharge )
import Trace ( InvSTrace, LPTrace, filterTrace, traceLogProbs )
import LogP ( LogP(unLogP) )
import PrimDist ( PrimDist(..), sample, sampleInv )
import Model ( Model, handleCore )
import Effects.ObsRW ( ObsRW )
import Env ( ContainsVars(..), Vars, Env )
import Effects.Dist ( Tag, Observe, Sample(..), Dist, Addr )
import Effects.Lift ( Lift, lift, handleLift )
import qualified Inference.SIM as SIM
import Sampler ( Sampler, sampleRandom )

-- | Top-level wrapper for Metropolis-Hastings (MH) inference
mh :: forall env es a xs. (env `ContainsVars` xs)
  -- | number of MH iterations
  => Int
  -- | model
  -> Model env [ObsRW env, Dist, Lift Sampler] a
  -- | input model environment
  -> Env env
  -- | optional list of observable variable names (strings) to specify sample sites of interest
  {- for example, for interest in sampling @#mu@, provide @#mu <#> vnil@ to cause other variables
     to not be resampled unless necessary. -}
  -> Vars xs
  -- | output model environments
  -> Sampler [Env env]
mh n model env_in obs_vars  = do
  -- Handle model to probabilistic program
  let prog = handleCore env_in model
  -- Convert observable variables to strings
      tags = varsToStrs @env obs_vars
  -- Run MH for n iterations
  mh_trace <- mhInternal n prog Map.empty tags
  -- Return the accepted model environments
  pure (map (snd . fst . fst) mh_trace)

-- | Perform MH on a probabilistic program
mhInternal
   -- | number of MH iterations
   :: Int
   -- | probabilistic program
   -> Prog [Observe, Sample, Lift Sampler] a
   -- | initial sample trace
   -> InvSTrace
   -- | tags indicating sample sites of interest
   -> [Tag]
   -- | [(accepted outputs, logps), samples)]
   -> Sampler [((a, LPTrace), InvSTrace)]
mhInternal n prog strace tags = do
  -- Perform initial run of mh
  mh_ctx_0 <- runMH strace prog
  -- A function performing n mhSteps using initial mhCtx.
  -- Note: most recent samples are at the front (head) of the trace
  foldl (>=>) pure (replicate n (mhStep prog tags)) [mh_ctx_0]

-- | Perform one iteration of MH by drawing a new sample and then rejecting or accepting it.
mhStep
  :: Prog [Observe, Sample, Lift Sampler] a
  -- | tags indicating sample sites of interest
  -> [Tag]
  -- | trace of previous MH results
  -> [((a, LPTrace), InvSTrace)]
  -- | updated trace of MH results
  -> Sampler [((a, LPTrace), InvSTrace)]
mhStep prog tags trace = do
  -- Get previous MH output
  let mh_ctx@(_, strace) = head trace
  -- Propose a new random value for a sample site
  (α_samp, r) <- propose strace tags
  -- Run MH with proposed value to get an MHCtx using LPTrace as its probability type
  mh_ctx' <- runMH (Map.insert α_samp r strace) prog
  -- Compute acceptance ratio to see if we use the proposed mhCtx'
  -- (which is mhCtx'_lp with 'LPTrace' converted to some type 'p')
  b <- accept α_samp mh_ctx mh_ctx'
  if b then pure (mh_ctx':trace)
       else pure trace

-- | Handler for one iteration of MH
runMH ::
  -- | sample trace of previous MH iteration
     InvSTrace
  -- | sample address of interest
  -> Prog [Observe, Sample, Lift Sampler] a
  -- | ((model output, sample trace), log-probability trace)
  -> Sampler ((a, LPTrace), InvSTrace)
runMH strace  = handleLift . handleSamp strace . SIM.handleObs . traceLogProbs

-- | Handler for @Sample@ that uses samples from a provided sample trace when possible and otherwise draws new ones
handleSamp ::
  -- | Sample trace
     InvSTrace
  -> Prog  [Sample, Lift Sampler] a
  -> Prog '[Lift Sampler] (a, InvSTrace)
handleSamp strace (Val x)   = pure (x, strace)
handleSamp strace (Op op k) = case discharge op of
    Right (Sample d α) ->
      case Map.lookup α strace of
          Nothing -> do r <- lift sampleRandom
                        y <- lift (sampleInv d r)
                        k' (Map.insert α r strace) y
          Just r  -> do y <- lift (sampleInv d r)
                        k' strace  y
    Left op' -> Op op' (k' strace )

  where k' strace' = handleSamp strace' . k

{- | Propose a new random value at a random sample site.
-}
propose :: InvSTrace -> [Tag] -> Sampler (Addr, Double)
propose strace tags = do
  -- | Get possible addresses to propose new samples for
  let αs = Map.keys (if Prelude.null tags then strace else filterTrace tags strace)
  -- | Draw a proposal sample address
  α <- sample (UniformD 0 (length αs - 1)) >>= pure . (αs !!)
  -- | Draw a new random value
  r <- sampleRandom
  pure (α, r)

{- | An acceptance mechanism for MH.
-}
accept :: Addr
  -- | previous MH ctx, parameterised by @p@
  -> ((a, LPTrace), InvSTrace)
  -- | proposed MH ctx, parameterised by a log-probability map
  -> ((a, LPTrace), InvSTrace)
  -- | (proposed MH ctx using probability representation p, Acceptance ratio)
  -> Sampler Bool
accept x0 ((_, lptrace ), strace) ((a, lptrace'), strace') = do
  let dom_logα = log (fromIntegral $ Map.size strace) - log (fromIntegral $ Map.size strace')
      sampled  = Set.singleton x0 `Set.union` (Map.keysSet strace \\ Map.keysSet strace')
      sampled' = Set.singleton x0 `Set.union` (Map.keysSet strace' \\ Map.keysSet strace)
      logα     = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace))
                         0 (Map.keysSet lptrace \\ sampled)
      logα'    = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace'))
                         0 (Map.keysSet lptrace' \\ sampled')
      acceptance_ratio = (exp . unLogP) (dom_logα + logα' - logα)
  u <- sample (Uniform 0 1)
  pure (u < acceptance_ratio)