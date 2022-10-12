{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

{- | Metropolis-Hastings inference.
-}

module Inference.MH where

import Control.Monad ( (>=>) )
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Data.Maybe ( fromJust )
import Prog ( Prog(..), discharge )
import Trace ( InvSTrace, LPTrace, filterTrace, traceLogProbs )
import LogP ( LogP(unLogP) )
import PrimDist
import Model ( Model, handleCore )
import Effects.ObsRW ( ObsRW )
import Env ( ContainsVars(..), Vars, Env )
import Effects.Dist ( Tag, Observe, Sample(..), Dist, Addr )
import Effects.Lift ( Lift, lift, handleLift )
import qualified Inference.SIM as SIM
import Sampler ( Sampler, sampleRandom )

{- | Top-level wrapper for MH inference.
-}
mh :: forall env es a xs. (env `ContainsVars` xs)
  => Int                                          -- ^ number of MH iterations
  -> Model env [ObsRW env, Dist, Lift Sampler] a  -- ^ model
  -> Env env                                      -- ^ input environment
  -> Vars xs                                      -- ^ optional observable variables
    {- These allow one to specify sample sites of interest; for example, for interest in sampling @#mu@, provide @#mu <#> vnil@ to cause other variables to not be resampled unless necessary. -}
  -> Sampler [Env env]                            -- ^ output model environments
mh n model env_in obs_vars  = do
  -- | Handle model to probabilistic program
  let prog = handleCore env_in model
  -- | Convert observable variables to strings
      tags = varsToStrs @env obs_vars
  -- | Run MH for n iterations
  mh_trace <- mhInternal n prog Map.empty tags
  -- | Return the accepted model environments
  pure (map (snd . fst . fst) mh_trace)

{- | Perform MH on a probabilistic program.
-}
mhInternal
   :: Int                                           -- ^ number of MH iterations
   -> Prog [Observe, Sample, Lift Sampler] a        -- ^ probabilistic program
   -> InvSTrace                                     -- ^ initial sample trace
   -> [Tag]                                         -- ^ tags indicating sample sites of interest
   -> Sampler [((a, LPTrace), InvSTrace)]           -- ^ trace of (accepted outputs, log probabilities), samples)
mhInternal n prog strace tags = do
  -- | Perform initial run of mh
  mh_ctx_0 <- runMH strace prog
  -- | A function performing n mhSteps using initial mh_ctx. The most recent samples are at the front of the trace.
  foldl (>=>) pure (replicate n (mhStep prog tags)) [mh_ctx_0]

{- | Perform one iteration of MH by drawing a new sample and then rejecting or accepting it.
-}
mhStep
  :: Prog [Observe, Sample, Lift Sampler] a         -- ^ probabilistic program
  -> [Tag]                                          -- ^ tags indicating sample sites of interest
  -> [((a, LPTrace), InvSTrace)]                    -- ^ previous MH trace
  -> Sampler [((a, LPTrace), InvSTrace)]            -- ^ updated MH trace
mhStep prog tags trace = do
  -- | Get previous MH output
  let mh_ctx@(_, strace) = head trace
  -- | Propose a new random value for a sample site
  (α_samp, r) <- propose strace tags
  -- | Run MH with proposed value to get an MHCtx using LPTrace as its probability type
  mh_ctx' <- runMH (Map.insert α_samp r strace) prog
  -- | Compute acceptance ratio to see if we use the proposed mh_ctx'
  b <- accept α_samp mh_ctx mh_ctx'
  if b then pure (mh_ctx':trace)
       else pure trace

{- | Handler for one iteration of MH.
-}
runMH ::
     InvSTrace                                -- ^ sample trace of previous MH iteration
  -> Prog [Observe, Sample, Lift Sampler] a   -- ^ probabilistic program
  -> Sampler ((a, LPTrace), InvSTrace)        -- ^ ((model output, sample trace), log-probability trace)
runMH strace  = handleLift . handleSamp strace . SIM.handleObs . traceLogProbs

{- | Handler for @Sample@ that uses samples from a provided sample trace when possible and otherwise draws new ones.
-}
handleSamp ::
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
accept ::
     Addr                             -- ^ address of proposal site
  -> ((a, LPTrace), InvSTrace)        -- ^ result of previous MH iteration
  -> ((a, LPTrace), InvSTrace)        -- ^ result of current MH iteration
  -> Sampler Bool                     -- ^ whether the current MH iteration is accepted
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