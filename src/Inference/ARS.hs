{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE FlexibleContexts #-}

{- | Accept-Reject Sampling
-}

module Inference.ARS where

import Control.Monad ( (>=>) )
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Data.Maybe ( fromJust )
import Prog ( Prog(..), discharge, Members, LastMember, Member (..), call, weakenProg )
import Trace ( STrace, LPTrace, filterTrace, traceLogProbs )
import LogP ( LogP(unLogP) )
import PrimDist
import Model ( Model, handleCore, ProbSig )
import Effects.ObsRW ( ObsRW )
import Env ( ContainsVars(..), Vars, Env )
import Effects.Dist ( Tag, Observe, Sample(..), Dist, Addr )
import Effects.Lift ( Lift, lift, handleLift )
import qualified Inference.SIM as SIM
import Sampler ( Sampler, sampleRandom )

data Accept ctx a where
  Propose
    :: (ctx, STrace)
    -> Accept ctx (Addr, Double)
  Accept
    :: Addr
    -- | original context
    -> ctx
    -- | context using proposed sample
    -> ctx
    -- | whether the proposal is accepted or not
    -> Accept ctx Bool

{- | Top-level wrapper for MH inference.
-}
mh :: forall env es a xs ctx. (env `ContainsVars` xs)
  => Int                                          -- ^ number of MH iterations
  -> Model env [ObsRW env, Dist, Lift Sampler] a  -- ^ model
  -> (forall es a. STrace -> Prog es a -> Prog es ((a, ctx), STrace)) -- model handler
  -> (forall es a. Prog (Accept ctx : es) a -> Prog es a)
  -> Env env                                      -- ^ input environment
  -> Vars xs                                      -- ^ optional observable variables
    {- These allow one to specify sample sites of interest; for example, for interest in sampling @#mu@, provide @#mu <#> vnil@ to cause other variables to not be resampled unless necessary. -}
  -> Sampler [Env env]                            -- ^ output model environments
mh n model hdlModel hdlAccept env_in obs_vars  = do
  -- | Handle model to probabilistic program
  let prog = handleCore env_in model
  -- | Convert observable variables to strings
      tags = varsToStrs @env obs_vars
  -- | Run MH for n iterations
  mh_trace <- (handleLift . SIM.handleSamp . SIM.handleObs . hdlAccept) (arInternal n prog hdlModel Map.empty tags)
  -- | Return the accepted model environments
  pure (map (snd . fst . fst) mh_trace)

{- | Perform MH on a probabilistic program.
-}
arInternal :: ProbSig es
   => Int                                           -- ^ number of MH iterations
   -> Prog es a                                     -- ^ probabilistic program
   -> (forall es. STrace -> Prog es a -> Prog es ((a, ctx), STrace))
   -> STrace                                        -- ^ initial sample trace
   -> [Tag]                                         -- ^ tags indicating sample sites of interest
   -> Prog (Accept ctx : es) [((a, ctx), STrace)]  -- ^ trace of (accepted outputs, log probabilities), samples)
arInternal n prog hdlModel strace tags = do
  let mh_prog = weakenProg prog
  -- | Perform initial run of mh
  mh_ctx_0 <- hdlModel strace mh_prog
  -- | A function performing n mhSteps using initial mh_ctx. The most recent samples are at the front of the trace.
  foldl (>=>) pure (replicate n (arStep mh_prog hdlModel)) [mh_ctx_0]

{- | Perform one iteration of MH by drawing a new sample and then rejecting or accepting it.
-}
arStep :: ProbSig es
  => Prog (Accept ctx : es) a                             -- ^ probabilistic program
  -> (forall es. STrace -> Prog es a -> Prog es ((a, ctx), STrace))
  -> [((a, ctx), STrace)]                                      -- ^ previous MH trace
  -> Prog (Accept ctx : es) [((a, ctx), STrace)]      -- ^ updated MH trace
arStep prog hdlModel trace = do
  -- | Get previous MH output
  let mh_ctx@((_, ctx), strace) = head trace
  -- | Propose a new random value for a sample site
  (α_samp, r)             <- call (Propose (ctx, strace))
  -- | Run MH with proposed value to get an MHCtx using LPTrace as its probability type
  mh_ctx'@((_, ctx'), strace') <- hdlModel (Map.insert α_samp r strace) prog
  -- | Compute acceptance ratio to see if we use the proposed mh_ctx'
  b                       <- call (Accept α_samp ctx ctx')
  if b then pure (mh_ctx':trace)
       else pure trace

{- | Handler for one iteration of MH.
-}
runMH :: ProbSig es
  => STrace                                 -- ^ sample trace of previous MH iteration
  -> Prog es a                              -- ^ probabilistic program
  -> Prog es ((a, LPTrace), STrace)         -- ^ ((model output, sample trace), log-probability trace)
runMH strace  = undefined -- handleSamp strace . traceLogProbs

-- {- | Handler for @Sample@ that uses samples from a provided sample trace when possible and otherwise draws new ones.
-- -}
-- handleSamp :: (Member Sample es, LastMember (Lift Sampler) es)
--   => STrace
--   -> Prog es a
--   -> Prog es (a, STrace)
-- handleSamp strace (Val x)   = pure (x, strace)
-- handleSamp strace (Op op k) = case prj op of
--     Just (Sample d α) ->
--       case Map.lookup α strace of
--           Nothing -> do r <- lift sampleRandom
--                         let y = sampleInv d r
--                         k' (Map.insert α r strace) y
--           Just r  -> do let y = sampleInv d r
--                         k' strace  y
--     Nothing -> Op op (k' strace )

--   where k' strace' = handleSamp strace' . k

-- {- | Handler for @Accept@ for MH.
-- -}
-- handleAccept :: LastMember (Lift Sampler) es => Prog (Accept (LPTrace, STrace) : es) a -> Prog es a
-- handleAccept (Val x)   = pure x
-- handleAccept (Op op k) = case discharge op of
--   Right (Accept α (lptrace, strace) (lptrace', strace'))
--     ->  do  let dom_logα = log (fromIntegral $ Map.size strace) - log (fromIntegral $ Map.size strace')
--                 sampled  = Set.singleton α `Set.union` (Map.keysSet strace \\ Map.keysSet strace')
--                 sampled' = Set.singleton α `Set.union` (Map.keysSet strace' \\ Map.keysSet strace)
--                 logα     = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace))
--                                   0 (Map.keysSet lptrace \\ sampled)
--                 logα'    = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace'))
--                                   0 (Map.keysSet lptrace' \\ sampled')
--             u <- lift $ sample (Uniform 0 1)
--             handleAccept $ k ((exp . unLogP) (dom_logα + logα' - logα) > u)
--   Left op' -> Op op' (handleAccept . k)
