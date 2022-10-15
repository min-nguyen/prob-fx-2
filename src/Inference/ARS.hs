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
    -- | original sample trace
    :: STrace
    -- | original context
    -> ctx
    -> Accept ctx (Addr, Double)
  Accept
    -- | address of proposal site
    :: Addr
    -- | original context
    -> ctx
    -- | context using proposed sample
    -> ctx
    -- | whether the proposal is accepted or not
    -> Accept ctx Bool

{- | Top-level wrapper for MH inference.
-}
ar :: forall env a ctx es.
     Int                                          -- ^ number of MH iterations
  -> Model env [ObsRW env, Dist, Lift Sampler] a  -- ^ model
  -> (forall es a. ProbSig es => STrace -> Prog es a -> Prog es ((a, ctx), STrace)) -- model handler
  -> (forall es a. ProbSig es => Prog (Accept ctx : es) a -> Prog es a)
  -> Env env                                      -- ^ input environment
  -> Sampler [Env env]                            -- ^ output model environments
ar n model hdlModel hdlAccept env_in = do
  -- | Handle model to probabilistic program
  let prog = handleCore env_in model
  -- | Run MH for n iterations
  mh_trace <- (handleLift . SIM.handleSamp . SIM.handleObs . hdlAccept) (arInternal n prog hdlModel Map.empty)
  -- | Return the accepted model environments
  pure (map (snd . fst . fst) mh_trace)

{- | Perform MH on a probabilistic program.
-}
arInternal :: ProbSig es
   => Int                                           -- ^ number of MH iterations
   -> Prog es a                                     -- ^ probabilistic program
   -> (forall es a. ProbSig es => STrace -> Prog es a -> Prog es ((a, ctx), STrace))
   -> STrace                                        -- ^ initial sample trace
   -> Prog (Accept ctx : es) [((a, ctx), STrace)]  -- ^ trace of (accepted outputs, log probabilities), samples)
arInternal n prog hdlModel strace = do
  let mh_prog = weakenProg prog
  -- | Perform initial run of mh
  mh_ctx_0 <- hdlModel strace mh_prog
  -- | A function performing n mhSteps using initial mh_ctx. The most recent samples are at the front of the trace.
  foldl (>=>) pure (replicate n (arStep mh_prog hdlModel)) [mh_ctx_0]

{- | Propose a new sample, execute the model, and then reject or accept the proposal.
-}
arStep :: ProbSig es
  => Prog (Accept ctx : es) a                             -- ^ probabilistic program
  -> (forall es a. ProbSig es => STrace -> Prog es a -> Prog es ((a, ctx), STrace))
  -> [((a, ctx), STrace)]                                      -- ^ previous MH trace
  -> Prog (Accept ctx : es) [((a, ctx), STrace)]      -- ^ updated MH trace
arStep prog hdlModel trace = do
  -- | Get previous MH output
  let mh_ctx@((_, ctx), strace) = head trace
  -- | Propose a new random value for a sample site
  (α_samp, r)                  <- call (Propose strace ctx)
  -- | Run MH with proposed value to get an MHCtx using LPTrace as its probability type
  mh_ctx'@((_, ctx'), strace') <- hdlModel (Map.insert α_samp r strace) prog
  -- | Compute acceptance ratio to see if we use the proposed mh_ctx'
  b                            <- call (Accept α_samp ctx ctx')
  if b then pure (mh_ctx':trace)
       else pure trace
