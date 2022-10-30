{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
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
import Trace ( STrace, LPTrace, filterTrace )
import LogP ( LogP )
import PrimDist
import Model ( Model, handleCore, ProbSig )
import Effects.ObsRW ( ObsRW )
import Env ( ContainsVars(..), Vars, Env )
import Effects.Dist ( Tag, Observe, Sample(..), Dist, Addr )
import Effects.Lift ( Lift, lift, handleLift )
import qualified Inference.SIM as SIM
import Sampler ( Sampler, sampleRandom )

{- | The @Accept@ effect for proposing samples and accepting/rejecting according a context.
-}
data Accept ctx a where
  Propose
    -- | original sample trace
    :: STrace
    -- | original context
    -> ctx
    -- | (proposed addresses, proposed sample trace)
    -> Accept ctx ([Addr], STrace)
  Accept
    -- | address of proposal sites
    :: [Addr]
    -- | original context
    -> ctx
    -- | context using proposed sample
    -> ctx
    -- | whether the proposal is accepted or not
    -> Accept ctx Bool

{- | Template for Accept-Reject inference on a probabilistic program.
-}
arLoop :: ProbSig es
   => Int                                                                             -- ^ number of iterations
   -> STrace                                                                          -- ^ initial sample trace
   -> (forall es a. ProbSig es => STrace -> Prog es a -> Prog es ((a, ctx), STrace))  -- ^ model handler

   -> Prog es a                                                                       -- ^ probabilistic program
   -> Prog (Accept ctx : es) [((a, ctx), STrace)]                                                    -- ^ trace of ((accepted outputs, contexts), samples)
arLoop n strace hdlModel prog_0 = do
  let ar_prog_0 = weakenProg prog_0
  -- | Perform initial run of mh
  ar_ctx_0 <- hdlModel strace ar_prog_0
  -- | A function performing n mhSteps using initial mh_ctx. The most recent samples are at the front of the trace.
  foldl (>=>) pure (replicate n (arStep hdlModel ar_prog_0)) [ar_ctx_0]

{- | Propose a new sample, execute the model, and then reject or accept the proposal.
-}
arStep :: ProbSig es
  => (forall es a. ProbSig es => STrace -> Prog es a -> Prog es ((a, ctx), STrace))  -- ^ model handler
  -> Prog (Accept ctx : es) a                                                        -- ^ probabilistic program
  -> [((a, ctx), STrace)]                                                            -- ^ previous trace
  -> Prog (Accept ctx : es) [((a, ctx), STrace)]                                     -- ^ updated trace
arStep hdlModel prog_0 trace = do
  -- | Get previous MH output
  let ((r, ctx), strace) = head trace
  -- | Propose a new random value for a sample site
  (prp_αs, prp_strace)         <- call (Propose strace ctx)
  -- | Run MH with proposed value
  ((r', ctx'), strace') <- hdlModel prp_strace prog_0
  -- | Compute acceptance ratio to see if we use the proposal
  b                            <- call (Accept prp_αs ctx ctx')
  if b then pure (((r', ctx'), strace'):trace)
       else pure trace
