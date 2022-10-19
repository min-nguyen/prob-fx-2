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


{- | Template for Accept-Reject inference on a probabilistic program.
-}
arLoop :: ProbSig es
   => Int                                                                             -- ^ number of iterations
   -> STrace                                                                          -- ^ initial sample trace
   -> (forall es a. ProbSig es => STrace -> Prog es a -> Prog es ((a, ctx), STrace))  -- ^ model handler
   -> (forall es a. ProbSig es => Prog (Accept ctx : es) a -> Prog es a)              -- ^ accept handler
   -> Prog es a                                                                       -- ^ probabilistic program
   -> Prog es [((a, ctx), STrace)]  -- ^ trace of (accepted outputs, log probabilities), samples)
arLoop n strace hdlModel hdlAccept prog_0 = hdlAccept $ do
  let mh_prog_0 = weakenProg prog_0
  -- | Perform initial run of mh
  mh_ctx_0 <- hdlModel strace mh_prog_0
  -- | A function performing n mhSteps using initial mh_ctx. The most recent samples are at the front of the trace.
  foldl (>=>) pure (replicate n (arStep hdlModel mh_prog_0)) [mh_ctx_0]

{- | Propose a new sample, execute the model, and then reject or accept the proposal.
-}
arStep :: ProbSig es
  => (forall es a. ProbSig es => STrace -> Prog es a -> Prog es ((a, ctx), STrace))  -- ^ accept handler
  -> Prog (Accept ctx : es) a                                                        -- ^ probabilistic program
  -> [((a, ctx), STrace)]                                                            -- ^ previous MH trace
  -> Prog (Accept ctx : es) [((a, ctx), STrace)]                                     -- ^ updated MH trace
arStep hdlModel prog_0 trace = do
  -- | Get previous MH output
  let mh_ctx@((_, ctx), strace) = head trace
  -- | Propose a new random value for a sample site
  (α_samp, r)                  <- call (Propose strace ctx)
  -- | Run MH with proposed value
  mh_ctx'@((_, ctx'), strace') <- hdlModel (Map.insert α_samp r strace) prog_0
  -- | Compute acceptance ratio to see if we use the proposal
  b                            <- call (Accept α_samp ctx ctx')
  if b then pure (mh_ctx':trace)
       else pure trace
