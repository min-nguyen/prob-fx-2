{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

{- | Rejection Sampling
-}

module Inference.MC.Metropolis where

import Control.Monad ( (>=>) )
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Data.Maybe ( fromJust )
import Prog ( Prog(..), discharge, Members, LastMember, Member (..), call, weakenProg )
import Trace ( STrace, LPTrace, filterTrace )
import LogP ( LogP )
import PrimDist
import Model ( Model, handleCore, ProbProg )
import Effects.ObsRW ( ObsRW )
import Env ( ContainsVars(..), Vars, Env )
import Effects.Dist ( Tag, Observe, Sample(..), Dist, Addr )
import Effects.Lift ( Lift, lift, handleLift )
import qualified Inference.MC.SIM as SIM
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

type ModelHandler ctx = forall a. STrace -> ProbProg a -> Sampler ((a, ctx), STrace)

{- | A general framework for Metropolis inference.
-}
metropolisLoop :: (LastMember (Lift Sampler) fs)
   => Int                                                            -- ^ number of iterations
   -> STrace                                                         -- ^ initial sample trace
   -> (forall a. STrace -> ProbProg a -> Sampler ((a, ctx), STrace))  -- ^ model handler
   -> ProbProg a                                                      -- ^ probabilistic program
   -> Prog (Accept ctx : fs) [((a, ctx), STrace)]                    -- ^ trace of ((accepted outputs, contexts), samples)
metropolisLoop n strace hdlModel prog_0 = do
  -- | Perform initial run of mh
  ar_ctx_0 <- lift $ hdlModel strace prog_0
  -- | A function performing n mhSteps using initial mh_ctx. The most recent samples are at the front of the trace.
  foldl (>=>) pure (replicate n (metropolisStep hdlModel prog_0)) [ar_ctx_0]

{- | Propose a new sample, execute the model, and then reject or accept the proposal.
-}
metropolisStep :: (LastMember (Lift Sampler) fs)
  => (forall a. STrace -> ProbProg a -> Sampler ((a, ctx), STrace))  -- ^ model handler
  -> ProbProg a                                                        -- ^ probabilistic program
  -> [((a, ctx), STrace)]                                                            -- ^ previous trace
  -> Prog (Accept ctx : fs) [((a, ctx), STrace)]                                     -- ^ updated trace
metropolisStep hdlModel prog_0 trace = do
  -- | Get previous MH output
  let ((r, ctx), strace) = head trace
  -- | Propose a new random value for a sample site
  (prp_αs, prp_strace)  <- call (Propose strace ctx)
  -- | Run MH with proposed value
  ((r', ctx'), strace') <- lift (hdlModel prp_strace prog_0)
  -- | Compute acceptance ratio to see if we use the proposal
  b                     <- call (Accept prp_αs ctx ctx')
  if b then pure (((r', ctx'), strace'):trace)
       else pure trace

{- | Handler for @Sample@ that uses samples from a provided sample trace when possible and otherwise draws new ones.
-}
reuseSamples :: forall a. STrace -> Prog '[Sample] a -> Sampler (a, STrace)
reuseSamples = loop where
  loop :: STrace -> Prog '[Sample] a -> Sampler (a, STrace)
  loop strace (Val x) = pure (x, strace)
  loop strace (Op op k) = case discharge op of
    Right (Sample d α) ->  case Map.lookup α strace of
      Nothing -> do r <- sampleRandom
                    let y = sampleInv d r
                    loop (Map.insert α r strace) (k y)
      Just r  -> do let y = sampleInv d r
                    loop strace  (k y)
    Left op'  -> error "MH.handleSamp: Left should not happen"
