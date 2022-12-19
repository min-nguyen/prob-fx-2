{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

{- | Metropolis inference
-}

module Inference.MC.Metropolis where

import Control.Monad ( (>=>) )
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Data.Maybe ( fromJust )
import Prog ( Prog(..), discharge, Members, LastMember, Member (..), call, weakenProg )
import Trace ( Trace, LPTrace, filterTrace )
import LogP ( LogP )
import PrimDist
import Model ( Model, handleCore, ProbProg )
import Effects.ObsRW ( ObsRW )
import Env ( ContainsVars(..), Vars, Env )
import Effects.Dist ( Tag, Observe, Sample(..), Dist, Addr )
import Effects.Lift ( Lift, lift, handleLift, HasSampler )
import qualified Inference.MC.SIM as SIM
import Sampler ( Sampler, sampleRandom )

{- | The @Accept@ effect for proposing samples and accepting/rejecting according a context.
-}
data Accept s a where
  Propose
    -- | previous context and sample trace
    :: (s, Trace)
    -- | proposed *initial* context and sample trace
    -> Accept s (s, Trace)
  Accept
    -- | previous context
    :: s
    -- | proposed *final* context
    -> s
    -- | whether the proposal is accepted or not
    -> Accept s Bool

type ModelHandler s = forall a. ProbProg a -> (s, Trace) -> Sampler (a, (s, Trace))

{- | A general framework for Metropolis inference.
-}
metroLoop :: (HasSampler fs)
   => Int                                                                    -- ^ number of iterations
   -> (s, Trace)                                                          -- ^ initial context + sample trace
   -> ModelHandler s                                                        -- ^ model handler
   -> ProbProg a                                                             -- ^ probabilistic program
   -> Prog (Accept s : fs) [(a, (s, Trace))]                            -- ^ trace of accepted outputs
metroLoop n (s_0, trace_0) hdlModel prog_0 = do
  -- | Perform initial run of mh
  ar_s_0 <- lift (hdlModel prog_0 (s_0, trace_0))
  -- | A function performing n mhSteps using initial mh_s. The most recent samples are at the front of the trace.
  foldl (>=>) pure (replicate n (metroStep prog_0 hdlModel )) [ar_s_0]

{- | Propose a new sample, execute the model, and then reject or accept the proposal.
-}
metroStep :: (HasSampler fs)
  => ProbProg a                                                       -- ^ model handler
  ->  ModelHandler s                                                  -- ^ probabilistic program
  -> [(a, (s, Trace))]                                                   -- ^ previous trace
  -> Prog (Accept s : fs) [(a, (s, Trace))]                            -- ^ updated trace
metroStep prog_0 hdlModel markov_chain = do
  -- | Get previous iteration output
  let (_, (s, trace)) = head markov_chain
  -- | Construct an *initial* proposal
  prp                   <- call (Propose (s, trace))
  -- | Execute the model under the initial proposal to return the *final* proposal
  (r', (s', trace')) <- lift (hdlModel prog_0 prp )
  -- | Compute acceptance ratio
  b                     <- call (Accept s s')
  if b then pure ((r', (s', trace')):markov_chain)
       else pure markov_chain

{- | Handler for @Sample@ that uses samples from a provided sample trace when possible and otherwise draws new ones.
-}
reuseSamples :: Trace -> Prog '[Sample] a -> Sampler (a, Trace)
reuseSamples trace (Val x) = pure (x, trace)
reuseSamples trace (Op op k) = case discharge op of
  Right (Sample d α) ->  case Map.lookup α trace of
    Nothing -> do r <- sampleRandom
                  let y = draw d r
                  reuseSamples (Map.insert α r trace) (k y)
    Just r  -> do let y = draw d r
                  reuseSamples trace  (k y)
  Left op'  -> error "MH.handleSamp: Left should not happen"
