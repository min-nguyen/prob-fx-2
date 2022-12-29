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
data Accept p a where
  Propose
    -- | previous context and sample trace
    :: (p, Trace)
    -- | proposed *initial* context and sample trace
    -> Accept p (p, Trace)
  Accept
    -- | previous context
    :: p
    -- | proposed *final* context
    -> p
    -- | whether the proposal is accepted or not
    -> Accept p Bool

type ModelHandler p = forall a. ProbProg a -> (p, Trace) -> Sampler ((a, p), Trace)

{- | A general framework for Metropolis inference.
-}
metropolis :: (HasSampler fs)
   => Int                                                                    -- ^ number of iterations
   -> (p, Trace)                                                          -- ^ initial context + sample trace
   -> ModelHandler p                                                        -- ^ model handler
   -> ProbProg a                                                             -- ^ probabilistic program
   -> Prog (Accept p : fs) [((a, p), Trace)]                            -- ^ trace of accepted outputs
metropolis n (p_0, τ_0) hdlModel prog_0 = do
  -- | Perform initial run of mh
  x0 <- lift (hdlModel prog_0 (p_0, τ_0))
  -- | A function performing n mhSteps using initial mh_s. The most recent samples are at the front of the trace.
  foldl (>=>) pure (replicate n (metroStep prog_0 hdlModel )) [x0]

{- | Propose a new sample, execute the model, and then reject or accept the proposal.
-}
metroStep :: (HasSampler fs)
  => ProbProg a                                                       -- ^ model handler
  -> ModelHandler p                                                  -- ^ probabilistic program
  -> [((a, p), Trace)]                                                   -- ^ previous trace
  -> Prog (Accept p : fs) [((a, p), Trace)]                            -- ^ updated trace
metroStep prog_0 hdlModel markov_chain = do
  -- | Get previous iteration output
  let ((_, p), τ) = head markov_chain
  -- | Construct an *initial* proposal
  prp             <- call (Propose (p, τ))
  -- | Execute the model under the initial proposal to return the *final* proposal
  ((r', p'), τ') <- lift (hdlModel prog_0 prp )
  -- | Compute acceptance ratio
  b              <- call (Accept p p')
  if b then pure (((r', p'), τ'):markov_chain)
       else pure markov_chain

{- | Handler for @Sample@ that uses samples from a provided sample trace when possible and otherwise draws new ones.
-}
reuseSamples :: Trace -> Prog '[Sample] a -> Sampler (a, Trace)
reuseSamples τ (Val x) = pure (x, τ)
reuseSamples τ (Op op k) = case discharge op of
  Right (Sample d α) ->  case Map.lookup α τ of
    Nothing -> do r <- sampleRandom
                  let y = draw d r
                  reuseSamples (Map.insert α r τ) (k y)
    Just r  -> do let y = draw d r
                  reuseSamples τ (k y)
  Left op'  -> error "MH.handleSamp: Left should not happen"
