
{-# LANGUAGE RankNTypes #-}



{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

{- | Metropolis inference
-}

module Inference.MC.Metropolis where

import Control.Monad ( (>=>) )
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Data.Maybe ( fromJust )
import Prog
import Trace ( Trace, LPTrace, filterTrace )
import LogP ( LogP )
import PrimDist
import Model ( GenModel, handleCore, Model )
import Effects.EnvRW ( EnvRW )
import Env ( ContainsVars(..), Vars, Env )
import Effects.Dist ( Tag, Observe, Sample(..), Dist, Addr )
import Effects.Lift ( handleM, random' )
import qualified Inference.MC.SIM as SIM
import Sampler ( Sampler, sampleRandom )

{- | The @Accept@ effect for proposing samples and accepting/rejecting according a context.
-}
data Accept p a where
  Propose
    -- | previous context and sample trace
    :: Trace
    -- | proposed *initial* context and sample trace
    -> Accept p Trace
  Accept
    -- | previous context
    :: p
    -- | proposed *final* context
    -> p
    -- | whether the proposal is accepted or not
    -> Accept p Bool

type ModelHandler es p = forall a. Model es a -> Trace -> Sampler ((a, p), Trace)

{- | A general framework for Metropolis inference.
-}
metropolis :: (Members [Accept p, Sampler] fs)
   => Int                                                                    -- ^ number of iterations
   -> Trace                                                          -- ^ initial context + sample trace
   -> ModelHandler es p                                                        -- ^ model handler
   -> Model es a                                                             -- ^ probabilistic program
   -> Prog fs [((a, p), Trace)]                            -- ^ trace of accepted outputs
metropolis n τ_0 hdlModel prog_0 = do
  -- | Perform initial run of mh
  x0 <- call (hdlModel prog_0 τ_0)
  -- | A function performing n mhSteps using initial mh_s. The most recent samples are at the front of the trace.
  foldl (>=>) pure (replicate n (metroStep prog_0 hdlModel )) [x0]

{- | Propose a new sample, execute the model, and then reject or accept the proposal.
-}
metroStep :: forall es fs p a. (Members [Accept p, Sampler] fs)
  => Model es a                                                       -- ^ model handler
  -> ModelHandler es p                                                  -- ^ probabilistic program
  -> [((a, p), Trace)]                                                   -- ^ previous trace
  -> Prog fs [((a, p), Trace)]                            -- ^ updated trace
metroStep prog_0 hdlModel markov_chain = do
  -- | Get previous iteration output
  let ((r, p), τ) = head markov_chain
  -- | Construct an *initial* proposal
  τ_0            <- call (Propose τ :: Accept p Trace)
  -- | Execute the model under the initial proposal to return the *final* proposal
  ((r', p'), τ') <- call (hdlModel prog_0 τ_0)
  -- | Compute acceptance ratio
  b              <- call (Accept p p')
  if b then pure (((r', p'), τ'):markov_chain)
       else pure markov_chain

{- | Handler for @Sample@ that uses samples from a provided sample trace when possible and otherwise draws new ones.
-}
reuseSamples :: Member Sampler es => Trace -> Handler Sample es a (a, Trace)
reuseSamples τ0 = handle τ0 (\τ x -> Val (x, τ))
  (\τ (Sample d α) k ->
        case Map.lookup α τ of
              Nothing -> do r <- random'
                            let y = draw d r;
                            k (Map.insert α r τ) y
              Just r  -> do let y = draw d r;
                            k τ y
  )