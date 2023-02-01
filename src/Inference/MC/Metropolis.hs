
{-# LANGUAGE RankNTypes #-}



{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

{- | Metropolis inference
-}

module Inference.MC.Metropolis where

import Control.Monad ( (>=>) )
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Data.Maybe ( fromJust )
import Comp
import Trace ( Trace, LPTrace, filterTrace )
import LogP ( LogP )
import PrimDist
import Model ( GenModel, handleCore, Model )
import Effects.EnvRW ( EnvRW )
import Env ( ContainsVars(..), Vars, Env )
import Effects.Dist ( Tag, Observe, Sample(..), Dist, Addr )
import qualified Inference.MC.SIM as SIM
import Sampler ( Sampler, random )

{- | The @Proposal@ effect for proposing samples and accepting/rejecting according a context.
-}
data Proposal p a where
  Propose
    -- | previous context and sample trace
    :: Trace
    -- | proposed *initial* context and sample trace
    -> Proposal p Trace
  Accept
    -- | previous context
    :: p
    -- | proposed *final* context
    -> p
    -- | whether the proposal is accepted or not
    -> Proposal p Bool

type ModelHandler es p = forall a. Model es a -> Trace -> Sampler ((a, p), Trace)

{- | A general framework for Metropolis inference.
-}
metropolis :: forall p fs es a. (Members [Proposal p, Sampler] fs)
   => Int                                                                    -- ^ number of iterations
   -> Trace                                                          -- ^ initial context + sample trace
   -> ModelHandler es p                                                        -- ^ model handler
   -> Model es a                                                             -- ^ probabilistic program
   -> Comp fs [((a, p), Trace)]                            -- ^ trace of accepted outputs
metropolis n τ_0 exec model = do
  -- | Perform initial run of mh
  x0 <- call (exec model τ_0)
  -- | A function performing n mhSteps using initial mh_s. The most recent samples are at the front of the trace.
  let metroStep' :: [((a, p), Trace)] -> Comp fs [((a, p), Trace)]
      metroStep' xs = metroStep model exec (head xs) >>= return . (:xs)
  foldl (>=>) pure (replicate n metroStep') [x0]

{- | Propose a new sample, execute the model, and then reject or accept the proposal.
-}
metroStep :: forall es fs p a. (Members [Proposal p, Sampler] fs)
  => Model es a                                                       -- ^ model handler
  -> ModelHandler es p                                                  -- ^ probabilistic program
  -> ((a, p), Trace)                                                   -- ^ previous trace
  -> Comp fs ((a, p), Trace)                            -- ^ updated trace
metroStep prog_0 exec ((r, p), τ) = do
  -- | Construct an *initial* proposal
  τ_0            <- call (Propose τ :: Proposal p Trace)
  -- | Execute the model under the initial proposal to return the *final* proposal
  ((r', p'), τ') <- call (exec prog_0 τ_0)
  -- | Compute acceptance ratio
  b              <- call (Accept p p')
  if b then pure ((r', p'), τ')
       else pure ((r, p), τ)

{- | Handler for @Sample@ that uses samples from a provided sample trace when possible and otherwise draws new ones.
-}
reuseTrace :: Member Sampler es => Trace -> Handler Sample es a (a, Trace)
reuseTrace τ0 = handle τ0 (\τ x -> Val (x, τ))
  (\τ (Sample d α) k ->
        case Map.lookup α τ of
              Nothing -> do r <- random
                            let y = draw d r;
                            k (Map.insert α r τ) y
              Just r  -> do let y = draw d r;
                            k τ y
  )