
{-# LANGUAGE RankNTypes #-}



{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

{- | Metropolis inference
-}

module Inference.MC.MH where

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
import Sampler ( Sampler, random, handleIO )

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

type ModelHandler es p a = Trace -> Model es a -> Sampler ((a, p), Trace)

{- | Handler for @Sample@ that uses samples from a provided sample trace when possible and otherwise draws new ones.
-}
reuseTrace :: Member Sampler es => Trace -> Handler Sample es a (a, Trace)
reuseTrace τ0 = handleSt τ0 (\τ x -> Val (x, τ))
  (\τ (Sample d α) k ->
        case Map.lookup α τ of
              Nothing -> do r <- random
                            let y = draw d r;
                            k (Map.insert α r τ) y
              Just r  -> do let y = draw d r;
                            k τ y
  )

{- Original version, for benchmarking purposes -}
mh :: -- (Member Sampler fs)
      Int                                                                    -- ^ number of iterations
   -> Trace                                                          -- ^ initial context + sample trace
   -> (forall b. Handler (Proposal p) '[Sampler] b b)
   -> ModelHandler es p a                                                       -- ^ model handler
   -> Model es a                                                             -- ^ probabilistic program
   -> Sampler [((a, p), Trace)]                            -- ^ trace of accepted outputs
mh n τ_0 hdlProposal exec model = handleIO . hdlProposal $ do
  -- | Perform initial run of mh
  x0 <- call (exec τ_0 model )
  -- | A function performing n mhSteps using initial mh_s. The most recent samples are at the front of the trace.
  foldl1 (>=>) (replicate n (mhStep model exec)) [x0]

mhStep :: forall es fs p a. (Members [Proposal p, Sampler] fs)
  => Model es a                                                       -- ^ model handler
  -> ModelHandler es p a                                                 -- ^ probabilistic program
  -> [((a, p), Trace)]                                                   -- ^ previous trace
  -> Comp fs [((a, p), Trace)]                            -- ^ updated trace
mhStep prog_0 exec markov_chain = do
  -- | Get previous iteration output
  let ((r, p), τ) = head markov_chain
  -- | Construct an *initial* proposal
  τ_0            <- call (Propose τ :: Proposal p Trace)
  -- | Execute the model under the initial proposal to return the *final* proposal
  ((r', p'), τ') <- call (exec τ_0 prog_0 )
  -- | Compute acceptance ratio
  b              <- call (Accept p p')
  if b then pure (((r', p'), τ'):markov_chain)
       else pure markov_chain
{--}



{- Paper version
mh :: forall p fs es a. (Members [Proposal p, Sampler] fs)
   => Int                                                                    -- ^ number of iterations
   -> Trace                                                          -- ^ initial context + sample trace
   -> ModelHandler es p                                                        -- ^ model handler
   -> Model es a                                                             -- ^ probabilistic program
   -> Comp fs [((a, p), Trace)]                            -- ^ trace of accepted outputs
mh n τ_0 exec model = do
  -- | Perform initial run of mh
  x0 <- call (exec model τ_0)
  -- | A function performing n mhSteps using initial mh_s. The most recent samples are at the front of the trace.
  foldl1 (>=>) (replicate n f) [x0]
  where f :: [((a, p), Trace)] -> Comp fs [((a, p), Trace)]
        f mchain = metroStep model exec (head mchain) >>= return . (:mchain)

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
-}

mh' :: forall es p a. Int -> Trace -> (forall fs b. Handler (Proposal p) fs b b) -> ModelHandler es p a -> Model es a -> Sampler [((a, p), Trace)]
mh' n τ_0 hdlProposal exec prog_0 = handleIO . hdlProposal $ do
  -- | A function performing n mhSteps using initial mh_s.
  let loop :: Int -> [((a, p), Trace)] -> Comp [Proposal p, Sampler] [((a, p), Trace)]
      loop i mrkchain
        | i < n     = do
            let ((x, w), τ) = head mrkchain
            τ_0            <- call (Propose τ :: Proposal p Trace)
            ((x', w'), τ') <- call (exec τ_0 prog_0 )
            b              <- call (Accept w w')
            -- let mrkchain'   =
            loop (i + 1) (if b then ((x', w'), τ') : mrkchain else ((x, w), τ) : mrkchain)
        | otherwise = return mrkchain
  -- | Perform initial run of mh
  node_0 <- call (exec τ_0 prog_0 )
  -- | Perform initial run of mh
  loop 0 [node_0]

{- One function version of mh

-}