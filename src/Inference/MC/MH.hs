
{-# LANGUAGE RankNTypes #-}



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
import Dist
import Model ( MulModel, conditionWith, Model )
import Effects.EnvRW ( EnvRW )
import Env ( ContainsVars(..), Vars, Env )
import Effects.MulDist ( Tag, Observe, Sample(..), MulDist, Addr )
import qualified Inference.MC.SIM as SIM
import Sampler ( Sampler, random )

{- | The @Proposal@ effect for proposing samples and accepting/rejecting according a context.
-}
data Proposal w a where
  Propose
    -- | previous context and sample trace
    :: Trace
    -- | proposed *initial* context and sample trace
    -> Proposal w Trace
  Accept
    -- | previous context
    :: ((a, w), Trace)
    -- | proposed *final* context
    -> ((a, w), Trace)
    -- | whether the proposal is accepted or not
    -> Proposal w ((a, w), Trace)

type ModelExec es w a = Trace -> Model es a -> Sampler ((a, w), Trace)

{- | Handler for @Sample@ that uses samples from a provided sample trace when possible and otherwise draws new ones.
-}
reuseTrace :: Member Sampler es => Trace -> Handler Sample es a (a, Trace)
reuseTrace τ0 = handleWith τ0 (\τ x -> Val (x, τ))
  (\τ (Sample d α) k ->
        case Map.lookup α τ of
              Nothing -> do r <- random
                            let y = draw d r;
                            k (Map.insert α r τ) y
              Just r  -> do let y = draw d r;
                            k τ y
  )

-- reuseTrace' :: Member Sampler es => Trace -> Handler Sample es a (a, Trace)
-- reuseTrace' τ0 = handleWith Map.empty
--   (\τ x -> Val (x, τ))
--   (\τ (Sample d α) k ->
--         case Map.lookup α τ0 of
--               Nothing -> do r <- random
--                             let y = draw d r;
--                             k (Map.insert α r τ) y
--               Just r  -> do let y = draw d r;
--                             k (Map.insert α r τ) y
--   )

{- Original version, for benchmarking purposes -}
mh :: (Members [Proposal w, Sampler] fs)
   => Int                                                                    -- ^ number of iterations
   -> Trace                                                          -- ^ initial context + sample trace
   -> ModelExec es w a                                                       -- ^ model handler
   -> Model es a                                                             -- ^ probabilistic program
   -> Comp fs [((a, w), Trace)]                            -- ^ trace of accepted outputs
mh n τ_0 exec model = do
  -- | Perform initial run of mh
  x0 <- call (exec τ_0 model )
  -- | A function performing n mhSteps using initial mh_s. The most recent samples are at the front of the trace.
  foldl1 (>=>) (replicate n (mhStep model exec)) [x0]

mhStep :: forall es fs w a. (Members [Proposal w, Sampler] fs)
  => Model es a                                                       -- ^ model handler
  -> ModelExec es w a                                                 -- ^ probabilistic program
  -> [((a, w), Trace)]                                                   -- ^ previous trace
  -> Comp fs [((a, w), Trace)]                            -- ^ updated trace
mhStep model exec markov_chain = do
  -- | Get previous iteration output
  let ((r, w), τ) = head markov_chain
  -- | Construct an *initial* proposal
  τ_0            <- call (Propose τ :: Proposal w Trace)
  -- | Execute the model under the initial proposal to return the *final* proposal
  ((r', w'), τ') <- call (exec τ_0 model )
  -- | Compute acceptance ratio
  x              <- call (Accept ((r, w), τ)  ((r', w'), τ'))
  pure (x : markov_chain)

{- One function version of mh
mh' :: forall fs es a w. (Members [Proposal w, Sampler] fs)
   => Int -> Trace -> ModelExec es w a -> Model es a -> Comp fs [((a, w), Trace)]
mh' n τ_0 exec model = do
  -- | A function performing n mhSteps using initial mh_s.
  let mhStep :: Int -> [((a, w), Trace)] -> Comp fs [((a, w), Trace)]
      mhStep i mrkchain
        | i < n     = do
            let ((x, w), τ) = head mrkchain
            τ_0            <- call (Propose τ :: Proposal w Trace)
            ((x', w'), τ') <- call (exec τ_0 model )
            b              <- call (Accept ((x, w), τ) ((x', w'), τ'))
            -- let mrkchain'   =
            mhStep (i + 1) (b : mrkchain)
        | otherwise = return mrkchain
  -- | Perform initial run of mh
  node_0 <- call (exec τ_0 model )
  -- | Perform initial run of mh
  mhStep 0 [node_0]
-}

{- Paper version
mh :: forall w fs es a. (Members [Proposal w, Sampler] fs)
   => Int                                                                    -- ^ number of iterations
   -> Trace                                                          -- ^ initial context + sample trace
   -> ModelExec es w                                                        -- ^ model handler
   -> Model es a                                                             -- ^ probabilistic program
   -> Comp fs [((a, w), Trace)]                            -- ^ trace of accepted outputs
mh n τ_0 exec model = do
  -- | Perform initial run of mh
  x0 <- call (exec model τ_0)
  -- | A function performing n mhSteps using initial mh_s. The most recent samples are at the front of the trace.
  foldl1 (>=>) (replicate n f) [x0]
  where f :: [((a, w), Trace)] -> Comp fs [((a, w), Trace)]
        f mchain = metroStep model exec (head mchain) >>= return . (:mchain)

{- | Propose a new sample, execute the model, and then reject or accept the proposal.
-}
metroStep :: forall es fs w a. (Members [Proposal w, Sampler] fs)
  => Model es a                                                       -- ^ model handler
  -> ModelExec es w                                                  -- ^ probabilistic program
  -> ((a, w), Trace)                                                   -- ^ previous trace
  -> Comp fs ((a, w), Trace)                            -- ^ updated trace
metroStep model exec ((r, w), τ) = do
  -- | Construct an *initial* proposal
  τ_0            <- call (Propose τ :: Proposal w Trace)
  -- | Execute the model under the initial proposal to return the *final* proposal
  ((r', w'), τ') <- call (exec model τ_0)
  -- | Compute acceptance ratio
  b              <- call (Accept w w')
  if b then pure ((r', w'), τ')
       else pure ((r, w), τ)
-}
