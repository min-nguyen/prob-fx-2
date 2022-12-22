{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE FlexibleContexts #-}

{- | Random Walk Metropolis inference, where the proposal distribution is symmetric.

    Note: As this uses the prior distribution as the proposal by default, and the prior is by definition *independent* of the previous sample, the prior will in general *not be symmetric*. This is because an independent proposal that is also symmetric will imply:

      q(x | x') = q(x)    ∧  q(x | x') = q(x' | x)  ⇒         q(x) = q(x')
      q is independent         q is symmetric           q is independent + symmetric

    which is not true for any two points x and x' unless q is a uniform distribution.

    This hence requires some extra work to be done to allow for a custom proposal/transition kernel.
-}

module Inference.MC.RWM where

import Control.Monad ( replicateM )
import qualified Data.Map as Map
import Prog ( Prog(..), discharge, LastMember )
import Trace ( Trace, LPTrace, filterTrace )
import LogP ( LogP (..), expLogP )
import PrimDist
import Model ( Model, handleCore, ProbProg )
import Effects.ObsRW ( ObsRW )
import Env ( Env )
import Effects.Dist ( Dist, pattern SampPrj, pattern ObsPrj )
import Effects.Lift ( Lift, lift, handleLift, liftPutStrLn, HasSampler, random' )
import Sampler ( Sampler, sampleRandom )
import qualified Inference.MC.SIM as SIM
import Inference.MC.Metropolis as Metropolis
import Util

{- | Top-level wrapper for Random-Walk Metropolis
-}
rwm ::
     Int                            -- ^ number of iterations
  -> Model env [ObsRW env, Dist] a  -- ^ model
  -> Env env                        -- ^ input environment
  -> Sampler [Env env]              -- ^ output model environments
rwm n model env_in   = do
  -- | Handle model to probabilistic program
  let prog_0   = handleCore env_in model
      s_0    = LogP 0
      trace_0 = Map.empty
  rwm_trace <- (handleLift . handleAccept . metroLoop n (s_0, trace_0) handleModel) prog_0
  pure (map (snd . fst) rwm_trace)

{- | Handler for one iteration of RWM.
-}
handleModel ::
      ProbProg a                         -- ^ probabilistic program
  -> (LogP, Trace)                     -- ^ sample trace of previous RWM iteration
  -> Sampler (a, (LogP, Trace))        -- ^ ((model output, sample trace), log-probability trace)
handleModel prog (lρ, τ)  = do
  ((assocR <$>) . Metropolis.reuseSamples τ . SIM.handleObs . joint lρ) prog

{- | Record the joint log-probability P(Y, X)
-}
joint :: LogP -> ProbProg a -> ProbProg (a, LogP)
joint lρ (Val x)   = pure (x, lρ)
joint lρ (Op op k) = case op of
  ObsPrj d y α   -> Op op (\x -> joint (lρ + logProb d x) $ k x)
  SampPrj d  α   -> Op op (\x -> joint (lρ + logProb d x) $ k x)
  _              -> Op op (joint lρ . k)

{- | Handler for @Accept@ for RWM.
     - Propose by drawing all components for latent variable X' ~ P(X)
     - Accept using the ratio P(X', Y')/P(X, Y)
-}
handleAccept :: HasSampler fs => Prog (Accept LogP : fs) a -> Prog fs a
handleAccept (Val x)   = pure x
handleAccept (Op op k) = case discharge op of
  Right (Propose (_, τ))
    ->  do  prp_τ <- mapM (const random') τ
            (handleAccept . k) (LogP 0, prp_τ)
  Right (Accept lρ lρ')
    ->  do  u <- random'
            (handleAccept . k) (expLogP (lρ' - lρ) > u)
  Left op' -> Op op' (handleAccept . k)
