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
import Trace ( STrace, LPTrace, filterTrace )
import LogP ( LogP (..), expLogP )
import PrimDist
import Model ( Model, handleCore, ProbProg )
import Effects.ObsRW ( ObsRW )
import Env ( Env )
import Effects.Dist ( Dist, pattern SampPrj, pattern ObsPrj )
import Effects.Lift ( Lift, lift, handleLift, liftPutStrLn, HasSampler )
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
      ctx_0    = LogP 0
      strace_0 = Map.empty
  rwm_trace <- (handleLift . handleAccept . metropolisLoop n (ctx_0, strace_0) handleModel) prog_0
  pure (map (snd . fst) rwm_trace)

{- | Handler for one iteration of RWM.
-}
handleModel ::
      ProbProg a                         -- ^ probabilistic program
  -> (LogP, STrace)                     -- ^ sample trace of previous RWM iteration
  -> Sampler (a, (LogP, STrace))        -- ^ ((model output, sample trace), log-probability trace)
handleModel prog (logp, strace)  = do
  ((assocR <$>) . Metropolis.reuseSamples strace . SIM.handleObs . joint logp) prog

{- | Record the joint log-probability P(Y, X)
-}
joint :: LogP -> ProbProg a -> ProbProg (a, LogP)
joint logp (Val x)   = pure (x, logp)
joint logp (Op op k) = case op of
  ObsPrj d y α   -> Op op (\x -> joint (logp + logProb d x) $ k x)
  SampPrj d  α   -> Op op (\x -> joint (logp + logProb d x) $ k x)
  _              -> Op op (joint logp . k)

{- | Handler for @Accept@ for RWM.
     - Propose by drawing all components for latent variable X' ~ P(X)
     - Accept using the ratio P(X', Y')/P(X, Y)
-}
handleAccept :: HasSampler fs => Prog (Accept LogP : fs) a -> Prog fs a
handleAccept (Val x)   = pure x
handleAccept (Op op k) = case discharge op of
  Right (Propose (_, strace))
    ->  do  let αs = Map.keys strace
            rs <- replicateM (length αs) (lift sampleRandom)
            let prp_strace = Map.union (Map.fromList (zip αs rs)) strace
                prp_ctx    = LogP 0
            -- liftPutStrLn (show $ rs)
            (handleAccept . k) (prp_ctx, prp_strace)
  Right (Accept logp logp')
    ->  do  u <- lift $ sample (mkUniform 0 1)
            -- liftPutStrLn (show $ logp' - logp)
            (handleAccept . k) (expLogP (logp' - logp) > u)
  Left op' -> Op op' (handleAccept . k)
