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

{- | Random Walk Metropolis inference, where the prior proposal distribution is symmetric.
-}

module Inference.MC.RWM where

import Control.Monad ( replicateM )
import qualified Data.Map as Map
import Prog ( Prog(..), discharge, LastMember )
import Trace ( STrace, LPTrace, filterTrace )
import LogP ( LogP, expLogP )
import PrimDist
import Model ( Model, handleCore, ProbProg )
import Effects.ObsRW ( ObsRW )
import Env ( Env )
import Effects.Dist ( Dist, pattern SampPrj, pattern ObsPrj )
import Effects.Lift ( Lift, lift, handleLift, liftPutStrLn )
import Sampler ( Sampler, sampleRandom )
import qualified Inference.MC.SIM as SIM
import Inference.MC.Metropolis as Metropolis

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
      strace_0 = Map.empty
  rwm_trace <- (handleLift . handleAccept . metropolisLoop n strace_0 handleModel) prog_0
  pure (map (snd . fst . fst) rwm_trace)

{- | Handler for one iteration of RWM.
-}
handleModel ::
     STrace                             -- ^ sample trace of previous RWM iteration
  -> ProbProg a                         -- ^ probabilistic program
  -> Sampler ((a, LogP), STrace)        -- ^ ((model output, sample trace), log-probability trace)
handleModel strace =
  Metropolis.reuseSamples strace . SIM.handleObs . weighJoint

{- | Record the joint log-probability P(Y, X)
-}
weighJoint :: ProbProg a -> ProbProg (a, LogP)
weighJoint = loop 0 where
  loop :: LogP -> ProbProg a -> ProbProg (a, LogP)
  loop logp (Val x)   = pure (x, logp)
  loop logp (Op op k) = case op of
      ObsPrj d y α   -> Op op (\x -> loop (logp + logProb d x) $ k x)
      SampPrj d  α   -> Op op (\x -> loop (logp + logProb d x) $ k x)
      _              -> Op op (loop logp . k)

{- | Handler for @Accept@ for RWM.
     - Propose by drawing all components for latent variable X' ~ P(X)
     - Accept using the ratio P(X', Y')/P(X, Y)
-}
handleAccept :: LastMember (Lift Sampler) fs => Prog (Accept LogP : fs) a -> Prog fs a
handleAccept (Val x)   = pure x
handleAccept (Op op k) = case discharge op of
  Right (Propose strace logp)
    ->  do  let αs = Map.keys strace
            rs <- replicateM (length αs) (lift sampleRandom)
            let strace' = Map.union (Map.fromList (zip αs rs)) strace
            (handleAccept . k) (αs, strace')
  Right (Accept _ logp logp')
    ->  do  u <- lift $ sample (mkUniform 0 1)
            (handleAccept . k) (expLogP (logp' - logp) > u)
  Left op' -> Op op' (handleAccept . k)
