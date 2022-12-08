
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Inference.MC.Gibbs where

import Control.Monad ( replicateM )
import qualified Data.Map as Map
import Prog ( Prog(..), discharge, LastMember )
import Trace ( STrace, LPTrace, filterTrace )
import LogP ( LogP (..), expLogP )
import PrimDist
import Model ( Model, handleCore, ProbProg )
import Effects.ObsRW ( ObsRW )
import Env ( Env, ContainsVars (varsToStrs), Vars )
import Effects.Dist ( Dist, Addr, Tag )
import Effects.Lift ( Lift, lift, handleLift, liftPutStrLn )
import Sampler ( Sampler, sampleRandom )
import qualified Inference.MC.SIM as SIM
import qualified Inference.MC.RWM as RWM
import Inference.MC.Metropolis as Metropolis
import Data.Bifunctor (Bifunctor(..))
import Util (assocR)

{- | Top-level wrapper for Gibbs inference.
-}
gibbs ::
     Int                            -- ^ number of Gibbs iterations
  -> Model env [ObsRW env, Dist] a  -- ^ model
  -> Env env                        -- ^ input environment
  -> Sampler [Env env]              -- ^ output environments
gibbs n model env_in   = do
  -- | Handle model to probabilistic program
  let prog_0   = handleCore env_in model
      ctx_0    = (0, LogP 0)
      strace_0 = Map.empty
  gibbs_trace <-  ( handleLift
                  . handleAccept
                  . metropolisLoop n (ctx_0, strace_0) handleModel) prog_0
  pure (map (snd . fst) gibbs_trace)

{- | Handler for one iteration of Gibbs.
-}
handleModel ::
     ((Int, LogP), STrace)               -- ^ proposed index + initial log-prob + initial sample trace
  -> ProbProg a                          -- ^ probabilistic program
  -> Sampler (a, ((Int, LogP), STrace))  -- ^ proposed index + final log-prob   + final sample trace
handleModel ((idx, logp), strace)  =
  (assocR . first (second (idx,)) <$>) . (Metropolis.reuseSamples strace . SIM.handleObs . RWM.weighJoint logp)

-- | For simplicity, the acceptance ratio is p(X', Y)/p(X, Y), but should be p(X' \ {x_i}, Y)/p(X \ {x_i}, Y)
handleAccept :: LastMember (Lift Sampler) fs => Prog (Accept (Int, LogP) : fs) a -> Prog fs a
handleAccept (Val x)    = pure x
handleAccept (Op op k) = case discharge op of
    Right (Propose ((idx, _), strace))
      ->  do r <- lift sampleRandom
             let prp_strace = Map.updateAt (\_ _ -> Just r) (idx `mod` length strace) strace
                 prp_ctx    = (idx + 1, LogP 0)
             (handleAccept . k) (prp_ctx, prp_strace)
    Right (Accept (_, logp) (_, logp'))
      ->  do u <- lift $ sample (mkUniform 0 1)
             (handleAccept . k) (expLogP (logp' - logp) > u)
    Left op' -> Op op' (handleAccept . k)