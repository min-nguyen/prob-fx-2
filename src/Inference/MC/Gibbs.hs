
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
import Trace ( Trace, LPTrace, filterTrace )
import LogP ( LogP (..), expLogP )
import PrimDist
import Model ( Model, handleCore, ProbProg )
import Effects.ObsRW ( ObsRW )
import Env ( Env, ContainsVars (varsToStrs), Vars )
import Effects.Dist ( Dist, Addr, Tag )
import Effects.Lift ( Lift, lift, handleLift, liftPutStrLn, HasSampler )
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
      s_0    = (0, LogP 0)
      trace_0 = Map.empty
  gibbs_trace <-  ( handleLift
                  . handleAccept
                  . metroLoop n (s_0, trace_0) handleModel) prog_0
  pure (map (snd . fst) gibbs_trace)

{- | Handler for one iteration of Gibbs.
-}
handleModel ::
     ProbProg a                          -- ^ probabilistic program
  -> ((Int, LogP), Trace)               -- ^ proposed index + initial log-prob + initial sample trace
  -> Sampler (a, ((Int, LogP), Trace))  -- ^ proposed index + final log-prob   + final sample trace
handleModel prog ((idx, logp), trace)  =
  ((assocR . first (second (idx,)) <$>) . (Metropolis.reuseSamples trace . SIM.handleObs . RWM.joint logp)) prog

-- | For simplicity, the acceptance ratio is p(X', Y)/p(X, Y), but should be p(X' \ {x_i}, Y)/p(X \ {x_i}, Y)
handleAccept :: HasSampler fs => Prog (Accept (Int, LogP) : fs) a -> Prog fs a
handleAccept (Val x)    = pure x
handleAccept (Op op k) = case discharge op of
    Right (Propose ((idx, _), trace))
      ->  do r <- lift sampleRandom
             let prp_trace = Map.updateAt (\_ _ -> Just r) (idx `mod` length trace) trace
                 prp_s    = (idx + 1, LogP 0)
             (handleAccept . k) (prp_s, prp_trace)
    Right (Accept (_, logp) (_, logp'))
      ->  do u <- lift $ sample (mkUniform 0 1)
             (handleAccept . k) (expLogP (logp' - logp) > u)
    Left op' -> Op op' (handleAccept . k)