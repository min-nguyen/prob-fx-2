
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}



{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE FlexibleContexts #-}

{- | Independence Metropolis inference, where proposals are independent of each other.
-}

module Inference.MC.IM where

import Control.Monad ( replicateM )
import qualified Data.Map as Map
import Prog ( Prog(..), discharge, LastMember )
import Trace ( Trace, LPTrace, filterTrace )
import LogP ( LogP (..) )
import PrimDist
import Model ( Model, handleCore, ProbProg )
import Effects.ObsRW ( ObsRW )
import Env ( Env )
import Effects.Dist ( Dist, pattern SampPrj, pattern ObsPrj )
import Effects.Lift ( Lift, lift, handleLift, liftPutStrLn, HasSampler, random' )
import Sampler ( Sampler, sampleRandom )
import qualified Inference.MC.SIM as SIM
import qualified Inference.MC.LW as LW
import Inference.MC.Metropolis as Metropolis
import Util


{- | Top-level wrapper for Independence Metropolis
-}
im ::
     Int                            -- ^ number of iterations
  -> Model env [ObsRW env, Dist] a  -- ^ model
  -> Env env                        -- ^ input environment
  -> Sampler [Env env]              -- ^ output model environments
im n model env_in   = do
  -- | Handle model to probabilistic program
  let prog_0  = handleCore env_in model
      τ_0     = Map.empty
  rwm_trace <- (handleLift . handleAccept . Metropolis.metropolis n τ_0 handleModel) prog_0
  pure (map (snd . fst . fst) rwm_trace)

{- | Handler for one iteration of IM.
-}
handleModel ::
     ProbProg a                         -- ^ probabilistic program
  -> Trace                             -- ^ proposed initial log-prob + sample trace
  -> Sampler ((a, LogP), Trace)        -- ^ proposed final log-prob + sample trace
handleModel prog τ =
  (Metropolis.reuseSamples τ . LW.likelihood 0) prog

handleAccept :: HasSampler fs => Prog (Accept LogP : fs) a -> Prog fs a
handleAccept (Val x)   = pure x
handleAccept (Op op k) = case discharge op of
  Right (Propose τ)
    ->  do  τ0 <- mapM (const random') τ
            (handleAccept . k) τ0
  Right (Accept lρ lρ')
    ->  do  u <- random'
            (handleAccept . k) (exp (lρ' - lρ) > u)
  Left op' -> Op op' (handleAccept . k)
