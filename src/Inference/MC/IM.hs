
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}



{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{- | Independence Metropolis inference, where proposals are independent of each other.
-}

module Inference.MC.IM where

import Control.Monad ( replicateM )
import qualified Data.Map as Map
import Comp ( Handler, Comp(..), discharge, handle, handleWith, LastMember, Member, runImpure )
import Trace ( Trace, LPTrace, filterTrace )
import LogP ( LogP (..) )
import Dist
import Model ( MulModel, conditionWith, Model )
import Effects.EnvRW ( EnvRW )
import Env ( Env )
import Effects.MulDist ( MulDist, pattern SampPrj, pattern ObsPrj )
import Sampler ( Sampler, random )
import qualified Inference.MC.SIM as SIM
import qualified Inference.MC.LW as LW
import Inference.MC.MH as MH
import Util


{- | Top-level wrapper for Independence Metropolis
-}
im ::
     Int                              -- ^ number of iterations
  -> MulModel env [EnvRW env, MulDist, Sampler] a  -- ^ model
  -> Env env                        -- ^ input environment
  -> Sampler [Env env]              -- ^ output model environments
im n gen_model env_in   = do
  -- | Handle model to probabilistic program
  let model  = conditionWith env_in gen_model
      τ_0     = Map.empty
  rwm_trace <- im' n model
  pure (map (snd . fst . fst) rwm_trace)

{- | Top-level wrapper for Independence Metropolis
-}
im' ::
     Int                              -- ^ number of iterations
  -> Model '[Sampler] a  -- ^ model
  -> Sampler [((a, LogP), Trace)]            -- ^ output model environments
im' n = runImpure . handleProposal . MH.mh n Map.empty exec

{- | Handler for one iteration of IM.
-}
exec :: ModelExec '[Sampler] LogP a
exec τ   =
  runImpure . MH.reuseTrace τ . LW.likelihood

handleProposal :: Member Sampler fs => Handler (Proposal LogP) fs a a
handleProposal = handle Val hop
  where hop :: Member Sampler es => Proposal LogP x -> (() -> x -> Comp es b) -> Comp es b
        hop op k = case op of
          (Propose τ)     -> do τ0 <- mapM (const random) τ
                                k () τ0
          (Accept lρ lρ') -> do let ratio = exp (lρ' - lρ)
                                u <- random
                                k () (ratio > u)