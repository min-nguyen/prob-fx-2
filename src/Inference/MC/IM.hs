
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
import Comp ( Handler, Comp(..), discharge, handle, LastMember, Member )
import Trace ( Trace, LPTrace, filterTrace )
import LogP ( LogP (..) )
import PrimDist
import Model ( GenModel, handleCore, Model )
import Effects.EnvRW ( EnvRW )
import Env ( Env )
import Effects.Dist ( Dist, pattern SampPrj, pattern ObsPrj )
import Sampler ( Sampler, random, handleIO )
import qualified Inference.MC.SIM as SIM
import qualified Inference.MC.LW as LW
import Inference.MC.Metropolis as Metropolis
import Util


{- | Top-level wrapper for Independence Metropolis
-}
im ::
     Int                              -- ^ number of iterations
  -> GenModel env [EnvRW env, Dist, Sampler] a  -- ^ model
  -> Env env                        -- ^ input environment
  -> Sampler [Env env]              -- ^ output model environments
im n model env_in   = do
  -- | Handle model to probabilistic program
  let prog_0  = handleCore env_in model
      τ_0     = Map.empty
  rwm_trace <- (handleIO . handleProposal . Metropolis.metropolis n τ_0 handleModel) prog_0
  pure (map (snd . fst . fst) rwm_trace)

{- | Handler for one iteration of IM.
-}
handleModel :: ModelHandler '[Sampler] LogP
handleModel prog τ =
  (handleIO . Metropolis.reuseTrace τ . LW.likelihood 0) prog

handleProposal :: Member Sampler fs => Handler (Proposal LogP) fs a a
handleProposal = handle () (\_ -> Val) (\_ op k -> hop op k)
  where hop :: Member Sampler es => Proposal LogP x -> (() -> x -> Comp es b) -> Comp es b
        hop op k = case op of
          (Propose τ)     -> do τ0 <- mapM (const random) τ
                                k () τ0
          (Accept lρ lρ') -> do let ratio = exp (lρ' - lρ)
                                u <- random
                                k () (ratio > u)