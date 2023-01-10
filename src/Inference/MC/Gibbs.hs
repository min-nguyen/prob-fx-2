



{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Inference.MC.Gibbs where

import Control.Monad ( replicateM )
import qualified Data.Map as Map
import Prog ( Prog(..), discharge, LastMember, Member )
import Trace ( Trace, LPTrace, filterTrace )
import LogP ( LogP (..) )
import PrimDist
import Model ( Model, handleCore, ProbProg )
import Effects.EnvRW ( EnvRW )
import Env ( Env, ContainsVars (varsToStrs), Vars )
import Effects.Dist ( Dist, Addr, Tag )
import Effects.Lift ( Lift, lift, handleLift, liftPutStrLn, HasSampler, random' )
import Effects.State
import Sampler ( Sampler, sampleRandom )
import qualified Inference.MC.SIM as SIM
import qualified Inference.MC.LW as LW
import Inference.MC.Metropolis as Metropolis
import Data.Bifunctor (Bifunctor(..))
import Util (assocR)

{- | Top-level wrapper for Gibbs inference.
-}
gibbs ::
     Int                            -- ^ number of Gibbs iterations
  -> Model env [EnvRW env, Dist] a  -- ^ model
  -> Env env                        -- ^ input environment
  -> Sampler [Env env]              -- ^ output environments
gibbs n model env_in   = do
  -- | Handle model to probabilistic program
  let prog_0  = handleCore env_in model
      τ_0     = Map.empty
  gibbs_trace <-  ( handleLift
                  . evalState (0 :: Int)
                  . handleAccept
                  . metropolis n τ_0 handleModel) prog_0
  pure (map (snd . fst . fst) gibbs_trace)

{- | Handler for one iteration of Gibbs.
-}
handleModel ::
     ProbProg a                  -- ^ probabilistic program
  -> Trace                       -- ^ proposed index + initial log-prob + initial sample trace
  -> Sampler ((a, LogP), Trace)  -- ^ proposed index + final log-prob   + final sample trace
handleModel prog τ0  = do
  ((a, ρ), τ) <- (Metropolis.reuseSamples τ0 . SIM.defaultObserve . LW.joint 0) prog
  return ((a, ρ), τ)

-- | For simplicity, the acceptance ratio is p(X', Y)/p(X, Y), but should be p(X' \ {x_i}, Y)/p(X \ {x_i}, Y)
handleAccept :: (Member (State Int) fs, HasSampler fs) => Prog (Accept LogP : fs) a -> Prog fs a
handleAccept (Val x)    = pure x
handleAccept (Op op k) = case discharge op of
    Right (Propose τ)      ->
          do r <- random'
             idx <- get
             put (idx + 1)
             let prp_τ = Map.updateAt (\_ _ -> Just r) (idx `mod` length τ) τ
             (handleAccept . k) prp_τ
    Right (Accept lρ lρ')
      ->  do u <- random'
             (handleAccept . k) (exp (lρ' - lρ) > u)
    Left op' -> Op op' (handleAccept . k)