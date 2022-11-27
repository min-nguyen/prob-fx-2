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

import Data.Functor ( (<&>) )
import Control.Monad ( (>=>), replicateM )
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Data.Maybe ( fromJust )
import Prog ( Prog(..), discharge, Members, LastMember, Member (..), call, weakenProg, weaken )
import Trace ( STrace, LPTrace, filterTrace )
import LogP ( LogP, expLogP )
import PrimDist
import Model ( Model, handleCore, ProbProg )
import Effects.ObsRW ( ObsRW )
import Env ( ContainsVars(..), Vars, Env )
import Effects.Dist ( Tag, Observe, Sample(..), Dist, Addr, pattern SampPrj, pattern ObsPrj )
import Effects.Lift ( Lift, lift, handleLift, liftPutStrLn )
import Effects.State
import qualified Inference.MC.SIM as SIM
import Sampler ( Sampler, sampleRandom )
import Inference.MC.Metropolis
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
  reuseSamples strace . SIM.handleObs . weighJoint

{- | Record the joint log-probability P(X, Y)
-}
weighJoint :: ProbProg a -> ProbProg (a, LogP)
weighJoint = loop 0 where
  loop :: LogP -> ProbProg a -> ProbProg (a, LogP)
  loop logp (Val x)   = pure (x, logp)
  loop logp (Op op k) = case op of
      ObsPrj d y α   -> Op op (\x -> loop (logp + logProb d x) $ k x)
      SampPrj d  α   -> Op op (\x -> loop (logp + logProb d x) $ k x)
      _              -> Op op (loop logp . k)

{- | Handler for @Sample@ that uses samples from a provided sample trace when possible and otherwise draws new ones.
-}
reuseSamples :: forall a. STrace -> Prog '[Sample] a -> Sampler (a, STrace)
reuseSamples = loop where
  loop :: STrace -> Prog '[Sample] a -> Sampler (a, STrace)
  loop strace (Val x) = pure (x, strace)
  loop strace (Op op k) = case discharge op of
    Right (Sample d α) ->  case Map.lookup α strace of
      Nothing -> do r <- sampleRandom
                    let y = sampleInv d r
                    loop (Map.insert α r strace) (k y)
      Just r  -> do let y = sampleInv d r
                    loop strace  (k y)
    Left op'  -> error "MH.handleSamp: Left should not happen"

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
  Right (Accept αs logp logp')
    ->  do  u <- lift $ sample (mkUniform 0 1)
            (handleAccept . k) (expLogP (logp' - logp) > u)
  Left op' -> Op op' (handleAccept . k)
