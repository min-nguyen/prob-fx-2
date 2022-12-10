{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

{- | Single-Site Metropolis-Hastings inference.
-}

module Inference.MC.MH where

import           Data.Functor ( (<&>) )
import           Control.Monad ( (>=>), replicateM )
import qualified Data.Map as Map
import           Data.Set ((\\))
import qualified Data.Set as Set
import           Data.Maybe ( fromJust )
import           Prog ( Prog(..), discharge, Members, LastMember, Member (..), call, weakenProg, weaken )
import           Env ( ContainsVars(..), Vars, Env )
import           Trace ( Trace, LPTrace, filterTrace )
import           LogP ( LogP, expLogP )
import           PrimDist
import           Model ( Model, handleCore, ProbProg )
import           Effects.ObsRW ( ObsRW )
import           Effects.Dist ( Tag, Observe, Sample(..), Dist, Addr(..), pattern SampPrj, pattern ObsPrj )
import           Effects.Lift ( Lift, lift, handleLift, liftPutStrLn, HasSampler )
import qualified Inference.MC.SIM as SIM
import           Inference.MC.Metropolis as Metropolis
import           Sampler ( Sampler, sampleRandom )
import           Data.Bifunctor (Bifunctor(..))
import           Util ( assocR )

{- | Top-level wrapper for MH inference.
-}
mh :: forall env vars a. (env `ContainsVars` vars)
  => Int                            -- ^ number of MH iterations
  -> Model env [ObsRW env, Dist] a  -- ^ model
  -> Env env                        -- ^ input environment
  -> Vars vars                      -- ^ optional variable names of interest
    {- These allow one to specify sample sites of interest; for example, for interest in sampling @#mu@
     , provide @#mu <#> vnil@ to cause other variables to not be resampled unless necessary. -}
  -> Sampler [Env env]              -- ^ output model environments
mh n model env_in obs_vars  = do
  -- | Handle model to probabilistic program
  let prog_0   = handleCore env_in model
      trace_0 = Map.empty
  -- | Convert observable variables to strings
  let tags = varsToStrs @env obs_vars
  mh_trace <- handleLift (mhInternal n tags trace_0 prog_0)
  pure (map (snd . fst) mh_trace)

{- | MH inference on a probabilistic program.
-}
mhInternal :: (HasSampler fs)
  => Int                                   -- ^ number of MH iterations
  -> [Tag]                                 -- ^ tags indicating variables of interest
  -> Trace                                -- ^ initial sample trace
  -> ProbProg a                            -- ^ probabilistic program
  -> Prog fs [(a, ((Addr, LPTrace), Trace))]
mhInternal n tags trace_0 =
  handleAccept tags . metropolisLoop n (ctx_0, trace_0) handleModel
  where
    ctx_0 = (Addr 0 "" 0, Map.empty)

{- | Handler for one iteration of MH.
-}
handleModel ::
     ProbProg a                              -- ^ probabilistic program
  -> ((Addr, LPTrace), Trace)               -- ^ proposed address + initial log-probability trace + initial sample trace
  -> Sampler (a, ((Addr, LPTrace), Trace))  -- ^ proposed address + final log-probability trace + final sample trace
handleModel prog ((α, lptrace), trace)  = do
  ((assocR . first (second (α,)) <$>) . (Metropolis.reuseSamples trace . SIM.handleObs . traceLogProbs lptrace)) prog

{- | Record the log-probabilities at each @Sample@ or @Observe@ operation.
-}
traceLogProbs :: LPTrace -> ProbProg a -> ProbProg (a, LPTrace)
traceLogProbs lptrace (Val x)   = pure (x, lptrace)
traceLogProbs lptrace (Op op k) = case op of
  ObsPrj d y α   -> Op op (\x -> traceLogProbs (Map.insert α (logProb d x) lptrace) $ k x)
  SampPrj d  α   -> Op op (\x -> traceLogProbs (Map.insert α (logProb d x) lptrace) $ k x)
  _              -> Op op (traceLogProbs lptrace . k)

{- | Handler for @Accept@ for MH.
    - Propose by drawing a component x_i of latent variable X' ~ p(X)
    - Accept using the ratio:
       p(X', Y')q(X | X')/p(X, Y)q(X' | X)
-}
handleAccept :: HasSampler fs
  => [Tag]
  -> Prog (Accept (Addr, LPTrace) : fs) a
  -> Prog fs a
handleAccept tags = loop
 where
  loop (Val x)   = pure x
  loop (Op op k) = case discharge op of
    Right (Propose (_, trace))
      ->  do (α, prp_trace) <- lift (propose tags trace)
             let prp_ctx = (α, Map.empty)
             (loop . k) (prp_ctx, prp_trace)
    Right (Accept (_, lptrace) (α', lptrace'))
      ->  do  let dom_logα = log (fromIntegral $ Map.size lptrace) - log (fromIntegral $ Map.size lptrace')
                  sampled  = Set.singleton α' `Set.union` (Map.keysSet lptrace \\ Map.keysSet lptrace')
                  sampled' = Set.singleton α' `Set.union` (Map.keysSet lptrace' \\ Map.keysSet lptrace)
                  logα     = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace))
                                    0 (Map.keysSet lptrace \\ sampled)
                  logα'    = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace'))
                                    0 (Map.keysSet lptrace' \\ sampled')
              u <- lift $ sample (mkUniform 0 1)
              (loop . k) (expLogP (dom_logα + logα' - logα) > u)
    Left op' -> Op op' (loop . k)

{- Propose a new random value at a single component x_i of latent variable X = {x_0, ... x_N}.
-}
propose
  :: [Tag]                        -- ^ observable variable names of interest
  -> Trace                       -- ^ original sample trace
  -> Sampler (Addr, Trace)       -- ^ (proposed addresses, proposed sample trace)
propose tags trace = do
  -- | Get possible addresses to propose new samples for
  let α_range = Map.keys (if Prelude.null tags then trace else filterTrace tags trace)
  -- | Draw proposal sample addresse
  α <- sample (mkUniformD 0 (length α_range - 1)) <&> (α_range !!)
  -- | Draw new random value
  r <- sampleRandom
  let trace' = Map.insert α r trace
  return (α, trace')
