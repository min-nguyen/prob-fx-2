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
import           LogP ( LogP )
import           PrimDist
import           Model ( Model, handleCore, ProbProg )
import           Effects.ObsRW ( ObsRW )
import           Effects.Dist ( Tag, Observe, Sample(..), Dist, Addr(..), pattern SampPrj, pattern ObsPrj )
import           Effects.Lift ( Lift, lift, handleLift, liftPutStrLn, HasSampler, random', randomFrom' )
import           Inference.MC.SIM
import           Inference.MC.Metropolis as Metropolis
import           Sampler ( Sampler, sampleRandom, sampleUniformD, sampleRandomFrom )
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
  pure (map (snd . fst . fst) mh_trace)

{- | MH inference on a probabilistic program.
-}
mhInternal :: (HasSampler fs)
  => Int                                   -- ^ number of MH iterations
  -> [Tag]                                 -- ^ tags indicating variables of interest
  -> Trace                                -- ^ initial sample trace
  -> ProbProg a                            -- ^ probabilistic program
  -> Prog fs [((a, (Addr, LPTrace)), Trace)]
mhInternal n tags τ_0 =
  handleAccept tags . metropolis n (s_0, τ_0) handleModel
  where
    s_0 = (Addr 0 "" 0, Map.empty)

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
    Right (Propose τ)
      ->  do  α0 <- randomFrom' (Map.keys (if Prelude.null tags then τ else filterTrace tags τ))
              r0 <- random'
              let τ0 = Map.insert α0 r0 τ
              (loop . k) ((α0, Map.empty), τ0)
    Right (Accept (_, ρ) (α', ρ'))
      ->  do  let ratio = (exp . sum . Map.elems . Map.delete α') (Map.intersectionWith (-) ρ' ρ)
              u <- random'
              (loop . k) (ratio > u)
    Left op' -> Op op' (loop . k)

{- Propose a new random value at a single component x_i of latent variable X = {x_0, ... x_N}.
-}
propose
  :: [Tag]                        -- ^ observable variable names of interest
  -> Trace                       -- ^ original sample trace
  -> Sampler (Addr, Trace)       -- ^ (proposed addresses, proposed sample trace)
propose tags τ = do
  -- | Get possible addresses to propose new samples for
  let α_range = Map.keys (if Prelude.null tags then τ else filterTrace tags τ)
  -- | Draw proposal sample addresse
  α <- sampleRandomFrom α_range
  -- | Draw new random value
  r <- sampleRandom
  let τ' = Map.insert α r τ
  return (α, τ')

{- | Handler for one iteration of MH.
-}
handleModel ::
     ProbProg a                              -- ^ probabilistic program
  -> ((Addr, LPTrace), Trace)               -- ^ proposed address + initial log-probability trace + initial sample trace
  -> Sampler ((a, (Addr, LPTrace)), Trace)  -- ^ proposed address + final log-probability trace + final sample trace
handleModel prog ((α0, ρ0), τ0)  = do
  ((a, ρ), τ) <- (reuseSamples τ0 . defaultObserve . traceLP ρ0) prog
  return ((a, (α0, ρ)), τ)

{- | Record the log-probabilities at each @Sample@ or @Observe@ operation.
-}
traceLP :: LPTrace -> ProbProg a -> ProbProg (a, LPTrace)
traceLP ρ (Val x)   = pure (x, ρ)
traceLP ρ (Op op k) = case op of
  ObsPrj d y α   -> Op op (\x -> traceLP (Map.insert α (logProb d x) ρ) $ k x)
  SampPrj d  α   -> Op op (\x -> traceLP (Map.insert α (logProb d x) ρ) $ k x)
  _              -> Op op (traceLP ρ . k)
