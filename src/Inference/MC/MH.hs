
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}

{-# LANGUAGE TypeApplications #-}


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
import           Prog ( Prog(..), discharge, Members, LastMember, Member (..), call, weakenProg, weaken, Handler, handle )
import           Env ( ContainsVars(..), Vars, Env )
import           Trace ( Trace, LPTrace, filterTrace )
import           LogP ( LogP )
import           PrimDist
import           Model ( GenModel, handleCore, Model )
import           Effects.EnvRW ( EnvRW )
import           Effects.Dist ( Tag, Observe, Sample(..), Dist, Addr(..), pattern SampPrj, pattern ObsPrj )
import           Effects.IO ( handleIO, liftPutStrLn, random, randomFrom )
import           Inference.MC.SIM
import           Inference.MC.Metropolis as Metropolis
import           Sampler ( Sampler, sampleRandom, sampleUniformD, sampleRandomFrom )
import           Data.Bifunctor (Bifunctor(..))
import           Util ( assocR )
import Effects.State

{- | Top-level wrapper for MH inference.
-}
mh :: forall env vars a. (env `ContainsVars` vars)
  => Int                            -- ^ number of MH iterations
  -> GenModel env [EnvRW env, Dist, Sampler] a  -- ^ model
  -> Env env                        -- ^ input environment
  -> Vars vars                      -- ^ optional variable names of interest
    {- These allow one to specify sample sites of interest; for example, for interest in sampling @#mu@
     , provide @#mu <#> vnil@ to cause other variables to not be resampled unless necessary. -}
  -> Sampler [Env env]              -- ^ output model environments
mh n model env_in obs_vars  = do
  -- | Handle model to probabilistic program
  let prog_0 = handleCore env_in model
      τ_0    = Map.empty
  -- | Convert observable variables to strings
  let tags = varsToStrs @env obs_vars
  mh_trace <- (handleIO . handleProposal tags . metropolis n τ_0 handleModel) prog_0
  pure (map (snd . fst . fst) mh_trace)

{- | MH inference on a probabilistic program.
-}
ssmh :: (Member Sampler fs)
  => Int                                   -- ^ number of MH iterations
  -> Trace                                -- ^ initial sample trace
  -> [Tag]                                 -- ^ tags indicating variables of interest
  -> Model '[Sampler] a                            -- ^ probabilistic program
  -> Prog fs [((a, LPTrace), Trace)]
ssmh n τ_0  tags = handleProposal tags  . metropolis n τ_0 handleModel

{- | Handler for @Proposal@ for MH.
    - Propose by drawing a component x_i of latent variable X' ~ p(X)
    - Proposal using the ratio:
       p(X', Y')q(X | X')/p(X, Y)q(X' | X)
-}
handleProposal :: Member Sampler fs => [Tag] -> Handler (Proposal LPTrace) fs a a
handleProposal tags  = handle (Addr "" 0) (const Val) hop
  where
    hop :: Member Sampler es => Addr -> Proposal LPTrace x -> (Addr -> x -> Prog es b) -> Prog es b
    hop _ (Propose τ) k   = do  α <- randomFrom (Map.keys (if Prelude.null tags then τ else filterTrace tags τ))
                                r <- random
                                k α (Map.insert α r τ)
    hop α (Accept ρ ρ') k = do  let ratio = (exp . sum . Map.elems . Map.delete α) (Map.intersectionWith (-) ρ' ρ)
                                u <- random
                                k α (ratio > u)

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
     Model '[Sampler] a                             -- ^ probabilistic program
  -> Trace                                  -- ^ proposed address + initial log-probability trace + initial sample trace
  -> Sampler ((a, LPTrace), Trace)  -- ^ proposed address + final log-probability trace + final sample trace
handleModel prog τ0 = (handleIO . reuseTrace τ0 . defaultObserve . traceLP Map.empty) prog

{- | Record the log-probabilities at each @Sample@ or @Observe@ operation.
-}
traceLP :: Members [Observe, Sample] es
  => LPTrace -> Prog es a -> Prog es (a, LPTrace)
traceLP ρ (Val x)   = pure (x, ρ)
traceLP ρ (Op op k) = case op of
  ObsPrj d y α   -> Op op (\x -> traceLP (Map.insert α (logProb d x) ρ) $ k x)
  SampPrj d  α   -> Op op (\x -> traceLP (Map.insert α (logProb d x) ρ) $ k x)
  _              -> Op op (traceLP ρ . k)
