
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

module Inference.MC.SSMH where

import           Data.Functor ( (<&>) )
import           Control.Monad ( (>=>), replicateM )
import qualified Data.Map as Map
import           Data.Set ((\\))
import qualified Data.Set as Set
import           Data.Maybe ( fromJust )
import           Comp ( Comp(..), discharge, Members, LastMember, Member (..), call, weakenProg, weaken, Handler, handleWith )
import           Env ( ContainsVars(..), Vars, Env )
import           Trace ( Trace, LPTrace, filterTrace )
import           LogP ( LogP )
import           Dist
import           Model ( MulModel, conditionWith, Model )
import           Effects.EnvRW ( EnvRW )
import           Effects.MulDist ( Tag, Observe, Sample(..), MulDist, Addr(..), pattern SampPrj, pattern ObsPrj )
import           Inference.MC.SIM
import           Inference.MC.MH as MH
import           Sampler ( Sampler, random, randomFrom, handleIO )
import           Data.Bifunctor (Bifunctor(..))
import           Util ( assocR )
import Effects.State
import Effects.Observe

{- | Top-level wrapper for SSMH inference.
-}
ssmh :: forall env vars a. (env `ContainsVars` vars)
  => Int                            -- ^ number of SSMH iterations
  -> MulModel env [EnvRW env, MulDist, Sampler] a  -- ^ model
  -> Env env                        -- ^ input environment
  -> Vars vars                      -- ^ optional variable names of interest
    {- These allow one to specify sample sites of interest; for example, for interest in sampling @#mu@
     , provide @#mu <#> vnil@ to cause other variables to not be resampled unless necessary. -}
  -> Sampler [Env env]              -- ^ output model environments
ssmh n gen_model env_in obs_vars  = do
  -- | Handle model to probabilistic program
  let model = conditionWith env_in gen_model
      τ_0    = Map.empty
  -- | Convert observable variables to strings
  let tags = varsToStrs @env obs_vars
  mh_trace <- (handleIO . handleProposal tags . mh n τ_0 exec) model
  pure (map (snd . fst . fst) mh_trace)

{- | SSMH inference on a probabilistic program.
-}
ssmh' :: Int                                   -- ^ number of SSMH iterations
  -> Trace                                -- ^ initial sample trace
  -> [Tag]                                 -- ^ tags indicating variables of interest
  -> Model '[Sampler] a                            -- ^ probabilistic program
  -> Sampler [((a, LPTrace), Trace)]
ssmh' n τ_0  tags = handleIO . handleProposal tags  . mh n τ_0 exec

{- | Handler for @Proposal@ for SSMH.
    - Propose by drawing a component x_i of latent variable X' ~ p(X)
    - Proposal using the ratio:
       p(X', Y')q(X | X')/p(X, Y)q(X' | X)
-}
handleProposal :: Member Sampler fs => [Tag] -> Handler (Proposal LPTrace) fs a a
handleProposal tags  = handleWith (Addr "" 0) (const Val) hop
  where
    hop :: Member Sampler es => Addr -> Proposal LPTrace x -> (Addr -> x -> Comp es b) -> Comp es b
    hop _ (Propose τ) k   = do  α <- randomFrom (Map.keys (if Prelude.null tags then τ else filterTrace tags τ))
                                r <- random
                                k α (Map.insert α r τ)
    hop α (Accept ρ ρ') k = do  let ratio = (exp . sum . Map.elems . Map.delete α) (Map.intersectionWith (-) ρ' ρ)
                                u <- random
                                k α (ratio > u)

{- | Handler for one iteration of SSMH.
-}
exec :: Trace
  -> Model '[Sampler] a                             -- ^ probabilistic program
  -> Sampler ((a, LPTrace), Trace)  -- ^ proposed address + final log-probability trace + final sample trace
exec τ0 = handleIO . reuseTrace τ0 . defaultObserve . traceLP Map.empty

{- | Record the log-probabilities at each @Sample@ or @Observe@ operation.
-}
traceLP :: Members [Observe, Sample] es
  => LPTrace -> Comp es a -> Comp es (a, LPTrace)
traceLP ρ (Val x)   = pure (x, ρ)
traceLP ρ (Op op k)
  | Just (Observe d y α) <- prj op = Op op (\x -> traceLP (Map.insert α (logProb d x) ρ) $ k x)
  | Just (Sample d  α)   <- prj op = Op op (\x -> traceLP (Map.insert α (logProb d x) ρ) $ k x)
  | otherwise                      = Op op (traceLP ρ . k)
