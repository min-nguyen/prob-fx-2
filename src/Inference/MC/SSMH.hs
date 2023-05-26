
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
import           Comp ( Comp(..), Members, LastMember, Member (..), runImpure, call,  Handler, handleWith )
import           Env ( ContainsVars(..), Vars, Env )
import           Trace ( Trace, LPTrace, filterTrace )
import           LogP ( LogP )
import           Dist
import           Model ( MulModel, conditionWith, Model )
import           Effects.EnvRW ( EnvRW )
import           Effects.MulDist ( Tag, Observe, Sample(..), MulDist, Addr(..), pattern SampPrj, pattern ObsPrj )
import           Inference.MC.SIM
import           Inference.MC.MH as MH
import           Sampler ( Sampler, random, randomFrom )
import           Data.Bifunctor (Bifunctor(..))
import           Util ( assocR )
import Effects.State
import Effects.Observe

{- | Top-level wrapper for SSMH inference.
-}
ssmhWith :: forall env vars a. (env `ContainsVars` vars)
  => Int                            -- ^ number of SSMH iterations
  -> MulModel env [EnvRW env, MulDist, Sampler] a  -- ^ model
  -> Env env                        -- ^ input environment
  -> Vars vars                      -- ^ optional variable names of interest
    {- These allow one to specify sample sites of interest; for example, for interest in sampling @#mu@
     , provide @#mu <#> vnil@ to cause other variables to not be resampled unless necessary. -}
  -> Sampler [Env env]              -- ^ output model environments
ssmhWith n gen_model env_in obs_vars  = do
  -- | Handle model to probabilistic program
  let model = conditionWith env_in gen_model
      τ_0    = Map.empty
  -- | Convert observable variables to strings
  let tags = varsToStrs @env obs_vars
  mh_trace <- (runImpure . handleProposal tags . mh n τ_0 exec) model
  pure (map (snd . fst . fst) mh_trace)

{- | SSMH inference on a probabilistic program.
-}
ssmh :: Int                                   -- ^ number of SSMH iterations
  -> Trace                                -- ^ initial sample trace
  -> [Tag]                                 -- ^ tags indicating variables of interest
  -> Model '[Sampler] a                            -- ^ probabilistic program
  -> Sampler [((a, LPTrace), Trace)]
ssmh n τ_0  tags = runImpure . handleProposal tags  . mh n τ_0 exec

{- | Handler for @Propose@ for SSMH.
-}
handleProposal :: Member Sampler fs => [Tag] -> Handler (Propose LPTrace) fs a a
handleProposal tags  = handleWith (Addr "" 0) (const Val) hop
  where
    hop :: Member Sampler es => Addr -> Propose LPTrace x -> (Addr -> x -> Comp es b) -> Comp es b
    hop _ (Propose τ) k   = do
      α <- randomFrom (Map.keys (if Prelude.null tags then τ else filterTrace tags τ))
      r <- random
      k α (Map.insert α r τ)
    hop α (Accept ((x, w), τ) ((x', w'),  τ')) k = do
      let domτ  = (log . fromIntegral . Map.size) τ
          domτ' = (log . fromIntegral . Map.size) τ'
          ratio = (exp . sum . Map.elems . Map.delete α) (Map.intersectionWith (-) w' w) + domτ - domτ'
{-
    `ratio` is equivalent to Algorithm 2: [https://stuhlmueller.org/papers/lightweight-mcmc-aistats2011.pdf]
            (ll' - ll + R - F + ll_stale - ll_fresh + log|τ| - log|τ'|
        =  (ll' \ vars(ll_fresh)) − (ll \ vars(ll_stale)) + log|τ| - log|τ'|
    where:
      (1) F and R cancel out (due to using the model prior as a proposal)
      (2) Program executions with different numbers of observe operations (or differently addressed observe operations) are considered esoteric.
      (3) We exclude the likelihood of the proposed site α, following Sec 4.2 (Alg 6) [https://arxiv.org/pdf/1809.10756.pdf].
          I can't seem to find this detail in the Wingate lightweight-mcmc (2011) version.

    A full implementation with (2) would be:
      ll_samples  = Map.intersection w τ    -- get log-likelihoods of new samples
      ll_samples' = Map.intersection w' τ'  -- get log-likelihoods of old samples
      ll_stale = (sum . Map.elems . Map.delete α) (Map.difference ll_samples ll_samples')   -- sum log-likelihoods of stale samples
      ll_fresh = (sum . Map.elems . Map.delete α) (Map.difference ll_samples' ll_samples)   -- sum log-likelihoods of fresh samples
      ll       = (sum . Map.elems . Map.delete α) w  -- accounting for all old samples + observes
      ll'      = (sum . Map.elems . Map.delete α) w' -- accounting for all new samples + observes
      ratio   = ll' - ll + domτ - domτ' + ll_stale - ll_fresh
-}
      u <- random
      k α (if ratio > u
            then  ((x', w'), Map.intersection τ' w') -- | Remove stale trace entries not used during model execution
            else  ((x, w), τ))

{- | Model execution for one iteration of SSMH.
-}
exec :: ModelExec '[Sampler] LPTrace a
exec τ0 = runImpure . reuseTrace τ0 . defaultObserve . traceLP

{- | Record the log-probabilities at each @Sample@ or @Observe@ operation,
     always starting from an empty map.
-}
traceLP :: forall es a. Members [Observe, Sample] es
  => Comp es a -> Comp es (a, LPTrace)
traceLP  = loop Map.empty where
  loop :: LPTrace -> Comp es a -> Comp es (a, LPTrace)
  loop w (Val x) = Val (x, w)
  loop w (Op op k)
    | Just (Observe d y α) <- prj op = Op op (\x -> loop (Map.insert α (logProb d x) w) $ k x)
    | Just (Sample d  α)   <- prj op = Op op (\x -> loop (Map.insert α (logProb d x) w) $ k x)
    | otherwise                      = Op op (loop w . k)
