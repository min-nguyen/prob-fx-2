
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
import           Model ( Model, handleCore, ProbProg )
import           Effects.EnvRW ( EnvRW )
import           Effects.Dist ( Tag, Observe, Sample(..), Dist, Addr(..), pattern SampPrj, pattern ObsPrj )
import           Effects.Lift ( handleM, liftPutStrLn, random', randomFrom' )
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
  -> Model env [EnvRW env, Dist] a  -- ^ model
  -> Env env                        -- ^ input environment
  -> Vars vars                      -- ^ optional variable names of interest
    {- These allow one to specify sample sites of interest; for example, for interest in sampling @#mu@
     , provide @#mu <#> vnil@ to cause other variables to not be resampled unless necessary. -}
  -> Sampler [Env env]              -- ^ output model environments
mh n model env_in obs_vars  = do
  -- | Handle model to probabilistic program
  let prog_0 = handleCore env_in model
      ??_0    = Addr "" 0
      ??_0    = Map.empty
  -- | Convert observable variables to strings
  let tags = varsToStrs @env obs_vars
  mh_trace <- (handleM . handleAccept tags ??_0 . metropolis n ??_0 handleModel) prog_0
  pure (map (snd . fst . fst) mh_trace)

{- | MH inference on a probabilistic program.
-}
ssmh :: (Member Sampler fs)
  => Int                                   -- ^ number of MH iterations
  -> Trace                                -- ^ initial sample trace
  -> Addr
  -> [Tag]                                 -- ^ tags indicating variables of interest
  -> ProbProg a                            -- ^ probabilistic program
  -> Prog fs [((a, LPTrace), Trace)]
ssmh n ??_0 ??_0 tags = handleAccept tags ??_0 . metropolis n ??_0 handleModel

{- | Handler for @Accept@ for MH.
    - Propose by drawing a component x_i of latent variable X' ~ p(X)
    - Accept using the ratio:
       p(X', Y')q(X | X')/p(X, Y)q(X' | X)
-}
handleAccept :: Member Sampler fs => [Tag] -> Addr -> Handler (Accept LPTrace) fs a a
handleAccept tags ??0 = handle ??0 (const Val) hop
  where
    hop :: Member Sampler es => Addr -> Accept LPTrace x -> (Addr -> x -> Prog es b) -> Prog es b
    hop _ (Propose ??) k   = do  ?? <- randomFrom' (Map.keys (if Prelude.null tags then ?? else filterTrace tags ??))
                                r <- random'
                                k ?? (Map.insert ?? r ??)
    hop ?? (Accept ?? ??') k = do  let ratio = (exp . sum . Map.elems . Map.delete ??) (Map.intersectionWith (-) ??' ??)
                                u <- random'
                                k ?? (ratio > u)

{- Propose a new random value at a single component x_i of latent variable X = {x_0, ... x_N}.
-}
propose
  :: [Tag]                        -- ^ observable variable names of interest
  -> Trace                       -- ^ original sample trace
  -> Sampler (Addr, Trace)       -- ^ (proposed addresses, proposed sample trace)
propose tags ?? = do
  -- | Get possible addresses to propose new samples for
  let ??_range = Map.keys (if Prelude.null tags then ?? else filterTrace tags ??)
  -- | Draw proposal sample addresse
  ?? <- sampleRandomFrom ??_range
  -- | Draw new random value
  r <- sampleRandom
  let ??' = Map.insert ?? r ??
  return (??, ??')

{- | Handler for one iteration of MH.
-}
handleModel ::
     ProbProg a                             -- ^ probabilistic program
  -> Trace                                  -- ^ proposed address + initial log-probability trace + initial sample trace
  -> Sampler ((a, LPTrace), Trace)  -- ^ proposed address + final log-probability trace + final sample trace
handleModel prog ??0 = (reuseSamples ??0 . defaultObserve . traceLP Map.empty) prog

{- | Record the log-probabilities at each @Sample@ or @Observe@ operation.
-}
traceLP :: Members [Observe, Sample] es
  => LPTrace -> Prog es a -> Prog es (a, LPTrace)
traceLP ?? (Val x)   = pure (x, ??)
traceLP ?? (Op op k) = case op of
  ObsPrj d y ??   -> Op op (\x -> traceLP (Map.insert ?? (logProb d x) ??) $ k x)
  SampPrj d  ??   -> Op op (\x -> traceLP (Map.insert ?? (logProb d x) ??) $ k x)
  _              -> Op op (traceLP ?? . k)
