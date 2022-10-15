{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE FlexibleContexts #-}

{- | Metropolis-Hastings inference.
-}

module Inference.MH where

import Control.Monad ( (>=>) )
import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Data.Maybe ( fromJust )
import Prog ( Prog(..), discharge, Members, LastMember, Member (..), call, weakenProg )
import Trace ( STrace, LPTrace, filterTrace, traceLogProbs )
import LogP ( LogP(unLogP) )
import PrimDist
import Model ( Model, handleCore, ProbSig )
import Effects.ObsRW ( ObsRW )
import Env ( ContainsVars(..), Vars, Env )
import Effects.Dist ( Tag, Observe, Sample(..), Dist, Addr )
import Effects.Lift ( Lift, lift, handleLift )
import qualified Inference.SIM as SIM
import Sampler ( Sampler, sampleRandom )
import Inference.ARS

{- | Top-level wrapper for MH inference.
-}
mh :: forall env a xs. (env `ContainsVars` xs)
  => Int                                          -- ^ number of MH iterations
  -> Model env [ObsRW env, Dist, Lift Sampler] a  -- ^ model
  -> Env env                                      -- ^ input environment
  -> Vars xs                                      -- ^ optional observable variable names of interest
    {- These allow one to specify sample sites of interest; for example, for interest in sampling @#mu@
     , provide @#mu <#> vnil@ to cause other variables to not be resampled unless necessary. -}
  -> Sampler [Env env]                            -- ^ output model environments
mh n model env_in obs_vars  = do
  -- | Handle model to probabilistic program
  let prog_0   = handleCore env_in model
      strace_0 = Map.empty
  -- | Convert observable variables to strings
  let tags = varsToStrs @env obs_vars
  mh_trace <- (handleLift . SIM.handleSamp . SIM.handleObs)
              (mhInternal n tags strace_0 prog_0)
  pure (map (snd . fst . fst) mh_trace)

{- | MH inference on a probabilistic program.
-}
mhInternal :: ProbSig es
  => Int                                   -- ^ number of MH iterations
  -> [Tag]                                 -- ^ tags indicating variables of interest
  -> STrace                                -- ^ initial sample trace
  -> Prog es a                             -- ^ probabilistic program
  -> Prog es [((a, LPTrace), STrace)]
mhInternal n tags strace_0 =
  arLoop n strace_0 handleModel (handleAccept tags)

{- | Handler for one iteration of MH.
-}
handleModel :: ProbSig es
  => STrace                                 -- ^ sample trace of previous MH iteration
  -> Prog es a                              -- ^ probabilistic program
  -> Prog es ((a, LPTrace), STrace)         -- ^ ((model output, sample trace), log-probability trace)
handleModel strace  = handleSamp strace . traceLogProbs

{- | Handler for @Sample@ that uses samples from a provided sample trace when possible and otherwise draws new ones.
-}
handleSamp :: (Member Sample es, LastMember (Lift Sampler) es)
  => STrace
  -> Prog es a
  -> Prog es (a, STrace)
handleSamp strace (Val x)   = pure (x, strace)
handleSamp strace (Op op k) = case prj op of
    Just (Sample d α) ->
      case Map.lookup α strace of
          Nothing -> do r <- lift sampleRandom
                        let y = sampleInv d r
                        k' (Map.insert α r strace) y
          Just r  -> do let y = sampleInv d r
                        k' strace  y
    Nothing -> Op op (k' strace )

  where k' strace' = handleSamp strace' . k

{- | Handler for @Accept@ for MH.
-}
handleAccept :: LastMember (Lift Sampler) es
  => [Tag]                                 -- ^ observable variable names of interest
  -> Prog (Accept LPTrace : es) a
  -> Prog es a
handleAccept tags = loop
 where
  loop (Val x)   = pure x
  loop (Op op k) = case discharge op of
    Right (Propose strace lptrace)
      ->  do   -- | Get possible addresses to propose new samples for
              let αs = Map.keys (if Prelude.null tags then strace else filterTrace tags strace)
              -- | Draw a proposal sample address
              α <- lift (sample (UniformD 0 (length αs - 1))) >>= pure . (αs !!)
              -- | Draw a new random value
              r <- lift sampleRandom
              (loop . k) (α, r)
    Right (Accept α lptrace lptrace')
      ->  do  let dom_logα = log (fromIntegral $ Map.size lptrace) - log (fromIntegral $ Map.size lptrace')
                  sampled  = Set.singleton α `Set.union` (Map.keysSet lptrace \\ Map.keysSet lptrace')
                  sampled' = Set.singleton α `Set.union` (Map.keysSet lptrace' \\ Map.keysSet lptrace)
                  logα     = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace))
                                    0 (Map.keysSet lptrace \\ sampled)
                  logα'    = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace'))
                                    0 (Map.keysSet lptrace' \\ sampled')
              u <- lift $ sample (Uniform 0 1)
              (loop . k) ((exp . unLogP) (dom_logα + logα' - logα) > u)
    Left op' -> Op op' (loop . k)