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

{- | Metropolis-Hastings inference.
-}

module Inference.MH where

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
import qualified Inference.SIM as SIM
import Sampler ( Sampler, sampleRandom )
import Inference.ARS
import Util

{- | Top-level wrapper for MH inference.
-}
mh :: forall env a xs. (env `ContainsVars` xs)
  => Int                            -- ^ number of MH iterations
  -> Model env [ObsRW env, Dist] a  -- ^ model
  -> Env env                        -- ^ input environment
  -> Vars xs                        -- ^ optional observable variable names of interest
    {- These allow one to specify sample sites of interest; for example, for interest in sampling @#mu@
     , provide @#mu <#> vnil@ to cause other variables to not be resampled unless necessary. -}
  -> Sampler [Env env]              -- ^ output model environments
mh n model env_in obs_vars  = do
  -- | Handle model to probabilistic program
  let prog_0   = handleCore env_in model
      strace_0 = Map.empty
  -- | Convert observable variables to strings
  let tags = varsToStrs @env obs_vars
  mh_trace <- handleLift (mhInternal n tags strace_0 prog_0)
  pure (map (snd . fst . fst) mh_trace)

{- | MH inference on a probabilistic program.
-}
mhInternal :: (LastMember (Lift Sampler) fs)
  => Int                                   -- ^ number of MH iterations
  -> [Tag]                                 -- ^ tags indicating variables of interest
  -> STrace                                -- ^ initial sample trace
  -> ProbProg a                             -- ^ probabilistic program
  -> Prog fs [((a, LPTrace), STrace)]
mhInternal n tags strace_0 =
  handleAccept tags 1 . arLoop n strace_0 handleModel

{- | Handler for one iteration of MH.
-}
handleModel ::
     STrace                                 -- ^ sample trace of previous MH iteration
  -> ProbProg a                             -- ^ probabilistic program
  -> Sampler ((a, LPTrace), STrace)         -- ^ ((model output, sample trace), log-probability trace)
handleModel strace =
  handleSamp strace . SIM.handleObs . traceLogProbs

{- | Insert stateful operations for recording the log-probabilities at each @Sample@ or @Observe@ operation.
-}
traceLogProbs :: ProbProg a -> ProbProg (a, LPTrace)
traceLogProbs = loop Map.empty where
  loop :: LPTrace -> ProbProg a -> ProbProg (a, LPTrace)
  loop lptrace (Val x)   = pure (x, lptrace)
  loop lptrace (Op op k) = case op of
      ObsPrj d y α   -> Op op (\x -> loop (Map.insert α (logProb d x) lptrace) $ k x)
      SampPrj d  α   -> Op op (\x -> loop (Map.insert α (logProb d x) lptrace) $ k x)
      _              -> Op op (loop lptrace . k)

{- | Handler for @Sample@ that uses samples from a provided sample trace when possible and otherwise draws new ones.
-}
handleSamp :: forall a. STrace -> Prog '[Sample] a -> Sampler (a, STrace)
handleSamp = loop where
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

{- | Handler for @Accept@ for MH.
-}
handleAccept :: LastMember (Lift Sampler) fs
  => [Tag]                                 -- ^ observable variable names of interest
  -> Int                                   -- ^ number of proposal sites
  -> Prog (Accept LPTrace : fs) a
  -> Prog fs a
handleAccept tags n_proposals = loop
 where
  loop (Val x)   = pure x
  loop (Op op k) = case discharge op of
    Right (Propose strace lptrace)
      ->  lift (propose tags n_proposals strace) >>= (loop . k)
    Right (Accept αs lptrace lptrace')
      ->  do  let dom_logα = log (fromIntegral $ Map.size lptrace) - log (fromIntegral $ Map.size lptrace')
                  sampled  = Set.fromList αs `Set.union` (Map.keysSet lptrace \\ Map.keysSet lptrace')
                  sampled' = Set.fromList αs `Set.union` (Map.keysSet lptrace' \\ Map.keysSet lptrace)
                  logα     = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace))
                                    0 (Map.keysSet lptrace \\ sampled)
                  logα'    = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace'))
                                    0 (Map.keysSet lptrace' \\ sampled')
              u <- lift $ sample (mkUniform 0 1)
              (loop . k) (expLogP (dom_logα + logα' - logα) > u)
    Left op' -> Op op' (loop . k)

{- Propose new random values for multiple sites.
-}
propose
  :: [Tag]                          -- ^ observable variable names of interest
  -> Int                            -- ^ number of proposal sites
  -> STrace                         -- ^ original sample trace
  -> Sampler ([Addr], STrace)       -- ^ (proposed addresses, proposed sample trace)
propose tags n_proposals strace = do
  -- | Get possible addresses to propose new samples for
  let α_range = Map.keys (if Prelude.null tags then strace else filterTrace tags strace)
  -- | Draw proposal sample addresses
  αs <- replicateM n_proposals (sample (mkUniformD 0 (length α_range - 1)) <&> (α_range !!))
  -- | Draw new random values
  rs <- replicateM n_proposals sampleRandom
  let strace' = Map.union (Map.fromList (zip αs rs)) strace
  return (αs, strace')
