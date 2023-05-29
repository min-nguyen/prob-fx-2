



{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}

{- | Particle Marginal Metropolis-Hastings inference.
-}

module Inference.MC.PMMH where

import Comp
import Sampler
import LogP
import Trace (Trace, filterTrace)
import Effects.MulDist
import Dist
import Model
import Env
import Effects.EnvRW
import qualified Data.Map as Map
import Inference.MC.PIM as PIM
import Inference.MC.SIM as SIM
import qualified Inference.MC.SSMH as SSMH
import Inference.MC.MH as MH
import           Inference.MC.SIS as SIS
import           Inference.MC.SMC (handleResampleMul, advance)
import qualified Data.Vector as Vector

{- | Top-level wrapper for PMMH inference.
-}
pmmhWith :: forall env vars a. (env `ContainsVars` vars)
  => Int                                            -- ^ number of SSMH steps
  -> Int                                            -- ^ number of particles
  -> MulModel env [EnvRW env, MulDist, Sampler] a                  -- ^ model
  -> Env env                                        -- ^ input environment
  -> Vars vars                                      -- ^ parameter names
  -> Sampler [Env env]                              -- ^ output environments
pmmhWith mh_steps n_prts gen_model env_in obs_vars = do
  -- | Handle model to probabilistic program
  let model   = conditionWith env_in gen_model
  -- | Convert observable variables to strings
  let θ       = varsToStrs @env obs_vars
  -- | Initialise sample trace to include only parameters
  (_, τ_0)    <- (runImpure . reuseTrace Map.empty . defaultObserve) model
  map (snd . fst . fst) <$> pmmh mh_steps n_prts τ_0 θ model

pmmh :: Int -> Int -> Trace -> [Tag] -> Model '[Sampler] a -> Sampler [((a, LogP), Trace)]
pmmh m n τ θ = do
  let τθ = filterTrace θ τ
  runImpure . handleProposal . mh m τθ (PIM.exec n)

{- | An acceptance mechanism for PMMH.
-}
handleProposal :: Member Sampler fs => Handler (Propose LogP) fs a a
handleProposal = handle Val hop where
  hop :: Member Sampler es => Propose LogP x -> (x -> Comp es b) -> Comp es b
  hop (Propose τθ) k
    = do  α <- call $ randomFrom (Map.keys τθ)
          r <- call random
          let τθ' = Map.insert α r τθ
          k τθ'
  hop (Accept r@((_, w), _) r'@((_, w'), _)) k
    =  do let ratio = exp (w' - w)
          u <- call random
          k (if ratio > u then r' else r)
