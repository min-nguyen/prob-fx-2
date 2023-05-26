



{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

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
  let θ        = varsToStrs @env obs_vars
  -- | Initialise sample trace to include only parameters
  (_, τ)       <- (runImpure . reuseTrace Map.empty . defaultObserve) model
  let τθ       = filterTrace θ τ
  pmmh_trace <- (runImpure . handleProposal . mh mh_steps τθ (PIM.exec n_prts)) model
  pure (map (snd . fst . fst) pmmh_trace)

pmmh :: Int -> Int -> Trace -> Model '[Sampler] a -> Sampler [((a, LogP), Trace)]
pmmh m n τθ  = runImpure . handleProposal . mh m τθ (PIM.exec n)

{- | An acceptance mechanism for PMMH.
-}
handleProposal :: Member Sampler fs => Handler (Propose LogP) fs a a
handleProposal (Val x)   = pure x
handleProposal (Op op k) = case discharge op of
  Right (Propose τθ)
    ->  do α <- randomFrom (Map.keys τθ)
           r <- random
           let τθ' = Map.insert α r τθ
           (handleProposal . k) τθ'
  Right (Accept x@((_, w), _) x'@((_, w'), _))
    -> do let ratio = exp (w' - w)
          u <- random
          (handleProposal . k) (if ratio > u then x' else x)
  Left op'
    -> Op op' (handleProposal . k)