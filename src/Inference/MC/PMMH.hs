



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
import Inference.MC.SIM as SIM
import qualified Inference.MC.SSMH as SSMH
import Inference.MC.MH as MH
import           Inference.MC.SIS as SIS
import           Inference.MC.SMC (handleResampleMul, advance)

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
  pmmh_trace <- (runImpure . handleProposal . mh mh_steps τθ (exec n_prts)) model
  pure (map (snd . fst . fst) pmmh_trace)

pmmh :: Int -> Int -> Trace -> Model '[Sampler] a -> Sampler [((a, LogP), Trace)]
pmmh m n τθ  = runImpure . handleProposal . mh m τθ (exec n)

{- | Handle probabilistic program using SSMH and compute the average log-probability using SMC.
-}
exec :: Int -> ModelExec '[Sampler] LogP a
exec n τθ prog   = do
  let execPrt :: ModelStep '[Sampler] LogP a
      execPrt (p, w) = (fmap fst . runImpure . reuseTrace τθ . advance w) p
  (as, ws) <- (runImpure . handleResampleMul . fmap unzip . pfilter n 0 execPrt ) prog
  let a   = head as
      w   = logMeanExp ws
  pure ((a, w), τθ)

{- | An acceptance mechanism for PMMH.
-}
handleProposal :: Member Sampler fs => Handler (Proposal LogP) fs a a
handleProposal (Val x)   = pure x
handleProposal (Op op k) = case discharge op of
  Right (Propose τθ)
    ->  do α <- randomFrom (Map.keys τθ)
           r <- random
           let τθ' = Map.insert α r τθ
           (handleProposal . k) τθ'
  Right (Accept w w')
    ->  do u <- random
           (handleProposal . k) (exp (w' - w) > u)
  Left op'
    -> Op op' (handleProposal . k)