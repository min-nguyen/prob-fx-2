



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
import Effects.Dist
import PrimDist
import Model
import Env
import Effects.EnvRW
import qualified Data.Map as Map
import Inference.MC.SIM as SIM
import qualified Inference.MC.SSMH as SSMH
import Inference.MC.MH as MH
import           Inference.MC.SIS as SIS
import           Inference.MC.SMC (handleResampleMul, step)

{- | Top-level wrapper for PMMH inference.
-}
pmmh :: forall env vars a. (env `ContainsVars` vars)
  => Int                                            -- ^ number of SSMH steps
  -> Int                                            -- ^ number of particles
  -> GenModel env [EnvRW env, Dist, Sampler] a                  -- ^ model
  -> Env env                                        -- ^ input environment
  -> Vars vars                                      -- ^ parameter names
  -> Sampler [Env env]                              -- ^ output environments
pmmh mh_steps n_prts model env_in obs_vars = do
  -- | Handle model to probabilistic program
  let prog_0   = handleCore env_in model
  -- | Convert observable variables to strings
  let θ        = varsToStrs @env obs_vars
  -- | Initialise sample trace to include only parameters
  (_, τ)       <- (handleIO . reuseTrace Map.empty . defaultObserve) prog_0
  let τθ       = filterTrace θ τ
  pmmh_trace <- (handleIO . handleProposal . mh mh_steps τθ (exec n_prts)) prog_0
  pure (map (snd . fst . fst) pmmh_trace)

pmmh' :: Int -> Int -> Trace -> Model '[Sampler] a -> Sampler [((a, LogP), Trace)]
pmmh' m n τθ model = do
  (handleIO . handleProposal . mh m τθ (exec n)) model

{- | Handle probabilistic program using SSMH and compute the average log-probability using SMC.
-}
exec :: Int -> ModelHandler '[Sampler] LogP
exec n τθ prog   = do
  let execPrt :: ParticleHandler '[Sampler] LogP
      execPrt logp = fmap fst . handleIO . reuseTrace τθ . step logp
  (as, ws) <- (handleIO . handleResampleMul . fmap unzip . pfilter execPrt) (replicate n (prog, 0))
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
  Right (Accept lw lw')
    ->  do u <- random
           (handleProposal . k) (exp (lw' - lw) > u)
  Left op'
    -> Op op' (handleProposal . k)