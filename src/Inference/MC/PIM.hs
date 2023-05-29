



{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

{- | Particle Independence Metropolis.
-}

module Inference.MC.PIM where

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
import qualified Inference.MC.SMC as SMC
import qualified Inference.MC.IM as IM
import qualified Data.Vector as Vector

{- | Top-level wrapper for PIM inference.
-}
pimWith :: forall env vars a. (env `ContainsVars` vars)
  => Int                                            -- ^ number of SSMH steps
  -> Int                                            -- ^ number of particles
  -> MulModel env [EnvRW env, MulDist, Sampler] a                  -- ^ model
  -> Env env                                        -- ^ input environment
  -> Vars vars                                      -- ^ parameter names
  -> Sampler [Env env]                              -- ^ output environments
pimWith mh_steps n_prts gen_model env_in obs_vars = do
  -- | Handle model to probabilistic program
  let model   = conditionWith env_in gen_model
  -- | Convert observable variables to strings
  let θ       = varsToStrs @env obs_vars
  -- | Initialise sample trace to include only parameters
  (_, τ_0)    <- (runImpure .  reuseTrace Map.empty . defaultObserve) model
  map (snd . fst . fst) <$> pim mh_steps n_prts τ_0 θ model

pim :: Int -> Int -> Trace -> [Tag] -> Model '[Sampler] a -> Sampler [((a, LogP), Trace)]
pim mh_steps n_prts τ θ = do
  let τθ = filterTrace θ τ
  runImpure . IM.handleProposal . mh mh_steps τθ (exec n_prts)

{- | Handle probabilistic program using SSMH and compute the average log-probability using SMC.
-}
exec :: Int -> ModelExec '[Sampler] LogP a
exec n τθ prog   = do
  let step :: ModelStep '[Sampler] LogP a
      step (p, w) = (fmap fst .  runImpure .  reuseTrace τθ . advance w) p
  (xs, ws) <- (fmap unzip . runImpure . handleResampleMul . pfilter n 0 step) prog
  -- | Compute the normalised particle weights and their average weights
  let (ws_norm, ws_avg) = normaliseAndLogMean ws
  -- | Require at least some particles' weights to be greater than -inf
  if not (isInfinite ws_avg)
    then do
      idx <- Sampler.categorical (Vector.fromList $ map exp ws_norm)
      return ((xs !! idx, ws_avg), τθ)
    else
      return ((head xs, ws_avg), τθ)
