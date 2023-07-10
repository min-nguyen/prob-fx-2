{-# LANGUAGE RankNTypes #-}


{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}


{- Sequential Monte Carlo^2 inference.
-}

module Inference.MC.SMC2 where

import qualified Data.Map as Map
import Data.Map (Map)
import Env
import Comp
import Model
import Sampler
import Effects.MulDist
import Effects.EnvRW
import Effects.NonDet
import qualified Inference.MC.SSMH as SSMH
import qualified Inference.MC.PMH as PMH
import           Inference.MC.RMPF as RMPF (PrtState(..), exec, suspendAfter)
import qualified Inference.MC.SMC as SMC
import qualified Inference.MC.SIM as SIM
import qualified Inference.MC.SIS as SIS hiding  (particleLogProb)
import Inference.MC.SIS (Resample(..), ModelStep)
import Inference.MC.MH as MH
import Data.Bifunctor
import Trace (filterTrace)
import LogP
import Unsafe.Coerce (unsafeCoerce)
import Inference.MC.SIM (defaultSample)
import qualified Data.Vector as Vector
import Control.Monad (replicateM)

{- | Top-level wrapper for SMC2 inference.
-}
smc2 :: forall env es a xs. (env `ContainsVars` xs)
  => Int                                            -- ^ number of outer SMC particles
  -> Int                                            -- ^ number of PMH steps
  -> Int                                            -- ^ number of inner SMC particles
  -> MulModel env [EnvRW env, MulDist, Sampler] a                  -- ^ model
  -> Env env                                        -- ^ input environment
  -> Vars xs                                        -- ^ optional observable variable names of interest
  -> Sampler [Env env]                              -- ^ output environments
smc2 n_outer_prts mh_steps n_inner_prts gen_model env obs_vars = do
  -- | Handle model to probabilistic program
  let model = (handleMulDist . handleEnvRW env) (runModel gen_model)
  -- | Convert observable variables to strings
      tags = varsToStrs @env obs_vars
  -- | Run SMC2do
  smc2_trace <- runImpure (smc2Internal n_outer_prts mh_steps n_inner_prts tags  model)
  -- Return the accepted model environments
  pure (map (snd . fst) smc2_trace)

{- | Perform SMC2 on a probabilistic program.
-}
smc2Internal :: (Member Sampler fs)
  => Int                                          -- ^ number of outer SMC particles
  -> Int                                          -- ^ number of PMH steps
  -> Int                                          -- ^ number of inner SMC particles
  -> [Tag]                                        -- ^ tags indicating variables of interest
  -> Model '[Sampler] a                                    -- ^ probabilistic program
  -> Comp fs [(a, PrtState)]                -- ^ final particle results and contexts
smc2Internal n_outer_prts mh_steps n_inner_prts tags  m  =
  (handleResample mh_steps n_inner_prts tags  m . SIS.pfilter n_outer_prts (0, Map.empty) RMPF.exec ) m

{- | A handler for resampling particles according to their normalized log-likelihoods,
     and then pertrubing their sample traces using PMH.
-}
handleResample :: Member Sampler fs
  => Int                                           -- ^ number of PMH steps
  -> Int                                           -- ^ number of inner SMC particles
  -> [Tag]                                      -- ^ tags indicating variables of interest
  -> Model '[Sampler] a
  -> Comp (Resample PrtState : fs) [(a, PrtState)]
  -> Comp fs [(a, PrtState)]
handleResample mh_steps n_inner_prts θ model = loop (0 :: Int) where
  loop t (Val x) = Val x
  loop t (Op op k) = case discharge op of
    Right  (Resample pwτs) ->
      do  let (ws, τs) = (unzip . map snd) pwτs
          -- | Compute the normalised particle weights and their average weights
          let (ws_norm, ws_avg) = normaliseAndLogMean ws
          if  -- | Require at least some particles' weights to be greater than -inf
              not (isInfinite ws_avg)
          then do
            idxs <- call $ (replicateM (length ws) . Sampler.categorical) (Vector.fromList (map exp ws_norm))
            -- | Resample the particle traces.
            let τs_res  = map (τs !!) idxs
            -- | Insert break point to perform SSMH up to
                model_t = suspendAfter t model
            -- | Perform PMH using each resampled particle's sample trace and get the most recent PMH iteration (where the traces will contain only values specified in θ).
            pwτθs_mov <- mapM (\τ -> do ((p_mov, _), τθ_mov) : _ <- call (PMH.pmh mh_steps n_inner_prts τ θ (unsafeCoerce model_t))
                                        return (p_mov, (ws_avg, τθ_mov))
                              ) τs_res
            (loop (t + 1) . k) pwτθs_mov
          else
            -- | Retain particles.
            (loop (t + 1) . k) pwτs
    Left op' -> Op op' (loop t . k)