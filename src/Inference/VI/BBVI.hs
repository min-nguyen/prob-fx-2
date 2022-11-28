{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

{- | BBVI inference on a model and guide as separate programs.
-}

module Inference.VI.BBVI
  where

import           Data.Maybe
import           Data.Bifunctor ( Bifunctor(..) )
import           Debug.Trace
import           Control.Monad ( replicateM, (>=>) )
import           Effects.Dist
import           Effects.Lift
import           Effects.ObsRW ( ObsRW )
import           Effects.State ( modify, handleState, State )
import           Env ( Env, union )
import           LogP ( LogP(..) )
import           Model
import           PrimDist
import           Prog ( discharge, Prog(..), call, weaken, LastMember, Member (..), Members, weakenProg )
import           Sampler ( Sampler )
import           Trace (GTrace, DTrace, Key(..), Some(..))
import qualified Trace
import qualified Inference.MC.SIM as SIM
import qualified Inference.VI.VI as VI
import           Inference.VI.VI as VI (GradDescent(..))
import qualified Vec
import           Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))

{- | Top-level wrapper for BBVI inference that takes a separate model and guide.
-}
bbvi :: forall env a b. (Show (Env env))
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> Model env [ObsRW env, Dist] b      -- ^ guide Q(X; λ)
  -> Model env [ObsRW env, Dist] a      -- ^ model P(X, Y)
  -> Env env                            -- ^ model environment (containing only observed data Y)
  -> Sampler DTrace                     -- ^ final guide parameters λ_T
bbvi num_timesteps num_samples guide_model model model_env  = do
{- | Prepare guide by:
      1) Handling the guide-model under the model environment.
          - In the proper case that the guide never refers model variables that are to be conditioned against (indicated by the presence of observed values in the model environment), this will be trivially equivalent to using an empty model environment. In this case, the guide will initially only contain the correct set of @Sample@ operations prior to applying `installParam`.
          - In the inproper case that the guide refers to model variables to be conditioned against, then handling the guide under the model environment and then handling these variables as @Observe@ operations using SIM.handleObs will ignore any of their (importance weighting) side-effects; in constrast, handling the guide under the *empty environment* would incorrectly produce some @Sample@ operations that should not be present. Also note that the original observed values will later be reproduced in the guide's output environment.
      2) Replacing any differentiable @Sample@ operations with @Param@.
-}
  let guide :: Prog '[Param, Sample] (b, Env env)
      guide = ((second (Env.union model_env) <$>) . VI.installGuideParams . handleCore model_env) guide_model
  -- | Collect initial proposal distributions
  guideParams_0 <- VI.collectGuideParams guide
  -- | Run BBVI for T optimisation steps
  ((fst <$>) . handleLift . VI.handleLRatioGradDescent)
    $ VI.viLoop num_timesteps num_samples guide VI.handleGuide model VI.handleModel (guideParams_0, Trace.empty)

