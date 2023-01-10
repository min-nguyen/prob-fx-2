
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE PolyKinds #-}


{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{- | Inclusive KL Divergence Variational Inference.
-}

module Inference.VI.INVI where

import Data.Maybe
import Data.Proxy
import Data.Bifunctor ( Bifunctor(..) )
import Control.Monad ( replicateM, (>=>) )
import Effects.Dist
import Effects.Lift
import Effects.EnvRW ( EnvRW )
import Effects.State ( modify, handleState, State )
import Env ( Env, union )
import LogP ( LogP(..), normaliseLogPs )
import Model
import PrimDist
import Prog ( discharge, Prog(..), call, weaken, LastMember, Member (..), Members, weakenProg )
import Sampler
import           Trace (GradTrace, ParamTrace, Key(..), Some(..))
import qualified Trace
import Debug.Trace
import qualified Inference.MC.SIM as SIM
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util
import qualified Inference.VI.BBVI as BBVI
import qualified Inference.VI.VI as VI
import           Inference.VI.VI as VI (GradDescent(..))


{- | Top-level wrapper that takes a separate model and guide.
-}
invi :: forall env a b. (Show (Env env))
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> Model env [EnvRW env, Dist] b      -- ^ guide Q(X; λ)
  -> Model env [EnvRW env, Dist] a      -- ^ model P(X, Y)
  -> Env env                            -- ^ model environment (containing only observed data Y)
  -> Sampler ParamTrace           -- ^ final proposal distributions Q(λ_T)
invi num_timesteps num_samples guide_model model model_env  = do
  let guide :: Prog '[Param, Sample] (b, Env env)
      guide = ((second (Env.union model_env) <$>) . VI.installGuideParams . handleCore model_env) guide_model
  -- | Collect initial proposal distributions
  guideParams_0 <- VI.collectGuideParams guide
  -- | Run BBVI for T optimisation steps
  (handleLift . VI.handleNormGradDescent) $
    VI.viLoop num_timesteps num_samples guide VI.handleGuide model VI.handleModel guideParams_0