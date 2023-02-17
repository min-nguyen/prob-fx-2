
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

{- | Maximum likelihood estimation in terms of VI that samples from the prior P(X) and assigns each simulation an
     importance weight P(Y | X; θ).
-}

module Inference.VI.MAP (module Inference.VI.MAP) where

import Data.Bifunctor ( Bifunctor(..) )
import Control.Monad ( replicateM, (>=>) )
import Effects.Dist
import Effects.EnvRW ( EnvRW, handleEnvRW )
import Effects.State ( modify, handleState, State )
import Env ( Env, Vars, ContainsVars, union, empty, varsToStrs )
import LogP ( LogP(..), normaliseLogPs )
import Model
import PrimDist
import Comp ( discharge, Comp(..), call, weaken, LastMember, Member (..), Members, weakenProg )
import Sampler
import           Trace (GradTrace, DistTrace, Key(..), Some(..))
import qualified Trace
import Debug.Trace
import Inference.MC.SIM
import Inference.VI.VI as VI
import Inference.VI.MLE (handleNormGradDescent)
import Inference.MC.LW (joint)
import qualified Inference.VI.BBVI as BBVI

map :: forall env es a b. es ~ '[Sampler]
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> VIGuide env es a                      -- ^ guide Q(X; λ)
  -> VIModel env es b                      -- ^ model P(X, Y)
  -> Env env                            -- ^ model environment (containing only observed data Y)
  -> Sampler DistTrace                     -- ^ final parameters θ_T
map num_timesteps num_samples guide model env  = do
  -- | Set up a empty dummy guide Q to return the original input model environment
  λ_0 <- collectParams env guide
  -- | Run MAP for T optimisation steps
  (handleIO . handleNormGradDescent) $
      VI.guidedLoop num_timesteps num_samples guide (execGuide env) model exec λ_0

-- | Return probability of 1
execGuide :: Env env -> DistTrace -> VIGuide env '[Sampler] a -> Sampler (((a, Env env), GradTrace), LogP)
execGuide env params  = (second (const 0) <$>) . BBVI.execGuide  env  params
  -- (handleIO . fmap (,0) . defaultSample . defaultParam params . handleEnvRW env) guide

-- | Compute P(Y, X)
exec :: Env env -> VIModel env '[Sampler] a -> Sampler (a, LogP)
exec  env =
  handleIO . defaultSample . defaultObserve . joint 0 . fmap fst . handleEnvRW env
