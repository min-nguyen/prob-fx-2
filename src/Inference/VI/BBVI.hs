
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE PolyKinds #-}


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
import           Effects.EnvRW ( EnvRW, handleEnvRW )
import           Effects.State ( modify, handleState, State )
import           Env ( Env, union )
import           LogP ( LogP(..) )
import           Model
import           PrimDist
import           Prog ( discharge, Prog(..), call, weaken, LastMember, Member (..), Members, weakenProg )
import           Sampler ( Sampler, liftIO )
import           Trace (GradTrace, ParamTrace, Key(..), Some(..))
import qualified Trace
import           Inference.MC.SIM as SIM
import           Inference.VI.VI as VI
-- import           Inference.VI.VI as VI (GradDescent(..))
import qualified Vec
import           Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Inference.MC.LW

{- | Top-level wrapper for BBVI inference that takes a separate model and guide.
-}
bbvi :: forall env a b.
    Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> VIGuide env b      -- ^ guide Q(X; λ)
  -> VIModel env a      -- ^ model P(X, Y)
  -> Env env                            -- ^ empty environment
  -> Sampler ParamTrace                 -- ^ final guide parameters λ_T
bbvi num_timesteps num_samples guide model env  = do
  λ_0 <- VI.collectParams env guide
  liftIO (print λ_0)
  (handleM . VI.handleLRatioGradDescent)
    $ VI.viLoop num_timesteps num_samples guide (handleGuide env) model handleModel λ_0

handleGuide :: Env env -> VIGuide env a -> ParamTrace -> Sampler (((a, Env env), LogP), GradTrace)
handleGuide env guide params =
  (handleM . SIM.defaultSample . handleParams . weighGuide . updateParams params . handleEnvRW env) guide

handleModel :: VIModel env a -> Env env -> Sampler (a, LogP)
handleModel model env  =
  (handleM . defaultSample . defaultObserve . joint 0 . fmap fst . handleEnvRW env) model
