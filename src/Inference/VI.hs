{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}
{-# LANGUAGE TupleSections #-}

{- | BBVI inference on a model and guide as separate programs.
-}

module Inference.VI
  where

import Data.Maybe
import Data.Proxy
import Data.Bifunctor ( Bifunctor(..) )
import Control.Monad ( replicateM, (>=>) )
import Effects.Dist
import Effects.Lift
import Effects.ObsRW ( ObsRW )
import Effects.State ( modify, handleState, State )
import Env ( Env, union )
import LogP ( LogP(..), normaliseLogPs, expLogP )
import Model
import PrimDist
import Prog ( discharge, Prog(..), call, weaken, LastMember, Member (..), Members, weakenProg )
import Sampler
import           Trace (GTrace, DTrace, Key(..), Some(..))
import qualified Trace
import Debug.Trace
import qualified Inference.SIM as SIM
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util

data GradDescent a where
  GradDescent :: [LogP] -> [GTrace] -> DTrace -> GradDescent DTrace

viLoop :: (LastMember (Lift Sampler) fs, Show (Env env))
  => Int                                          -- ^ number of optimisation steps (T)
  -> Int                                          -- ^ number of samples to estimate the gradient over (L)
  -> Prog [Param, Sample] (b, Env env)            -- ^ guide Q(X; λ)
  -> (forall b. Prog [Param, Sample] b -> DTrace -> Sampler ((b, LogP), GTrace))
  -> Model env [ObsRW env, Dist] a                -- ^ model P(X, Y)
  -> (forall a. Model env [ObsRW env, Dist] a -> DTrace -> Env env -> Sampler (((a, Env env), LogP), GTrace))
  -> Env env                                      -- ^ model environment (containing only observed data Y)
  -> (DTrace, DTrace)                             -- ^ guide parameters λ_t, model parameters θ_t
  -> Prog (GradDescent : fs) (DTrace, DTrace)      -- ^ final guide parameters λ_T
viLoop num_timesteps num_samples guide hdlGuide model hdlModel model_env (guideParams_0, modelParams_0) = do
  foldr (>=>) pure [viStep t num_samples guide' hdlGuide model hdlModel  | t <- [1 .. num_timesteps]]
    (guideParams_0, modelParams_0)
  where guide' = second (Env.union model_env) <$> guide

{- | 1. For L iterations,
        a) Generate values x from the guide Q(X; λ), accumulating:
            - the total guide log-weight:  log(Q(X = x; λ))
            - the gradient log-pdfs of the guide: δlog(Q(X = x; λ))
        b) Execute the model P using values x ~ Q(X; λ) and y, accumulating:
            - the total model log-weight:  log(P(X=x, Y=y))
     2. Compute an estimate of the ELBO gradient: E[δelbo]
     3. Update the parameters λ of the guide
-}
viStep :: (LastMember (Lift Sampler) fs, Show (Env env))
  => Int                                          -- ^ time step index (t)
  -> Int                                          -- ^ number of samples to estimate the gradient over (L)
  -> Prog [Param, Sample] (b, Env env)            -- ^ guide Q(X; λ)
  -> (forall b. Prog [Param, Sample] b        -> DTrace -> Sampler ((b, LogP), GTrace))
  -> Model env [ObsRW env, Dist] a                -- ^ model P(X, Y)
  -> (forall a. Model env [ObsRW env, Dist] a -> DTrace -> Env env -> Sampler (((a, Env env), LogP), GTrace))
  -> (DTrace, DTrace)                             -- ^ guide parameters λ_t, model parameters θ_t
  -> Prog (GradDescent : fs) (DTrace, DTrace)    -- ^ next guide parameters λ_{t+1}
viStep timestep num_samples guide hdlGuide model hdlModel (guideParams, modelParams) = do
  -- | Execute the guide X ~ Q(X; λ) for (L) iterations
  (((_, guide_envs), guide_logWs), guide_grads)
      <- Util.unzip4 <$> replicateM num_samples (lift (hdlGuide guide guideParams))
  -- | Execute the model P(X, Y) under the union of the model environment Y and guide environment X
  ((_, model_logWs), model_grads)
      <- Util.unzip3 <$> mapM (lift . hdlModel model modelParams) guide_envs
  -- | Compute total log-importance-weight, log(P(X, Y)) - log(Q(X; λ))
  let logWs  = zipWith (-) model_logWs guide_logWs
  -- | Update the parameters λ of the proposal distributions Q
  guideParams'    <- call (GradDescent logWs guide_grads guideParams)
  -- modelParams'    <- call (GradDescent logWs model_grads modelParams)

  pure (guideParams', modelParams)

{- | Update each variable v's parameters λ using their estimated ELBO gradients E[δelbo(v)].
        λ_{t+1} = λ_t + η_t * E[δelbo(v)]
     where the gradient δelbo(v) is implicitly w.r.t λ
-}
gradStep
  :: Double  -- ^ learning rate             η
  -> DTrace  -- ^ optimisable distributions Q(λ_t)
  -> GTrace  -- ^ elbo gradient estimates   E[δelbo]
  -> DTrace  -- ^ updated distributions     Q(λ_{t+1})
gradStep η = Trace.intersectLeftWith (\q δλ ->  q `safeAddGrad` (η *| δλ))
