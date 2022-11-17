{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{- | BBVI inference on a model and guide as separate programs.
-}

module Inference.BBVI
  where

import Data.Maybe
import Data.Proxy
import Data.Bifunctor ( Bifunctor(first) )
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
import qualified Inference.VI as VI
import           Inference.VI as VI (GradDescent(..))
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util

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
          - In the proper case that the guide never refers model variables that are to be conditioned against (indicated by the presence of observed values in the model environment), this will be trivially equivalent to using an empty model environment. In this case, the guide will initially only contain the correct set of @Sample@ operations prior to applying `installLearn`.
          - In the inproper case that the guide refers to model variables to be conditioned against, then handling the guide under the model environment and then handling these variables as @Observe@ operations using SIM.handleObs will ignore any of their (importance weighting) side-effects; in constrast, handling the guide under the *empty environment* would incorrectly produce some @Sample@ operations that should not be present. Also note that the original observed values will later be reproduced in the guide's output environment.
      2) Replacing any differentiable @Sample@ operations with @Learn@.
-}
  let guide :: Prog '[Learn, Sample] (b, Env env)
      guide = (VI.installLearn . SIM.handleObs . handleCore model_env) guide_model
  -- | Collect initial proposal distributions
  params_0 <- SIM.handleSamp (VI.collectParams guide)
  -- | Run BBVI for T optimisation steps
  (handleLift . handleGradDescent) $ VI.viLoop num_timesteps num_samples guide model model_env params_0

{- | Compute the ELBO gradient estimates for each variable v over L samples.
        E[δelbo(v)] = sum (F_v^{1:L} - b_v * G_v^{1:L}) / L
-}
handleGradDescent :: Prog (GradDescent : fs) a -> Prog fs a
handleGradDescent (Val a) = pure a
handleGradDescent (Op op k) = case discharge op of
  Right (GradDescent logWs δGs params) ->
    let δelbos  = likelihoodRatioEstimator logWs δGs
        params' = VI.updateParams 1 params δelbos
    in  handleGradDescent (k params')
  Left op' -> Op op' (handleGradDescent . k)

likelihoodRatioEstimator :: [LogP] -> [GTrace] -> GTrace
likelihoodRatioEstimator logWs δGs = foldr (\(Some v) -> Trace.insert v (estδELBO v)) Trace.empty vars
  where
    norm_c :: Double
    norm_c = 1/fromIntegral (length logWs)

    vars :: [Some DiffDistribution Key]
    vars = (Trace.keys . head) δGs
    {- | Uniformly scale each iteration's gradient trace G^l by its corresponding (normalised) importance weight W_norm^l.
            F^{1:L} = W_norm^{1:L} * G^{1:L}
        where the normalised importance weight is defined via:
            log(W_norm^l) = log(W^l) + max(log(W^{1:L})) -}
    δFs :: [GTrace]
    δFs = zipWith (\logW -> Trace.map (\_ δ -> expLogP logW *| δ)) (normaliseLogPs logWs) δGs
    {- | Compute the ELBO gradient estimate for a random variable v's associated parameters:
            E[δelbo(v)] = sum (F_v^{1:L} - b_v * G_v^{1:L}) / L
        where the baseline is:
            b_v    = covar(F_v^{1:L}, G_v^{1:L}) / var(G_v^{1:L}) -}
    estδELBO :: forall d. (DiffDistribution d)
      => Key d                -- ^   v
      -> Vec (Arity d) Double -- ^   E[δelbo(v)]
    estδELBO v  =
      let δGv        = map (fromJust . Trace.lookup v) δGs      -- G_v^{1:L}
          δFv        = map (fromJust . Trace.lookup v) δFs      -- F_v^{1:L}
          baseline_v = Vec.covar δFv δGv |/| Vec.var δGv  -- b_v
          δELBOv     = zipWith (\δgv δfv -> δfv |-| (baseline_v |*| δgv)) δGv δFv
          δestELBOv  = ((*|) norm_c . foldr (|+|) (Vec.zeros (Proxy @(Arity d)))) δELBOv
      in  δestELBOv