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

module Inference.BBVI
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
          - In the proper case that the guide never refers model variables that are to be conditioned against (indicated by the presence of observed values in the model environment), this will be trivially equivalent to using an empty model environment. In this case, the guide will initially only contain the correct set of @Sample@ operations prior to applying `installParam`.
          - In the inproper case that the guide refers to model variables to be conditioned against, then handling the guide under the model environment and then handling these variables as @Observe@ operations using SIM.handleObs will ignore any of their (importance weighting) side-effects; in constrast, handling the guide under the *empty environment* would incorrectly produce some @Sample@ operations that should not be present. Also note that the original observed values will later be reproduced in the guide's output environment.
      2) Replacing any differentiable @Sample@ operations with @Param@.
-}
  let guide :: Prog '[Param, Sample] (b, Env env)
      guide = ((second (Env.union model_env) <$>) . installGuideParams . handleCore model_env) guide_model
  -- | Collect initial proposal distributions
  guideParams_0 <- collectGuideParams guide
  -- | Run BBVI for T optimisation steps
  ((fst <$>) . handleLift . handleGradDescent)
    $ VI.viLoop num_timesteps num_samples guide handleGuide model handleModel (guideParams_0, Trace.empty)

-- | Ignore the side-effects of all @Observe@ operations, and replace all differentiable @Sample@ operations (representing the proposal distributions Q(λ)) with @Param@ operations.
installGuideParams :: Prog [Observe, Sample] a -> Prog [Param, Sample] a
installGuideParams = loop . SIM.handleObs where
  loop :: Prog '[Sample] a -> Prog [Param, Sample] a
  loop (Val x)   = pure x
  loop (Op op k) = case prj op of
    Just (Sample q α) -> case isDifferentiable q of
        Nothing      -> Op (weaken op) (loop  . k)
        Just Witness -> do x <- call (ParamS q α)
                           (loop  . k) x
    Nothing -> Op (weaken op) (loop . k)

-- | Collect the parameters λ_0 of the guide's initial proposal distributions.
collectGuideParams :: Prog [Param, Sample] a -> Sampler DTrace
collectGuideParams = SIM.handleSamp . (fst <$>) . handleGuideParams . loop Trace.empty
  where
  loop :: DTrace -> Prog (Param : es) a -> Prog (Param : es) DTrace
  loop params (Val _)   = pure params
  loop params (Op op k) = case prj op of
    Just (ParamS q α)   -> do let params' = Trace.insert (Key α) q params
                              Op op (loop params' . k)
    Just (ParamO q x α) -> error "VI.collectGuideParams: Should not happen unless using collectProposals"
    Nothing -> Op op (loop params . k)

{- | Execute the guide Q under a provided set of proposal distributions Q(λ), producing:
      1. The output environment of latent variables X=x (given `env` contains X) generated by @Sample@s
         i.e. fixed non-differentiable dists, and @Param@s, i.e. learnable proposal dists:
            x ~ Q(X; λ),          where Q can be fixed or learnable
      2. The total log-weight of latent variables X=x, resulting from @Sample@s and @Param@s:
            log(Q(X=x; λ)),      where Q can be fixed or learnable
      3. The gradients of all proposal distributions Q(λ) at X=x:
            δlog(Q(X=x; λ)),     where Q is learnable
 -}
handleGuide :: Prog [Param, Sample] a -> DTrace -> Sampler ((a, LogP), GTrace)
handleGuide guide params =
  (SIM.handleSamp . handleGuideParams . weighGuide . updateGuideParams params) guide

-- | Compute log(Q(X; λ)) over the guide.
weighGuide :: forall es a. (Members [Param, Sample] es) => Prog es a -> Prog es (a, LogP)
weighGuide = loop 0 where
  loop :: LogP -> Prog es a -> Prog es (a, LogP)
  loop logW (Val a)   = pure (a, logW)
  loop logW (Op op k) = case  op of
      -- | Compute: log(Q(X; λ)) for proposal distributions
      ParamSPrj q α   -> Op op (\x -> loop (logW + logProb q x) $ k x)
      -- | Compute: log(Q(X; λ)) for non-differentiable distributions
      SampPrj q α     -> Op op (\x -> loop (logW + logProb q x) $ k x)
      _               -> Op op (loop logW . k)

-- | Set the @Param@eters of the guide Q(X; λ).
updateGuideParams :: forall es a. Member Param es => DTrace -> Prog es a -> Prog es a
updateGuideParams proposals = loop where
  loop :: Prog es a -> Prog es a
  loop (Val a)   = pure a
  loop (Op op k) = case prj op of
    Just (ParamS q α) -> do
      let q' = fromMaybe q (Trace.lookup (Key α) proposals)
      x <- call (ParamS q' α)
      (loop . k) x
    Just (ParamO q x α) -> error "VI.updateGuideParams: Should not happen"
    Nothing -> Op op (loop . k)

-- | Sample from each @Param@ distribution, x ~ Q(X; λ), and record its grad-log-pdf, δlog(Q(X = x; λ)).
handleGuideParams :: forall es a. Member Sample es => Prog (Param : es) a -> Prog es (a, GTrace)
handleGuideParams = loop Trace.empty where
  loop :: GTrace -> Prog (Param : es) a -> Prog es (a, GTrace)
  loop grads (Val a)   = pure (a, grads)
  loop grads (Op op k) = case discharge op of
    Right (ParamS (q :: d) α) -> do
      x <- call (Sample q α)
      let grads' = Trace.insert @d (Key α) (gradLogProb q x) grads
      (loop grads' . k) x
    Right (ParamO (q :: d) x α) ->
      trace "VI.handleGuideParams: Should not happen unless using collectProposals"
      loop grads (k x)
    Left op' -> Op op' (loop grads . k)

{- | Execute the model P under an environment of samples X=x from the guide and observed values Y=y, producing:
       1. An output environment which we discard
       2. The total log-weight of all @Sample@ and @Observe@ operations: log(P(X=x, Y=y))
-}
handleModel :: Model env [ObsRW env, Dist] a -> DTrace -> Env env -> Sampler (((a, Env env), LogP), GTrace)
handleModel model _ env  =
  (((, Trace.empty) <$>) . SIM.handleSamp . SIM.handleObs . weighModel . handleCore env) model

-- | Compute log(P(X, Y)) over the model.
weighModel :: forall es a. (Members [Sample, Observe] es) => Prog es a -> Prog es (a, LogP)
weighModel = loop 0 where
  loop :: LogP -> Prog es a -> Prog es (a, LogP)
  loop logW (Val a)   = pure (a, logW)
  loop logW (Op op k) = case op of
      -- | Compute: log(P(Y))
      ObsPrj d y α -> Op op (\x -> loop (logW + logProb d x) $ k x)
      -- | Compute: log(P(X))
      SampPrj d α  -> Op op (\x -> loop (logW + logProb d x) $ k x)
      _            -> Op op (loop logW . k)

-- | Update the guide parameters using a likelihood-ratio-estimate of the ELBO gradient
handleGradDescent :: Prog (GradDescent : fs) a -> Prog fs a
handleGradDescent (Val a) = pure a
handleGradDescent (Op op k) = case discharge op of
  Right (GradDescent logWs δGs params) ->
    let δelbos  = likelihoodRatioEstimator logWs δGs
        params' = VI.gradStep 1 params δelbos
    in  handleGradDescent (k params')
  Left op' -> Op op' (handleGradDescent . k)

-- | Compute a likelihood-ratio-estimate E[δelbo] of the ELBO gradient
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