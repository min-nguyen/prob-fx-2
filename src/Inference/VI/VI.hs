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

module Inference.VI.VI
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
import LogP ( LogP(..), normaliseLogPs )
import Model
import PrimDist
import Prog ( discharge, Prog(..), call, weaken, LastMember, Member (..), Members, weakenProg )
import Sampler
import           Trace (GTrace, DTrace, Key(..), Some(..))
import qualified Trace
import Debug.Trace
import qualified Inference.MC.SIM as SIM
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util

data GradDescent a where
  GradDescent :: [LogP] -> [GTrace] -> DTrace -> GradDescent DTrace

viLoop :: (HasSampler fs, Show (Env env))
  => Int                                          -- ^ number of optimisation steps (T)
  -> Int                                          -- ^ number of samples to estimate the gradient over (L)
  -> Prog [Param, Sample] (a, Env env)            -- ^ guide Q(X; λ)
  -> (forall c. Prog [Param, Sample] c -> DTrace -> Sampler ((c, LogP), GTrace))
  -> Model env [ObsRW env, Dist] b                -- ^ model P(X, Y)
  -> (forall d env. Model env [ObsRW env, Dist] d -> DTrace -> Env env -> Sampler (((d, Env env), LogP), GTrace))
  -> (DTrace, DTrace)                             -- ^ guide parameters λ_t, model parameters θ_t
  -> Prog (GradDescent : fs) (DTrace, DTrace)      -- ^ final guide parameters λ_T
viLoop num_timesteps num_samples guide hdlGuide model hdlModel  (guideParams_0, modelParams_0) = do
  foldr (>=>) pure [viStep t num_samples guide hdlGuide model hdlModel  | t <- [1 .. num_timesteps]]
    (guideParams_0, modelParams_0)

{- | 1. For L iterations,
        a) Generate values x from the guide Q(X; λ), accumulating:
            - the total guide log-weight:  log(Q(X = x; λ))
            - the gradient log-pdfs of the guide: δlog(Q(X = x; λ))
        b) Execute the model P using values x ~ Q(X; λ) and y, accumulating:
            - the total model log-weight:  log(P(X=x, Y=y))
     2. Compute an estimate of the ELBO gradient: E[δelbo]
     3. Update the parameters λ of the guide
-}
viStep :: (HasSampler fs, Show (Env env))
  => Int                                          -- ^ time step index (t)
  -> Int                                          -- ^ number of samples to estimate the gradient over (L)
  -> Prog [Param, Sample] (a, Env env)            -- ^ guide Q(X; λ)
  -> (forall c. Prog [Param, Sample] c -> DTrace -> Sampler ((c, LogP), GTrace))
  -> Model env [ObsRW env, Dist] b                -- ^ model P(X, Y)
  -> (forall d env. Model env [ObsRW env, Dist] d -> DTrace -> Env env -> Sampler (((d, Env env), LogP), GTrace))
  -> (DTrace, DTrace)                             -- ^ guide parameters λ_t, model parameters θ_t
  -> Prog (GradDescent : fs) (DTrace, DTrace)    -- ^ next guide parameters λ_{t+1}
viStep timestep num_samples guide hdlGuide model hdlModel (guideParams, modelParams) = do
  -- | Execute the guide X ~ Q(X; λ) for (L) iterations
  (((_, guide_envs), guide_logWs), guide_grads)
      <- Util.unzip4 <$> replicateM num_samples (lift (hdlGuide guide guideParams))
  -- | Execute the model P(X, Y) under the union of the model environment Y and guide environment X
  ((_              , model_logWs), model_grads)
      <- Util.unzip3 <$> mapM (lift . hdlModel model modelParams) guide_envs
  -- | Compute total log-importance-weight, log(P(X, Y)) - log(Q(X; λ))
  let logWs  = zipWith (-) model_logWs guide_logWs
  -- | Update the parameters λ of the proposal distributions Q
  guideParams'    <- call (GradDescent logWs guide_grads guideParams)
  modelParams'    <- call (GradDescent logWs model_grads modelParams)
  -- liftPutStrLn (show modelParams')
  pure (guideParams', modelParams')

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

-- | Compute and update the guide parameters using a likelihood-ratio-estimate E[δelbo] of the ELBO gradient
handleLRatioGradDescent :: Prog (GradDescent : fs) a -> Prog fs a
handleLRatioGradDescent (Val a) = pure a
handleLRatioGradDescent (Op op k) = case discharge op of
  Right (GradDescent logWs δGs params) ->
    let δelbos  = likelihoodRatioEstimator logWs δGs
        params' = gradStep 1 params δelbos
    in  handleLRatioGradDescent (k params')
  Left op' -> Op op' (handleLRatioGradDescent . k)

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
    δFs = zipWith (\logW -> Trace.map (\_ δ -> exp logW *| δ)) (normaliseLogPs logWs) δGs
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

-- | Compute and update the guide parameters using a self-normalised importance weighted gradient estimate
handleNormGradDescent :: Prog (GradDescent : fs) a -> Prog fs a
handleNormGradDescent (Val a) = pure a
handleNormGradDescent (Op op k) = case discharge op of
  Right (GradDescent logWs δGs params) ->
    let δelbos  = normalisingEstimator logWs δGs
        params' = case δelbos of Just δelbos' -> gradStep 1 params δelbos'
                                 Nothing      -> params
    in  handleNormGradDescent (k params')
  Left op' -> Op op' (handleNormGradDescent . k)

normalisingEstimator :: [LogP] -> [GTrace] -> Maybe GTrace
normalisingEstimator logWs δGs = δelbos
  where
    {- | Store the gradient estimates for each variable v. -}
    δelbos :: Maybe GTrace
    δelbos = if isInfinite norm_c
              then Nothing
              else Just (foldr (\(Some v) -> estimateGrad v) Trace.empty vars)
    {- | Normalising constant -}
    norm_c :: Double
    norm_c = 1 / (fromIntegral (length logWs) * sum (map exp logWs))
    {- | Optimisable variables -}
    vars :: [Some DiffDistribution Key]
    vars = (Trace.keys . head) δGs
    {- | Uniformly scale each iteration's gradient trace G^l by its corresponding unnormalised importance weight W_norm^l -}
    δFs :: [GTrace]
    δFs = zipWith (\logW -> Trace.map (\_ δ -> exp logW *| δ)) logWs δGs
    {- | Compute the mean gradient estimate for a random variable v's associated parameters -}
    estimateGrad :: forall d. (DiffDistribution d) => Key d -> GTrace -> GTrace
    estimateGrad v = let δFv      = map (fromJust . Trace.lookup v) δFs
                         norm_δFv = ((*|) norm_c . foldr (|+|) (Vec.zeros (Proxy @(Arity d))) ) δFv
                     in  Trace.insert v norm_δFv