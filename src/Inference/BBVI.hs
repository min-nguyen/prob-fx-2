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
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util

{- | Top-level wrapper for BBVI inference that takes a separate model and guide.
-}
bbvi :: forall env a b. (Show (Env env))
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> Model env [ObsRW env, Dist] a      -- ^ model P
  -> Env env                            -- ^ model environment (containing only observed data Y)
  -> Model env [ObsRW env, Dist] b      -- ^ guide Q
  -> Sampler DTrace                     -- ^ final proposal distributions Q(λ_T)
bbvi num_timesteps num_samples model model_env guide_model = do
  {- | Prepare guide by:
        1) Handling the guide-model under the model environment.
            - In the proper case that the guide never refers model variables that are to be conditioned against (indicated by the presence of observed values in the model environment), this will be trivially equivalent to using an empty model environment. In this case, the guide will initially only contain the correct set of @Sample@ operations prior to applying `installLearn`.
            - In the inproper case that the guide refers to model variables to be conditioned against, then handling the guide under the model environment and then handling these variables as @Observe@ operations using SIM.handleObs will ignore any of their (importance weighting) side-effects; in constrast, handling the guide under the *empty environment* would incorrectly produce some @Sample@ operations that should not be present. Also note that the original observed values will later be reproduced in the guide's output environment.
        2) Replacing any differentiable @Sample@ operations with @Learn@.
  -}
  let guide :: Prog '[Learn, Observe, Sample] (b, Env env)
      guide = (installLearn . handleCore model_env) guide_model
  -- | Collect initial proposal distributions
  proposals_0 <- collectProposals guide
  -- | Run BBVI for T optimisation steps
  handleLift (bbviInternal num_timesteps num_samples model model_env guide proposals_0)

{- | BBVI on a guide and model. -}
bbviInternal :: (LastMember (Lift Sampler) fs, Show (Env env))
  => Int                                    -- ^ number of optimisation steps (T)
  -> Int                                    -- ^ number of samples to estimate the gradient over (L)
  -> Model env [ObsRW env, Dist] a          -- ^ model P
  -> Env env                                -- ^ model environment (containing only observed data Y)
  -> Prog [Learn, Observe, Sample] (b, Env env)      -- ^ guide Q
  -> DTrace                                 -- ^ guide initial proposals Q(λ_0)
  -> Prog fs DTrace                         -- ^ final proposal distributions Q(λ_T)
bbviInternal num_timesteps num_samples model model_env guide proposals_0 = do
  foldr (>=>) pure [bbviStep t num_samples model model_env guide | t <- [1 .. num_timesteps]] proposals_0

{- | 1. For L iterations,
        a) Generate values X from the guide Q, accumulating:
            - the total guide log-weight:  log(Q(X; λ))
            - the gradient log-pdfs of the guide: δlog(Q(X; λ))
        b) Execute the model P using values X ~ Q, accumulating:
            - the total model log-weight:  log(P(X, Y))
     2. Compute an estimate of the ELBO gradient: E[δelbo]
     3. Update the proposal distributions Q(λ) of the guide
-}
bbviStep :: (LastMember (Lift Sampler) fs, Show (Env env))
  => Int                                   -- ^ time step index (t)
  -> Int                                   -- ^ number of samples to estimate the gradient over (L)
  -> Model env [ObsRW env, Dist] a                -- ^ model P
  -> Env env                                      -- ^ model environment (containing only observed data Y)
  -> Prog [Learn, Observe, Sample] (b, Env env)   -- ^ guide Q
  -> DTrace                                       -- ^ guide proposal distributions Q(λ_t)
  -> Prog fs DTrace                               -- ^ next proposal distributions Q(λ_{t+1})
bbviStep timestep num_samples model model_env guide proposals = do
  -- | Execute the guide for (L) iterations
  (((_, guide_envs), guide_logWs), grads)
      <- Util.unzip4 <$> replicateM num_samples ((lift . handleGuide guide) proposals)
  -- | Execute the model under the union of the model and guide environment
  (_               , model_logWs)
      <- Util.unzip3 <$> mapM ((lift . handleModel model) . Env.union model_env) guide_envs
  -- | Compute total log-importance-weight: logW = log(P(X, Y)) - log(Q(X; λ))
  let logWs      = zipWith (-) model_logWs guide_logWs
  -- | Compute the ELBO gradient estimates
  let δelbos     = likelihoodRatioEstimator num_samples logWs grads
  -- | Update the parameters of the proposal distributions Q
  let proposals' = updateParams 1 proposals δelbos

  pure proposals'

{- | Execute the guide Q under a provided set of proposal distributions Q(λ), producing:
      1. The output environment of latent variables X (given `env` contains X) generated by @Sample@s,
         i.e. fixed non-differentiable dists, and @Learn@s, i.e. learnable proposal dists:
            X ~ Q(λ),          where Q can be fixed or learnable
      2. The total log-weight of latent variables X, resulting from @Sample@s and @Learn@s:
            log(Q(X; λ)),      where Q can be fixed or learnable
      3. The gradients of all proposal distributions Q(λ) at X:
            δlog(Q(X; λ)),     where Q is learnable
 -}
handleGuide :: Prog [Learn, Observe, Sample] (a, Env env) -> DTrace -> Sampler (((a, Env env), LogP), GTrace)
handleGuide guide proposals =
  (SIM.handleSamp . SIM.handleObs . handleLearn . weighGuide . updateLearn proposals) guide

{- | Execute the model P under an environment of samples X from the guide and observed values Y, producing:
       1. An output environment which we discard
       2. The total log-weight of all @Sample@ and @Observe@ operations: log(P(X, Y))
-}
handleModel :: Model env [ObsRW env, Dist] a -> Env env -> Sampler ((a, Env env), LogP)
handleModel model env =
  (SIM.handleSamp . SIM.handleObs . weighModel . handleCore env) model

{- | Collect all learnable distributions as the initial set of proposals.
-}
collectProposals :: Prog '[Learn, Observe, Sample] a -> Sampler DTrace
collectProposals = SIM.handleSamp . SIM.handleObs . ((fst <$>) . handleLearn) . loop Trace.empty where
  loop :: DTrace -> Prog '[Learn, Observe, Sample] a -> Prog '[Learn, Observe, Sample] DTrace
  loop proposals (Val _)   = pure proposals
  loop proposals (Op op k) = case prj op of
    Just (LearnS q α)   -> do let proposals' = Trace.insert (Key α) q proposals
                              Op op (loop proposals' . k)
    Just (LearnO q x α) -> do let proposals' = Trace.insert (Key α) q proposals
                              Op op (loop proposals' . k)
    Nothing -> Op op (loop proposals . k)

{- | Replace all differentiable @Sample@ operations, representing the proposal distributions Q(λ),
     with @Learn@ operations.
-}
installLearn :: Member Sample es => Prog es a -> Prog (Learn : es) a
installLearn (Val x)   = pure x
installLearn (Op op k) = case prj op of
  Just (Sample q α) -> case isDifferentiable q of
      Nothing      -> Op (weaken op) (installLearn  . k)
      Just Witness -> do x <- call (LearnS q α)
                         (installLearn  . k) x
  Nothing -> Op (weaken op) (installLearn . k)

{- | Set the proposal distributions Q(λ) of @Learn@ operations.
-}
updateLearn :: forall es a. Member Learn es => DTrace -> Prog es a -> Prog es a
updateLearn proposals = loop where
  loop :: Prog es a -> Prog es a
  loop (Val a)   = pure a
  loop (Op op k) = case prj op of
    Just (LearnS q α) -> do
      let q' = fromMaybe q (Trace.lookup (Key α) proposals)
      x <- call (LearnS q' α)
      (loop . k) x
    Just (LearnO q x α) -> error "BBVI.update: Should not happen"
    Nothing -> Op op (loop . k)

{- | Handle each @Learn@ operation by sampling from its proposal distribution:
        X ~ Q(λ)
     And record the gradient log-pdf at sample X w.r.t parameters λ:
        δlog(Q(X; λ))
-}
handleLearn :: forall es a. Member Sample es => Prog (Learn : es) a -> Prog es (a, GTrace)
handleLearn = loop Trace.empty where
  loop :: GTrace -> Prog (Learn : es) a -> Prog es (a, GTrace)
  loop grads (Val a)   = pure (a, grads)
  loop grads (Op op k) = case discharge op of
    Right (LearnS (q :: d) α) -> do
         x <- call (Sample q α)
         let grads' = Trace.insert @d (Key α) (gradLogProb q x) grads
         (loop grads' . k) x
    Right (LearnO (q :: d) x α) -> error "BBVI.handleLearn: Should not happen"
    Left op' -> Op op' (loop grads . k)

{- | Compute the log probability over the guide:
        log(Q(X; λ))
-}
weighGuide :: forall es a. (Members [Learn, Sample] es) => Prog es a -> Prog es (a, LogP)
weighGuide = loop 0 where
  loop :: LogP -> Prog es a -> Prog es (a, LogP)
  loop logW (Val a)   = pure (a, logW)
  loop logW (Op op k) = case  op of
      -- | Compute: log(Q(X; λ)) for proposal distributions
      LearnSPrj q α   -> Op op (\x -> loop (logW + logProb q x) $ k x)
      -- | Compute: log(Q(X; λ)) for non-differentiable distributions
      SampPrj q α     -> Op op (\x -> loop (logW + logProb q x) $ k x)
      _               -> Op op (loop logW . k)

{- | Compute the log probability over the model:
        log(P(X, Y))
-}
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

{- | Compute the ELBO gradient estimates for each variable v over L samples.
        E[δelbo(v)] = sum (F_v^{1:L} - b_v * G_v^{1:L}) / L
-}
likelihoodRatioEstimator :: Int -> [LogP] -> [GTrace] -> GTrace
likelihoodRatioEstimator l_samples logWs δGs = foldr (\(Some v) -> Trace.insert v (estδELBO v)) Trace.empty vars
  where
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
      let δGv  = map (fromJust . Trace.lookup v) δGs      -- G_v^{1:L}
          δFv  = map (fromJust . Trace.lookup v) δFs      -- F_v^{1:L}
          baseline_v = Vec.covar δFv δGv |/| Vec.var δGv  -- b_v
          δELBOv = let δelbos_v = zipWith (\δg δf -> δf |-| (baseline_v |*| δg)) δGv δFv
                   in  ((*|) (1/fromIntegral l_samples) . foldr (|+|) (Vec.zeros (Proxy @(Arity d))) ) δelbos_v
      in  δELBOv

      --trace ("traceGs_v: " ++ show traceGs_v ++ "\n traceFs_v" ++ show traceFs_v ++ "\n baseline: " ++ show baseline_v ++ "\n Elbos : " ++ show δelbos_v )


{- | Update each variable v's parameters λ using their estimated ELBO gradients E[δelbo(v)].
        λ_{t+1} = λ_t + η_t * E[δelbo(v)]
     where the gradient δelbo(v) is implicitly w.r.t λ
-}
updateParams
  :: Double  -- ^ learning rate             η
  -> DTrace  -- ^ optimisable distributions Q(λ_t)
  -> GTrace  -- ^ elbo gradient estimates   E[δelbo]
  -> DTrace  -- ^ updated distributions     Q(λ_{t+1})
updateParams η = Trace.intersectLeftWith (\q δλ ->  q `safeAddGrad` (η *| δλ))
