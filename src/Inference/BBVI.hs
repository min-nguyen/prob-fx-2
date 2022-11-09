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
import Env ( Env, emptyEnv )
import LogP ( LogP(..), normaliseLogPs, expLogP )
import Model
import PrimDist
import Prog ( discharge, Prog(..), call, weaken, LastMember, Member (..), Members, weakenProg )
import Sampler
import Trace
import Debug.Trace
import qualified Inference.SIM as SIM
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util

{- | Top-level wrapper for BBVI inference.
-}
bbvi :: forall env a. Show (Env env) => Int -> Int -> Model env [ObsRW env, Dist] a -> Env env -> Sampler DTrace
bbvi num_timesteps num_samples model env_in = do
  -- | Transform model to ignore @Observe@ operations and replace differentiable @Sample@ operations with @Learn@
  let guide :: Prog '[Learn, Sample] ((a, Env env), DTrace)
      guide = (installLearn . SIM.handleObs . handleCore env_in) model
  -- | Collect initial proposal distributions
  ((_, proposals_0), _) <- (SIM.handleSamp . handleLearn) guide
  handleLift (bbviInternal num_timesteps num_samples proposals_0 (fst <$> guide) model)

{- | BBVI on a probabilistic program.
-}
bbviInternal :: (LastMember (Lift Sampler) fs, Show (Env env))
  => Int                                    -- ^ number of optimisation steps (T)
  -> Int                                    -- ^ number of samples to estimate the gradient over (L)
  -> DTrace                                 -- ^ initial proposals
  -> Prog [Learn, Sample] (a, Env env)      -- ^ guide
  -> Model env [ObsRW env, Dist] a          -- ^ model
  -> Prog fs DTrace
bbviInternal num_timesteps num_samples proposals_0 guide model = do
  -- | Run BBVI for T optimisation steps
  foldr (>=>) pure [bbviStep t num_samples guide model | t <- [1 .. num_timesteps]] proposals_0

{- | 1. Execute a model for L iterations under proposals Q, accumulating:
      - the total importance weights: logW_l
      - the gradient log-pdfs: G_l
     2. Compute an estimate of the ELBO gradient: E[δelbo]
     3. Update the proposal distributions: Q
-}
bbviStep :: (ProbSig es, LastMember (Lift Sampler) fs, Show (Env env))
  => Int                                   -- ^ number of samples to estimate the gradient over (L)
  -> Int                                   -- ^ time step index
  -> Prog [Learn, Sample] (a, Env env)     -- ^ guide
  -> Model env [ObsRW env, Dist] a         -- ^ model
  -> DTrace                                -- ^ proposal distributions (Q)
  -> Prog fs DTrace                        -- ^ next proposal distribution (Q')
bbviStep timestep num_samples guide model proposals = do
  -- | Execute a guide for L iterations, collecting gradient traces G_l and importance weights logW_l:
  (((_, envs), logWs_guide), grads)
      <- Util.unzip4 <$> replicateM num_samples ((lift . handleGuide guide) proposals)
  -- | Execute the model using the guide outputs
  ((as, _), logWs_model)
      <- Util.unzip3 <$> mapM (lift . handleModel model) envs
  -- | Compute log(P(X, Y)) - log(Q(X; λ))
  let logWs   = zipWith (-) logWs_model logWs_guide
  -- | Compute the ELBO gradient estimates
  let δelbos  = estELBOs num_samples logWs grads
  -- | Update the parameters of the proposal distributions Q
  let proposals' = optimizeParams 1 proposals δelbos
  -- liftPutStrLn $ "Guide output environments {X, Y}:\n" ++ show envs ++ "\n"
  -- liftPutStrLn $ "Proposal Distributions Q:\n" ++ show proposals ++ "\n"
  -- liftPutStrLn $ "Gradient Log-Pdfs G_l for {1:L}:\n" ++ show traceGs ++ "\n"
  -- liftPutStrLn $ "Log Importance Weights logW for Guide:\n" ++ show logWs_guide ++ "\n"
  -- liftPutStrLn $ "Log Importance Weights logW for Model:\n" ++ show logWs_model ++ "\n"
  -- liftPutStrLn $ "ELBO Grad Estimate g:\n" ++ show δelbos ++ "\n"
  -- liftPutStrLn $ "Updated Proposal Distributions Q':\n" ++ show proposals' ++ "\n"
  pure proposals'

{- | One iteration of guide execution under BBVI. -}
handleGuide :: Prog [Learn, Sample] (a, Env env) -> DTrace -> Sampler (((a, Env env), LogP), GTrace)
handleGuide guide proposals =
  (SIM.handleSamp . handleLearn . weighGuide . updateLearn proposals) guide

{- | One iteration of model execution under BBVI. -}
handleModel :: Model env [ObsRW env, Dist] a -> Env env -> Sampler ((a, Env env), LogP)
handleModel model env =
  (SIM.handleSamp . SIM.handleObs . weighModel . handleCore env) model

{- | Replace all differentiable @Sample@ operations with @Learn@ operations, initialising
     the proposals distributions Q as priors P.
-}
installLearn :: forall es a. Member Sample es => Prog es a -> Prog (Learn : es) (a, DTrace)
installLearn = loop dempty where
  loop :: DTrace -> Prog es a -> Prog (Learn : es) (a, DTrace)
  loop proposals  (Val x)   = pure (x, proposals)
  loop proposals  (Op op k) = case prj op of
    Just (Sample d α) -> case isDifferentiable d of
        Nothing      -> Op (weaken op) (loop proposals . k)
        Just Witness -> do let proposals' = dinsert (Key α) d proposals
                           x <- call (Learn d α)
                           (loop proposals' . k) x
    Nothing -> Op (weaken op) (loop proposals . k)

{- | Set the proposal distributions Q(λ) of @Learn@ operations.
-}
updateLearn :: forall es a. Member Learn es => DTrace -> Prog es a -> Prog es a
updateLearn proposals = loop where
  loop :: Prog es a -> Prog es a
  loop (Val x)   = pure x
  loop (Op op k) = case prj op of
    Just (Learn q α) -> do
      let q' = fromMaybe q (dlookup (Key α) proposals)
      x <- call (Learn q' α)
      (loop . k) x
    Nothing -> Op op (loop . k)

{- | Handle each @Learn@ operation by sampling from its proposal distribution:
        X ~ Q(λ)
     And record the gradient log-pdf at sample X w.r.t parameters λ:
        δlog(Q(X; λ))
-}
handleLearn :: forall es a. Member Sample es => Prog (Learn : es) a -> Prog es (a, GTrace)
handleLearn = loop dempty where
  loop :: GTrace -> Prog (Learn : es) a -> Prog es (a, GTrace)
  loop grads (Val x)   = pure (x, grads)
  loop grads (Op op k) = case discharge op of
    Right (Learn (q :: d) α) -> do
         x <- call (Sample q α)
         let grads' = ginsert @d (Key α) (gradLogProb q x) grads
         (loop grads' . k) x
    Left op' -> Op op' (loop grads . k)

{- | Compute the log probability over the guide:
        log(Q(X; λ))
-}
weighGuide :: forall es a. (Members [Learn, Sample] es) => Prog es a -> Prog es (a, LogP)
weighGuide = loop 0 where
  loop :: LogP -> Prog es a -> Prog es (a, LogP)
  loop logW (Val x)   = pure (x, logW)
  loop logW (Op op k) = case  op of
      -- | Compute: log(Q(X; λ)) for proposal distributions
      LearnPrj q α    -> Op op (\x -> loop (logW + logProb q x) $ k x)
      -- | Compute: log(Q(X; λ)) for non-differentiable distributions
      SampPrj d α     -> Op op (\x -> loop (logW + logProb d x) $ k x)
      _               -> Op op (loop logW . k)

{- | Compute the log probability over the model:
        log(P(X, Y))
-}
weighModel :: forall es a. (Members [Sample, Observe] es) => Prog es a -> Prog es (a, LogP)
weighModel = loop 0 where
  loop :: LogP -> Prog es a -> Prog es (a, LogP)
  loop logW (Val x)   = pure (x, logW)
  loop logW (Op op k) = case op of
      -- | Compute: log(P(Y))
      ObsPrj d y α -> Op op (\x -> loop (logW + logProb d x) $ k x)
      -- | Compute: log(P(X))
      SampPrj d α  -> Op op (\x -> loop (logW + logProb d x) $ k x)
      _            -> Op op (loop logW . k)

{- | Compute the ELBO gradient estimates for each variable v over L samples.
        E[δelbo(v)] = sum (F_v^{1:L} - b_v * G_v^{1:L}) / L
-}
estELBOs :: Int -> [LogP] -> [GTrace] -> GTrace
estELBOs l_samples logWs traceGs = foldr f dempty vars where
  {- | Store the ELBO gradient estimate E[δelbo(v)] for a given variable v. -}
  f :: Some DiffDistribution Key -> GTrace -> GTrace
  f (Some kx) = ginsert kx (estELBO kx traceGs traceFs)
  {- | Store the ELBO gradient estimate E[δelbo(v)] for a given variable v. -}
  vars :: [Some DiffDistribution Key]
  vars = (gkeys . head) traceGs
  {- | Uniformly scale each iteration's gradient trace G^l by its corresponding (normalised) importance weight W_norm^l.
          F^{1:L} = W_norm^{1:L} * G^{1:L}
       where the normalised importance weight is defined via:
          log(W_norm^l) = log(W^l) + max(log(W^{1:L})) -}
  traceFs :: [GTrace]
  traceFs = zipWith (\logW -> gmap (expLogP logW *|)) (normaliseLogPs logWs) traceGs
  {- | Compute the ELBO gradient estimate for a random variable v's associated parameters:
          E[δelbo(v)] = sum (F_v^{1:L} - b_v * G_v^{1:L}) / L
       where the baseline is:
          b_v    = covar(F_v^{1:L}, G_v^{1:L}) / var(G_v^{1:L}) -}
  estELBO :: forall d. ( DiffDistribution d)
    => Key d    -- ^   v
    -> [GTrace]  -- ^   G^{1:L}
    -> [GTrace]  -- ^   F^{1:L}
    -> Vec (Arity d) Double         -- ^   E[δelbo(v)]
  estELBO v traceGs traceFs =
    let traceGs_v  = map (fromJust . glookup v) traceGs                                 -- G_v^{1:L}
        traceFs_v  = map (fromJust . glookup v) traceFs                                 -- F_v^{1:L}
        baseline_v = Vec.covar traceFs_v traceGs_v |/| Vec.var traceGs_v  -- b_v
        δelbos_v   = zipWith (\g_l f_l -> f_l |-| (baseline_v |*| g_l)) traceGs_v traceFs_v

    in  --trace ("traceGs_v: " ++ show traceGs_v ++ "\n traceFs_v" ++ show traceFs_v ++ "\n baseline: " ++ show baseline_v ++ "\n Elbos : " ++ show δelbos_v )
        ((*|) (1/fromIntegral l_samples) . foldr (|+|) (zero (Proxy @d)) ) δelbos_v

{- | Update each variable v's parameters λ using their estimated ELBO gradients E[δelbo(v)].
        λ_{t+1} = λ_t + η_t * E[δelbo(v)]
     where the gradient δelbo(v) is implicitly w.r.t λ
-}
optimizeParams
  :: Double  -- ^ learning rate             η
  -> DTrace  -- ^ optimisable distributions Q(λ_t)
  -> GTrace  -- ^ elbo gradient estimates   E[δelbo]
  -> DTrace  -- ^ updated distributions     Q(λ_{t+1})
optimizeParams η = dintersectLeftWith (\q δλ ->  q `safeAddGrad` (η *| δλ))
