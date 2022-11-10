{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{- | BBVI inference that takes a model and generates a model and guide as a combined program.
     Note: this naturally has different results than Inference.BBVI due ignoring the log-weights of *non-differentiable* @Sample@ operations (see definition of the `traceLogProbs` handler).
-}

module Inference.BBVICombined
  where

import Data.Maybe
import Data.Proxy
import Data.Bifunctor ( Bifunctor(first) )
import Control.Monad ( replicateM, (>=>) )
import Effects.Dist
import Effects.Lift
import Effects.ObsRW ( ObsRW )
import Effects.State ( modify, handleState, State )
import Env ( Env )
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
import qualified Util

{- | Top-level wrapper for BBVI inference.
-}
bbvi :: forall env a. Int -> Int -> Model env [ObsRW env, Dist] a -> Env env -> Sampler DTrace
bbvi num_timesteps num_samples model env_in = do
  -- | Transform probabilistic program to use @Score@ operations
  let bbvi_prog :: Prog [Score, Observe, Sample] ((a, Env env), DTrace)
      bbvi_prog = installScore (handleCore env_in model)
  -- | Collect initial proposal distributions
  ((_, proposals_0), _) <- (SIM.handleSamp . SIM.handleObs . handleScore) bbvi_prog
  handleLift (bbviInternal num_timesteps num_samples proposals_0 bbvi_prog)

{- | BBVI on a probabilistic program.
-}
bbviInternal :: forall fs a. (LastMember (Lift Sampler) fs)
  => Int                        -- ^ number of optimisation steps (T)
  -> Int                        -- ^ number of samples to estimate the gradient over (L)
  -> DTrace
  -> Prog [Score, Observe, Sample] a
  -> Prog fs DTrace
bbviInternal num_timesteps num_samples proposals_0 bbvi_prog = do
  -- | Run BBVI for T optimisation steps
  foldr (>=>) pure (replicate num_timesteps ((snd <$>) . bbviStep num_samples bbvi_prog)) proposals_0

{- | 1. Execute a model for L iterations under proposals Q, accumulating:
      - the total importance weights: logW_l
      - the gradient log-pdfs: G_l
     2. Compute an estimate of the ELBO gradient: E[δelbo]
     3. Update the proposal distributions: Q
-}
bbviStep :: (ProbSig es, LastMember (Lift Sampler) fs)
  => Int                            -- ^ number of samples to estimate the gradient over (L)
  -> Prog (Score : es) a            -- ^ initial bbvi probabilistic program
  -> DTrace                         -- ^ proposal distributions (Q)
  -> Prog fs ([(a, LogP)], DTrace)  -- ^ weighted outputs + next proposal distributions (Q')
bbviStep num_samples bbvi_prog proposals = do
  -- | Execute a model for L iterations, collecting gradient traces G_l and importance weights logW_l:
  ((as, logWs), grads) <- Util.unzip3 <$> replicateM num_samples (lift (runBBVI proposals bbvi_prog))
  -- | Compute the ELBO gradient estimates
  let δelbos     = estELBOs num_samples logWs grads
  -- | Update the parameters of the proposal distributions Q
      proposals' = optimizeParams 1.0 proposals δelbos
  -- liftPutStrLn $ "Proposal Distributions Q:\n" ++ show traceQ ++ "\n"
  -- liftPutStrLn $ "Gradient Log-Pdfs G_l for {1:L}:\n" ++ show traceGs ++ "\n"
  -- liftPutStrLn $ "Log Importance Weights logW_l for {1:L}:\n" ++ show logWs ++ "\n"
  -- liftPutStrLn $ "ELBO Grad Estimate g:\n" ++ show δelbos ++ "\n"
  -- liftPutStrLn $ "Updated Proposal Distributions Q':\n" ++ show traceQ' ++ "\n"
  pure (zip as logWs, proposals')

{- | One iteration of model execution under BBVI.
-}
runBBVI :: [Score, Observe, Sample] ~ es => DTrace -> Prog es a -> Sampler ((a, LogP), GTrace)
runBBVI proposals =
  SIM.handleSamp . SIM.handleObs . handleScore . traceLogProbs . updateScore proposals

{- | Replace all differentiable @Sample@ operations with @Score@ operations, initialising
     the proposals distributions Q as priors P.
-}
installScore :: forall es a. Member Sample es => Prog es a -> Prog (Score : es) (a, DTrace)
installScore = loop dempty where
  loop :: DTrace -> Prog es a -> Prog (Score : es) (a, DTrace)
  loop proposals  (Val x)   = pure (x, proposals)
  loop proposals  (Op op k) = case prj op of
    Just (Sample d α) -> case isDifferentiable d of
      Nothing      -> Op (weaken op) (loop proposals . k)
      Just Witness -> do let proposals' = dinsert (Key α) d proposals
                         x <- call (Score d d α)
                         (loop proposals' . k) x
    Nothing -> Op (weaken op) (loop proposals . k)

{- | Set the proposal distributions Q(λ) of @Score@ operations.
-}
updateScore :: forall es a. Member Score es => DTrace -> Prog es a -> Prog es a
updateScore proposals = loop where
  loop :: Prog es a -> Prog es a
  loop (Val x)   = pure x
  loop (Op op k) = case prj op of
    Just (Score d _ α) -> do
      let q = fromMaybe d (dlookup (Key α) proposals)
      x <- call (Score d q α)
      (loop . k) x
    Nothing -> Op op (loop . k)

{- | Trivially handle each @Score@ operation by sampling from its proposal distribution:
        X ~ Q
    And for each proposal distribution Q(λ), compute its gradient log-pdf at sample X w.r.t parameters λ:
        δlog(Q(X; λ))
-}
handleScore :: forall es a. Member Sample es => Prog (Score : es) a -> Prog es (a, GTrace)
handleScore = loop dempty where
  loop :: GTrace -> Prog (Score : es) a -> Prog es (a, GTrace)
  loop grads (Val x)   = pure (x, grads)
  loop grads (Op op k) = case discharge op of
    Right (Score _ (q :: d) α) -> do
         x <- call (Sample q α)
         let grads' = ginsert @d (Key α) (gradLogProb q x) grads
         (loop grads' . k) x
    Left op' -> Op op' (loop grads . k)

{- | Compute the total importance weight:
        log(P(X, Y) / Q(X; λ))
-}
traceLogProbs :: forall es a. (Members [Score, Observe] es) => Prog es a -> Prog es (a, LogP)
traceLogProbs = loop 0 where
  loop :: LogP -> Prog es a -> Prog es (a, LogP)
  loop logW (Val x)   = pure (x, logW)
  loop logW (Op op k) = case op of
      -- | Compute: log(P(Y))
      ObsPrj d y α   -> Op op (\x -> loop (logW + logProb d x) $ k x)
      -- | Compute: log(P(X)) - log(Q(X; λ))
      ScorePrj d q α -> Op op (\x -> loop (logW + logProb d x - logProb q x) $ k x)
      _              -> Op op (loop logW . k)


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
    in  ((*|) (1/fromIntegral l_samples) . foldr (|+|) (Vec.zero (Proxy @(Arity d))) ) δelbos_v

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
