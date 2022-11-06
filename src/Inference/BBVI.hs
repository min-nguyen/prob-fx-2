{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

{- | Likelihood-Weighting inference.
-}

module Inference.BBVI
  where

import Data.Maybe
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
import qualified Inference.LW as LW

{- | Top-level wrapper for BBVI inference.
-}
bbvi :: Int -> Int -> Model env [ObsRW env, Dist] a -> Env env -> Sampler DTrace
bbvi t_steps l_samples model env_in = do
  let prog_0 = handleCore env_in model
  handleLift (bbviInternal t_steps l_samples prog_0)

{- | BBVI on a probabilistic program.
-}
bbviInternal :: forall fs a. (LastMember (Lift Sampler) fs)
  => Int                        -- ^ number of optimisation steps (T)
  -> Int                        -- ^ number of samples to estimate the gradient over (L)
  -> Prog [Observe, Sample] a
  -> Prog fs DTrace
bbviInternal t_steps l_samples prog = do
  -- | Transform probabilistic program to use @Score@ operations
  let bbvi_prog :: Prog [Score, Observe, Sample] (a, DTrace)
      bbvi_prog = installScore prog
  -- | Collect initial proposal distributions
  (_, traceQ0) <- (lift . SIM.handleSamp . SIM.handleObs . handleScore) bbvi_prog
  -- | Run BBVI for T optimisation steps
  foldr (>=>) pure (replicate t_steps ((snd <$>) . bbviStep l_samples bbvi_prog)) traceQ0

{- | 1. Execute a model for L iterations under proposals Q, accumulating:
      - the total importance weights: logW_l
      - the gradient log-pdfs: G_l
     2. Compute an estimate of the ELBO gradient: E[δelbo]
     3. Update the proposal distributions: Q
-}
bbviStep :: (ProbSig es, LastMember (Lift Sampler) fs)
  => Int                            -- ^ number of samples to estimate the gradient over (L)
  -> Prog (Score : es) a                      -- ^ initial bbvi probabilistic program
  -> DTrace                         -- ^ proposal distributions (Q)
  -> Prog fs ([(a, LogP)], DTrace)  -- ^ weighted outputs + next proposal distributions (Q')
bbviStep l_samples bbvi_prog traceQ = do
  -- | Execute a model for L iterations, collecting gradient traces G_l and importance weights logW_l:
  ((as, traceGs), logWs) <- lift $ first unzip . unzip <$> replicateM l_samples (runBBVI traceQ bbvi_prog)
  -- | Compute the ELBO gradient estimates
  let δelbos  = estELBOs l_samples logWs traceGs
  -- | Update the parameters of the proposal distributions Q
      traceQ' = optimizeParams 1.0 traceQ δelbos
  -- liftPutStrLn $ "Proposal Distributions Q:\n" ++ show traceQ ++ "\n"
  -- liftPutStrLn $ "Gradient Log-Pdfs G_l for {1:L}:\n" ++ show traceGs ++ "\n"
  -- liftPutStrLn $ "Log Importance Weights logW_l for {1:L}:\n" ++ show logWs ++ "\n"
  -- liftPutStrLn $ "ELBO Grad Estimate g:\n" ++ show δelbos ++ "\n"
  -- liftPutStrLn $ "Updated Proposal Distributions Q':\n" ++ show traceQ' ++ "\n"
  pure (zip as logWs, traceQ')

{- | One iteration of model execution under BBVI.
-}
runBBVI :: [Score, Observe, Sample] ~ es => DTrace -> Prog es a -> Sampler ((a, GTrace), LogP)
runBBVI proposals =
  SIM.handleSamp . SIM.handleObs . handleScore . traceLogProbs . traceGrads . updateScore proposals

{- | Replace all differentiable @Sample@ operations with @Score@ operations, initialising
     the proposals distributions Q as priors P.
-}
installScore :: forall es a. Member Sample es => Prog es a -> Prog (Score : es) (a, DTrace)
installScore = loop dempty where
  loop :: DTrace -> Prog es a -> Prog (Score : es) (a, DTrace)
  loop traceQ  (Val x)   = pure (x, traceQ)
  loop traceQ  (Op op k) = case prj op of
    Just (Sample d α) ->
      case isDifferentiable d of
        Nothing      -> Op (weaken op) (loop traceQ . k)
        Just Witness -> do let traceQ' = dinsert (DKey α) d traceQ
                           x <- call (Score d d α)
                           (loop traceQ' . k) x
    Nothing -> Op (weaken op) (loop traceQ . k)

{- | Set the proposal distributions Q(λ) of @Score@ operations.
-}
updateScore :: forall es a. Member Score es => DTrace -> Prog es a -> Prog es a
updateScore traceQ = loop where
  loop :: Prog es a -> Prog es a
  loop (Val x)   = pure x
  loop (Op op k) = case prj op of
    Just (Score d _ α) -> do
      let q = dlookupDefault (DKey α) traceQ d
      x <- call (Score d q α)
      (loop . k) x
    Nothing -> Op op (loop . k)

{- | Trivially handle each @Score@ operation by sampling from its proposal distribution:
        X ~ Q
-}
handleScore :: Member Sample es => Prog (Score : es) a -> Prog es a
handleScore (Val x)   = pure x
handleScore (Op op k) = case discharge op of
  Right (Score _ q α) -> call (Sample q α) >>= handleScore . k
  Left op'            -> Op op' (handleScore  . k)

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

{- | For each proposal distribution Q(λ), compute its gradient log-pdf at sample X w.r.t parameters λ:
        δlog(Q(X; λ))
-}
traceGrads :: forall es a. Member Score es => Prog es a -> Prog es (a, GTrace)
traceGrads = loop dempty where
  loop :: GTrace -> Prog es a -> Prog es (a, GTrace)
  loop traceG (Val x)   = pure (x, traceG)
  loop traceG (Op op k) = case prj op of
    Just (Score _ q α)
      -> Op op (\x -> let traceG' = dinsert (DKey α) (gradLogProb q x) traceG
                      in  (loop traceG' . k) x)
    _ -> Op op (loop traceG . k)

{- | Compute the ELBO gradient estimates for each variable v over L samples.
        E[δelbo(v)] = sum (F_v^{1:L} - b_v * G_v^{1:L}) / L
-}
estELBOs :: Int -> [LogP] -> [GTrace] -> GTrace
estELBOs l_samples logWs traceGs = foldr f dempty vars where
  {- | Store the ELBO gradient estimate E[δelbo(v)] for a given variable v. -}
  f :: Some DKey -> GTrace -> GTrace
  f (Some kx) = dinsert kx (estELBO kx traceGs traceFs)
  {- | Store the ELBO gradient estimate E[δelbo(v)] for a given variable v. -}
  vars :: [Some DKey]
  vars = (dkeys . head) traceGs
  {- | Uniformly scale each iteration's gradient trace G^l by its corresponding (normalised) importance weight W_norm^l.
          F^{1:L} = W_norm^{1:L} * G^{1:L}
       where the normalised importance weight is defined via:
          log(W_norm^l) = log(W^l) + max(log(W^{1:L})) -}
  traceFs :: [GTrace]
  traceFs = zipWith (\logW -> dmap (expLogP logW *|)) (normaliseLogPs logWs) traceGs
  {- | Compute the ELBO gradient estimate for a random variable v's associated parameters:
          E[δelbo(v)] = sum (F_v^{1:L} - b_v * G_v^{1:L}) / L
       where the baseline is:
          b_v    = covar(F_v^{1:L}, G_v^{1:L}) / var(G_v^{1:L}) -}
  estELBO :: DiffDistribution d
    => DKey d    -- ^   v
    -> [GTrace]  -- ^   G^{1:L}
    -> [GTrace]  -- ^   F^{1:L}
    -> d         -- ^   E[δelbo(v)]
  estELBO v traceGs traceFs =
    let traceGs_v  = map (fromJust . dlookup v) traceGs                                 -- G_v^{1:L}
        traceFs_v  = map (fromJust . dlookup v) traceFs                                 -- F_v^{1:L}
        baseline_v = covarGrad traceFs_v traceGs_v |/| varGrad traceGs_v  -- b_v
        δelbos_v   = zipWith (\g_l f_l -> f_l |-| (baseline_v |*| g_l)) traceGs_v traceFs_v
    in  ((*|) (1/fromIntegral l_samples) . foldr (|+|) zero) δelbos_v

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
