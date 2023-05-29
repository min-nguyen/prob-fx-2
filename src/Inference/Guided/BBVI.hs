{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

{- | BBVI inference on a model and guide as separate programs.
-}

module Inference.Guided.BBVI
  where

import Inference.Guided.Guided
import Effects.Guide
import Data.Maybe
import LogP
import Sampler
import           Trace
import Inference.MC.LW (likelihood)
import Dist
import qualified Vec
import           Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Data.Data (Proxy(..))
import Comp
import Inference.MC.SIM
import Effects.MulDist
import Data.Some

{- | Top-level wrapper for BBVI inference that takes a separate model and guide.
-}
bbvi :: forall es a. ()
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> GuidedModel '[Sampler] a      -- ^ guide Q(X; λ)
  -> Sampler Guides                 -- ^ final guide parameters λ_T
bbvi num_timesteps num_samples model = do
  λ_0 <- collectGuide model
  -- liftIO (print λ_0)
  (runImpure . handleLRatio)
    $ guidedLoop num_timesteps num_samples exec model λ_0

-- | Compute Q(X; λ)
exec :: Guides -> GuidedModel '[Sampler] a -> Sampler ((a, ΔGuides), LogP)
exec params = runImpure . mergeWeights . priorDiff . defaultSample . likelihood .  useGuides params  where
  mergeWeights = fmap (\((x, w_lat), w_obs) -> (x, w_lat + w_obs))

-- | Sample from each @Guide@ distribution, x ~ Q(X; λ), and record its grad-log-pdf, δlog(Q(X = x; λ)).
priorDiff :: forall es a. Members '[Sampler] es => Handler Guide es a (a, LogP)
priorDiff  = handleWith 0 (\s a -> Val (a, s)) hop where
  hop :: LogP -> Guide x -> (LogP -> x -> Comp es b) -> Comp es b
  hop w (Guide (d :: d) (q :: q) α) k = do
        r <- call random
        let x = draw q r
        k (w + logProb d x - logProb q x) x


-- | Compute and update the guide parameters using a likelihood-ratio-estimate E[δelbo] of the ELBO gradient
handleLRatio :: forall fs a. Handler GradUpdate fs a a
handleLRatio = handleWith 1 (const Val) hop where
  hop :: Int -> GradUpdate x -> (Int -> x -> Comp fs a) -> Comp fs a
  hop t (GradUpdate wgrads params) k =
    let δelbo       :: ΔGuides  = lratio (unzip wgrads)
        scaledGrads :: ΔGuides  = Trace.map (\(VecFor δλ) -> VecFor (1.0 *| δλ)) δelbo
        params'     :: Guides = Trace.intersectWithAdd params scaledGrads
    in  k (t + 1) params'

-- | Where ws = logP(X, Y) - logQ(X; λ)
--         δGs   = δ_λ logQ(X;λ)
lratio :: ([ΔGuides], [LogP]) -> ΔGuides
-- lratio = undefined
lratio (δGs, ws) = foldr (\(Some k@(Key α)) -> Trace.insert k (VecFor (estδELBO k))) Trace.empty vars
  where
    norm_c :: Double
    norm_c = 1/fromIntegral (length ws)

    vars :: [Some  Key]
    vars = (Trace.keys . head) δGs
    {- | Uniformly scale each iteration's gradient trace G^l by its corresponding (normalised) importance weight W_norm^l.
            F^{1:L} = W_norm^{1:L} * G^{1:L}
        where the normalised importance weight is defined via:
            log(W_norm^l) = log(W^l) + max(log(W^{1:L})) -}
    δFs :: [ΔGuides]
    δFs = zipWith (\logW -> Trace.map (\(VecFor δ) -> VecFor (exp logW *| δ))) (normaliseLogPs ws) δGs
    {- | Compute the ELBO gradient estimate for a random variable v's associated parameters:
            E[δelbo(v)] = sum (F_v^{1:L} - b_v * G_v^{1:L}) / L
        where the baseline is:
            b_v    = covar(F_v^{1:L}, G_v^{1:L}) / var(G_v^{1:L}) -}
    estδELBO :: forall d. (DiffDistribution d)
      => Key d                -- ^   v
      -> Vec (Arity d) Double -- ^   E[δelbo(v)]
    estδELBO v  =
      let δGv        = Prelude.map (unVecFor . fromJust . Trace.lookup v) δGs      -- G_v^{1:L}
          δFv        = Prelude.map (unVecFor . fromJust . Trace.lookup v) δFs      -- F_v^{1:L}
          baseline_v = Vec.covar δFv δGv |/| Vec.var δGv  -- b_v
          δELBOv     = zipWith (\δgv δfv -> δfv |-| (baseline_v |*| δgv)) δGv δFv
          δestELBOv  = ((*|) norm_c . foldr (|+|) (Vec.zeros (Proxy @(Arity d)))) δELBOv
      in  δestELBOv

-- | Scale all log-probabilities by setting the maximum probability to 1
normaliseLogPs :: [LogP] -> [LogP]
normaliseLogPs xs = if isInfinite c then Prelude.map (const (-1/0)) xs else Prelude.map (\x -> x - c) xs
  where c = maximum xs