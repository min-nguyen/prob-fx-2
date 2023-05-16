{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

{- | BBVI inference on a model and guide as separate programs.
-}

module Inference.Guided.MLE
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
mle :: forall es a. ()
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> GuidedModel '[Sampler] a      -- ^ guide Q(X; λ)
  -> Sampler Guides                 -- ^ final guide parameters λ_T
mle num_timesteps num_samples model = do
  λ_0 <- collectGuide model
  -- liftIO (print λ_0)
  (handleImpure . handleNormGradDescent)
    $ guidedLoop num_timesteps num_samples exec model λ_0

-- | Compute Q(X; λ)
exec :: Guides -> GuidedModel '[Sampler] a -> Sampler ((a, ΔGuides), LogP)
exec params = handleImpure . defaultGuide . defaultSample . likelihood . useGuides params

-- | Compute and update the guide parameters using a self-normalised importance weighted gradient estimate
handleNormGradDescent :: Comp (GradUpdate : fs) a -> Comp fs a
handleNormGradDescent = handleWith 0 (const Val) hop where
  hop :: Int -> GradUpdate x -> (Int -> x -> Comp fs a) -> Comp fs a
  hop t (GradUpdate wgrads params) k =
    let δelbos  = normalisingEstimator (unzip wgrads)
        params' = case δelbos of Just δelbos' -> gradStep 1 params δelbos'
                                 Nothing      -> params
    in  k (t + 1) params'

normalisingEstimator :: ([ΔGuides], [LogP]) -> Maybe ΔGuides
normalisingEstimator (δGs, ws) = δelbos
  where
    {- | Store the gradient estimates for each variable v. -}
    δelbos :: Maybe ΔGuides
    δelbos = if isInfinite norm_c
              then Nothing
              else Just (foldr (\(Some k@(Key v)) -> estimateGrad k) Trace.empty vars)
    {- | Normalising constant -}
    norm_c :: Double
    norm_c = 1 / (fromIntegral (length ws) * sum (Prelude.map exp ws))
    {- | Optimisable variables -}
    vars :: [Some Key]
    vars = (Trace.keys . head) δGs
    {- | Uniformly scale each iteration's gradient trace G^l by its corresponding unnormalised importance weight W_norm^l -}
    δFs :: [ΔGuides]
    δFs = zipWith (\logW -> Trace.map (\(VecFor δ) -> VecFor (exp logW *| δ))) ws δGs
    {- | Compute the mean gradient estimate for a random variable v's associated parameters -}
    estimateGrad :: forall d. (DiffDistribution d) => Key d -> ΔGuides -> ΔGuides
    estimateGrad v = let δFv      = Prelude.map (unVecFor . fromJust . Trace.lookup v) δFs
                         norm_δFv = ((*|) norm_c . foldr (|+|) (Vec.zeros (Proxy @(Arity d))) ) δFv
                     in  Trace.insert v (VecFor norm_δFv)
