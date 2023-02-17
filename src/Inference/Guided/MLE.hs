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
import           Trace (GradTrace, DistTrace, Key(..), Some(..), ValueTrace)
import qualified Trace
import Inference.MC.LW (likelihood)
import PrimDist
import qualified Vec
import           Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Data.Data (Proxy(..))
import Comp
import Inference.MC.SIM
import Effects.Dist

{- | Top-level wrapper for BBVI inference that takes a separate model and guide.
-}
mle :: forall es a. ()
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> GuidedModel '[Sampler] a      -- ^ guide Q(X; λ)
  -> Sampler DistTrace                 -- ^ final guide parameters λ_T
mle num_timesteps num_samples model = do
  λ_0 <- collectGuide model
  -- liftIO (print λ_0)
  (handleIO . handleNormGradDescent)
    $ guidedLoop num_timesteps num_samples exec model λ_0

-- | Compute Q(X; λ)
exec :: DistTrace -> GuidedModel '[Sampler] a -> Sampler ((a, GradTrace), LogP)
exec params = handleIO . joint . handleGuide . updateGuide params . defaultSample . likelihood  where
  joint = fmap (\(((x, w_like), g), _, _) -> ((x, g), w_like))

-- | Compute and update the guide parameters using a self-normalised importance weighted gradient estimate
handleNormGradDescent :: Comp (GradEst : fs) a -> Comp fs a
handleNormGradDescent (Val a) = pure a
handleNormGradDescent (Op op k) = case discharge op of
  Right (UpdateParam logWs δGs params) ->
    let δelbos  = normalisingEstimator logWs δGs
        params' = case δelbos of Just δelbos' -> gradStep 1 params δelbos'
                                 Nothing      -> params
    in  handleNormGradDescent (k params')
  Left op' -> Op op' (handleNormGradDescent . k)

normalisingEstimator :: [LogP] -> [GradTrace] -> Maybe GradTrace
normalisingEstimator logWs δGs = δelbos
  where
    {- | Store the gradient estimates for each variable v. -}
    δelbos :: Maybe GradTrace
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
    δFs :: [GradTrace]
    δFs = zipWith (\logW -> Trace.map (\_ δ -> exp logW *| δ)) logWs δGs
    {- | Compute the mean gradient estimate for a random variable v's associated parameters -}
    estimateGrad :: forall d. (DiffDistribution d) => Key d -> GradTrace -> GradTrace
    estimateGrad v = let δFv      = map (fromJust . Trace.lookup v) δFs
                         norm_δFv = ((*|) norm_c . foldr (|+|) (Vec.zeros (Proxy @(Arity d))) ) δFv
                     in  Trace.insert v norm_δFv