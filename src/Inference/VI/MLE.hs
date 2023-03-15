
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- | Maximum likelihood estimation in terms of VI that samples from the prior P(X) and assigns each simulation an
     importance weight P(Y | X; θ).
-}

module Inference.VI.MLE where

import Data.Maybe ( fromMaybe, fromJust )
import Data.Bifunctor ( Bifunctor(..) )
import Control.Monad ( replicateM, (>=>) )
import Effects.Dist
import Effects.EnvRW ( EnvRW, handleEnvRW )
import Effects.State ( modify, handleState, State )
import Env ( Env, Vars, ContainsVars, union, initEmpty, varsToStrs )
import LogP ( LogP(..), normaliseLogPs )
import Model
import PrimDist
import Comp
import           Sampler ( Sampler, handleIO )
import           Trace
import qualified Inference.MC.SIM as SIM
import           Inference.VI.VI
import Inference.MC.LW (likelihood)
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Data.Data (Proxy(..))
import Data.Some

mle :: forall env es a b. es ~ '[Sampler]
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> VIGuide env es a                      -- ^ guide Q(X; λ)
  -> VIModel env es b                      -- ^ model P(X, Y)
  -> Env env                            -- ^ model environment (containing only observed data Y)
  -> Sampler Guides                     -- ^ final parameters θ_T
mle num_timesteps num_samples guide model env = do
  -- | Set up a empty dummy guide Q to return the original input model environment
  λ_0 <- collectParams env guide
  -- | Run MLE for T optimisation steps
  (handleIO . handleNormGradDescent) $
      guidedLoop num_timesteps num_samples guide (execGuide env) model exec λ_0

-- | Return probability of 1
execGuide :: Env env -> Guides -> VIGuide env '[Sampler] a -> Sampler (((a, Env env), ΔGuides), LogP)
execGuide env params   =
  handleIO . fmap (,0) . SIM.defaultSample . defaultParam params .  handleEnvRW env

-- | Compute P(Y | X; θ)
exec :: Env env -> VIModel env '[Sampler] a -> Sampler (a, LogP)
exec env =
  handleIO . SIM.defaultSample . likelihood . fmap fst . handleEnvRW env

-- | Compute and update the guide parameters using a self-normalised importance weighted gradient estimate
handleNormGradDescent :: Comp (GradUpdate : fs) a -> Comp fs a
handleNormGradDescent = handleWith 0 (const Val) hop where
  hop :: Int -> GradUpdate x -> (Int -> x -> Comp fs a) -> Comp fs a
  hop t (GradUpdate  ws δGs params) k =
    let δelbos  = normalisingEstimator (δGs, ws)
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
