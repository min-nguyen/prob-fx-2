
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE PolyKinds #-}


{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

{- | BBVI inference on a model and guide as separate programs.
-}

module Inference.VI.BBVI
  where

import           Data.Maybe
import           Data.Bifunctor ( Bifunctor(..) )
import           Debug.Trace
import           Control.Monad ( replicateM, (>=>) )
import           Effects.Dist

import           Effects.EnvRW ( EnvRW, handleEnvRW )
import           Effects.State ( modify, handleState, State )
import           Env ( Env, union )
import           LogP ( LogP(..), normaliseLogPs )
import           Model
import           PrimDist
import           Comp ( discharge, Comp(..), call, weaken, LastMember, Member (..), Members, weakenProg, Handler, handleSt )
import           Sampler ( Sampler, liftIO, handleIO )
import           Trace (GradTrace, ParamTrace, Key(..), Some(..))
import qualified Trace
import           Inference.MC.SIM as SIM
import           Inference.VI.VI as VI
-- import           Inference.VI.VI as VI (UpdateParam(..))
import qualified Vec
import           Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Inference.MC.LW
import Data.Data (Proxy(..))

{- | Top-level wrapper for BBVI inference that takes a separate model and guide.
-}
bbvi :: forall env es a b. es ~ '[Sampler]
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> VIGuide env es b      -- ^ guide Q(X; λ)
  -> VIModel env es a      -- ^ model P(X, Y)
  -> Env env                            -- ^ empty environment
  -> Sampler ParamTrace                 -- ^ final guide parameters λ_T
bbvi num_timesteps num_samples guide model env  = do
  λ_0 <- VI.collectParams env guide
  -- liftIO (print λ_0)
  (handleIO . handleLRatio)
    $ VI.viLoop num_timesteps num_samples guide (execGuide env) model exec λ_0

-- | Compute Q(X; λ)
execGuide :: Env env -> ParamTrace -> VIGuide env '[Sampler] a -> Sampler (((a, Env env), GradTrace), LogP)
execGuide env params =
  handleIO . VI.prior . defaultParam params . handleEnvRW env

-- | Compute P(X, Y)
exec :: Env env -> VIModel env '[Sampler] a -> Sampler (a, LogP)
exec env = handleIO . joint . fmap fst . handleEnvRW env where
  joint = fmap (\((x, a), b) -> (x, a + b)) . prior . likelihood


-- | Compute and update the guide parameters using a likelihood-ratio-estimate E[δelbo] of the ELBO gradient
handleLRatio :: forall fs a. Handler GradEst fs a a
handleLRatio = handleSt () (const Val) (const hop) where
  hop :: GradEst x -> (() -> x -> Comp fs a) -> Comp fs a
  hop (UpdateParam ws grads params) k =
    let δelbo = lratio ws grads
    in  k () (Trace.intersectLeftWith (\q δλ ->  q `safeAddGrad` (1 *| δλ)) params δelbo)

-- | Where logWs = logP(X, Y) - logQ(X; λ)
--         δGs   = δ_λ logQ(X;λ)
lratio :: [LogP] -> [GradTrace] -> GradTrace
lratio logWs δGs = foldr (\(Some v) -> Trace.insert v (estδELBO v)) Trace.empty vars
  where
    norm_c :: Double
    norm_c = 1/fromIntegral (length logWs)

    vars :: [Some DiffDistribution Key]
    vars = (Trace.keys . head) δGs
    {- | Uniformly scale each iteration's gradient trace G^l by its corresponding (normalised) importance weight W_norm^l.
            F^{1:L} = W_norm^{1:L} * G^{1:L}
        where the normalised importance weight is defined via:
            log(W_norm^l) = log(W^l) + max(log(W^{1:L})) -}
    δFs :: [GradTrace]
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
