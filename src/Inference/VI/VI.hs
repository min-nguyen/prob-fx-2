
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE PolyKinds #-}


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
import Control.Monad ( replicateM, (>=>), mapAndUnzipM )
import Effects.MulDist
import Effects.Param
import Model
import Effects.EnvRW ( EnvRW, handleEnvRW )
import Effects.State ( modify, handleState, State )
import Env ( Env, union )
import LogP ( LogP(..), normaliseLogPs )
import Dist
import Comp ( Comp(..), runImpure, call,  Member (..), Members, Handler, handleWith )
import Sampler
import           Trace
import Debug.Trace
import qualified Inference.MC.SIM as SIM
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util
import Inference.MC.LW (joint)

type VIGuide env es a  = Comp (EnvRW env : Param : Sample : es) a
type VIModel env es a  = Comp (EnvRW env : Observe : Sample : es) a

data GradUpdate a where
  GradUpdate :: [LogP] -> [ΔGuides] -> Guides -> GradUpdate Guides

type GuideHandler env es a = Guides -> VIGuide env es a -> Sampler (((a, Env env), ΔGuides), LogP)
type ModelExec env es a = Env env    -> VIModel env es a -> Sampler (a, LogP)

guidedLoop :: (Members [GradUpdate, Sampler] fs)
  => Int                                     -- ^ number of optimisation steps (T)
  -> Int                                     -- ^ number of samples to estimate the gradient over (L)
  -> VIGuide env es1 a -> GuideHandler env es1 a
  -> VIModel env es2 b -> ModelExec env es2 b
  -> Guides                             -- ^ guide parameters λ_t, model parameters θ_t
  -> Comp fs Guides      -- ^ final guide parameters λ_T
guidedLoop num_timesteps num_samples guide execg model execm guideParams_0 = do
  foldr (>=>) pure [guidedStep num_samples execg execm guide model  | t <- [1 .. num_timesteps]] guideParams_0

-- {- | 1. For L iterations,
--         a) Generate values x from the guide Q(X; λ), accumulating:
--             - the total guide log-weight:  log(Q(X = x; λ))
--             - the gradient log-pdfs of the guide: δlog(Q(X = x; λ))
--         b) Execute the model P using values x ~ Q(X; λ) and y, accumulating:
--             - the total model log-weight:  log(P(X=x, Y=y))
--      2. Compute an estimate of the ELBO gradient: E[δelbo]
--      3. Update the parameters λ of the guide
-- -}

guidedStep ::  (Members [GradUpdate, Sampler] fs)
  => Int
  -> GuideHandler env es1 a -> ModelExec env es2 b -> VIGuide env es1 a -> VIModel env es2 b
  -> Guides                            -- ^ guide parameters λ_t
  -> Comp fs Guides    -- ^ next guide parameters λ_{t+1}
guidedStep num_samples execg execm guide model  params = do
  let exec = do -- | Execute the guide X ~ Q(X; λ)
                (((_, env), δλ ), guide_w) <- call (execg params guide )
                -- | Execute the model P(X, Y) under the guide environment X
                (_        , model_w)      <- call (execm env model )
                -- | Compute total log-importance-weight, log(P(X, Y)) - log(Q(X; λ))
                let w     =  model_w - guide_w
                pure (δλ, w)
  -- | Execute for L iterations
  (δλs, ws) <- unzip <$> replicateM num_samples exec
  -- | Update the parameters λ of the proposal distributions Q
  call (GradUpdate ws δλs params)

-- {- | Execute the guide Q under a provided set of proposal distributions Q(λ), producing:
--       1. The output environment of latent variables X=x (given `env` contains X) generated by @Sample@s
--          i.e. fixed non-differentiable dists, and @Param@s, i.e. learnable proposal dists:
--             x ~ Q(X; λ),          where Q can be fixed or learnable
--       2. The total log-weight of latent variables X=x, resulting from @Sample@s and @Param@s:
--             log(Q(X=x; λ)),      where Q can be fixed or learnable
--       3. The gradients of all proposal distributions Q(λ) at X=x:
--             δlog(Q(X=x; λ)),     where Q is learnable
--  -}

-- | Collect the parameters λ_0 of the guide's initial proposal distributions.
collectParams :: Env env -> VIGuide env '[Sampler] a -> Sampler Guides
collectParams env = runImpure . SIM.defaultSample . (fst <$>) . defaultParam Trace.empty . loop Trace.empty . handleEnvRW env
  where
  loop :: Guides -> Comp (Param : es) a -> Comp (Param : es) Guides
  loop params (Val _)   = pure params
  loop params (Op op k) = case prj op of
    Just (Param (q :: q) α)   -> do
      let kα = Key α :: Key q
          params' = Trace.insert kα (Identity q) params
      Op op (loop params' . k)
    Nothing -> Op op (loop params . k)

-- | Sample from each @Param@ distribution, x ~ Q(X; λ), and record its grad-log-pdf, δlog(Q(X = x; λ)).
defaultParam :: forall es a. Member Sample es => Guides -> Handler Param es a (a, ΔGuides)
defaultParam param = handleWith Trace.empty (\s a -> Val (a, s)) hop where
  hop :: ΔGuides -> Param x -> (ΔGuides -> x -> Comp es b) -> Comp es b
  hop grads (Param (q :: q) α) k = do
      let kα = Key α :: Key q
          Identity q' = fromMaybe (Identity q) (Trace.lookup kα param)
      x <- call (Sample q' α)
      k (Trace.insert kα (VecFor $ gradLogProb q' x) grads) x

prior :: forall es a. Member Sampler es => Handler Sample es a (a, LogP)
prior = handleWith 0 (\lρ x -> Val (x, lρ)) hop
  where
  hop :: LogP -> Sample x -> (LogP -> x -> Comp es b) -> Comp es b
  hop lρ (Sample d α) k = do x <- call $ drawWithSampler d
                             k (lρ + logProb d x) x

{- | Update each variable v's parameters λ using their estimated ELBO gradients E[δelbo(v)].
        λ_{t+1} = λ_t + η_t * E[δelbo(v)]
     where the gradient δelbo(v) is implicitly w.r.t λ
-}
gradStep
  :: Double  -- ^ learning rate             η
  -> Guides  -- ^ optimisable distributions Q(λ_t)
  -> ΔGuides  -- ^ elbo gradient estimates   E[δelbo]
  -> Guides  -- ^ updated distributions     Q(λ_{t+1})
gradStep η guides grads =
  let scaledGrads = Trace.map (\(VecFor δλ) -> VecFor (η *| δλ)) grads
  in  Trace.intersectWithAdd guides scaledGrads