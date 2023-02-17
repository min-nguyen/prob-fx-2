
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
import Effects.Dist
import Effects.Param
import Model
import Effects.EnvRW ( EnvRW, handleEnvRW )
import Effects.State ( modify, handleState, State )
import Env ( Env, union )
import LogP ( LogP(..), normaliseLogPs )
import PrimDist
import Comp ( discharge, Comp(..), call, weaken, LastMember, Member (..), Members, weakenProg, Handler, handleWith )
import Sampler
import           Trace (GradTrace, DistTrace, Key(..), Some(..), ValueTrace)
import qualified Trace
import Debug.Trace
import qualified Inference.MC.SIM as SIM
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util
import Inference.MC.LW (joint)

type VIGuide env es a  = Comp (EnvRW env : Param : Sample : es) a
type VIModel env es a  = Comp (EnvRW env : Observe : Sample : es) a

data GradEst a where
  UpdateParam :: [LogP] -> [GradTrace] -> DistTrace -> GradEst DistTrace

type GuideHandler env es a = DistTrace -> VIGuide env es a -> Sampler (((a, Env env), GradTrace), LogP)
type ModelHandler env es a = Env env    -> VIModel env es a -> Sampler (a, LogP)

guidedLoop :: (Members [GradEst, Sampler] fs)
  => Int                                     -- ^ number of optimisation steps (T)
  -> Int                                     -- ^ number of samples to estimate the gradient over (L)
  -> VIGuide env es1 a -> GuideHandler env es1 a
  -> VIModel env es2 b -> ModelHandler env es2 b
  -> DistTrace                             -- ^ guide parameters λ_t, model parameters θ_t
  -> Comp fs DistTrace      -- ^ final guide parameters λ_T
guidedLoop num_timesteps num_samples guide execg model execm guideParams_0 = do
  foldr (>=>) pure [guidedStep num_samples execg execm guide model  | t <- [1 .. num_timesteps]] guideParams_0

{- | 1. For L iterations,
        a) Generate values x from the guide Q(X; λ), accumulating:
            - the total guide log-weight:  log(Q(X = x; λ))
            - the gradient log-pdfs of the guide: δlog(Q(X = x; λ))
        b) Execute the model P using values x ~ Q(X; λ) and y, accumulating:
            - the total model log-weight:  log(P(X=x, Y=y))
     2. Compute an estimate of the ELBO gradient: E[δelbo]
     3. Update the parameters λ of the guide
-}

guidedStep ::  (Members [GradEst, Sampler] fs)
  => Int
  -> GuideHandler env es1 a -> ModelHandler env es2 b -> VIGuide env es1 a -> VIModel env es2 b
  -> DistTrace                            -- ^ guide parameters λ_t
  -> Comp fs DistTrace    -- ^ next guide parameters λ_{t+1}
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
  call (UpdateParam ws δλs params)

{- | Execute the guide Q under a provided set of proposal distributions Q(λ), producing:
      1. The output environment of latent variables X=x (given `env` contains X) generated by @Sample@s
         i.e. fixed non-differentiable dists, and @Param@s, i.e. learnable proposal dists:
            x ~ Q(X; λ),          where Q can be fixed or learnable
      2. The total log-weight of latent variables X=x, resulting from @Sample@s and @Param@s:
            log(Q(X=x; λ)),      where Q can be fixed or learnable
      3. The gradients of all proposal distributions Q(λ) at X=x:
            δlog(Q(X=x; λ)),     where Q is learnable
 -}

-- | Collect the parameters λ_0 of the guide's initial proposal distributions.
collectParams :: Env env -> VIGuide env '[Sampler] a -> Sampler DistTrace
collectParams env = handleIO . SIM.defaultSample . (fst <$>) . defaultParam Trace.empty . loop Trace.empty . handleEnvRW env
  where
  loop :: DistTrace -> Comp (Param : es) a -> Comp (Param : es) DistTrace
  loop params (Val _)   = pure params
  loop params (Op op k) = case prj op of
    Just (Param q α)   -> do let params' = Trace.insert (Key α) q params
                             Op op (loop params' . k)
    Nothing -> Op op (loop params . k)

-- | Sample from each @Param@ distribution, x ~ Q(X; λ), and record its grad-log-pdf, δlog(Q(X = x; λ)).
defaultParam :: forall es a. Member Sample es => DistTrace -> Handler Param es a (a, GradTrace)
defaultParam param = handleWith Trace.empty (\s a -> Val (a, s)) hop where
  hop :: GradTrace -> Param x -> (GradTrace -> x -> Comp es b) -> Comp es b
  hop grads (Param (q :: d) α) k = do
      let q' = fromMaybe q (Trace.lookup (Key α) param)
      x <- call (Sample q' α)
      k (Trace.insert @d (Key α) (gradLogProb q' x) grads) x

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
  -> DistTrace  -- ^ optimisable distributions Q(λ_t)
  -> GradTrace  -- ^ elbo gradient estimates   E[δelbo]
  -> DistTrace  -- ^ updated distributions     Q(λ_{t+1})
gradStep η = Trace.intersectLeftWith (\q δλ ->  q `safeAddGrad` (η *| δλ))
