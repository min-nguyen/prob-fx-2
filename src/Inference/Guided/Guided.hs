{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}

{- | BBVI inference on a model and guide as separate programs.
-}

module Inference.Guided.Guided where

import Data.Maybe
import Data.Proxy
import Data.Bifunctor ( Bifunctor(..) )
import Control.Monad ( replicateM, (>=>), mapAndUnzipM )
import Effects.Dist
import Model
import Effects.EnvRW ( EnvRW, handleEnvRW )
import Effects.State ( modify, handleState, State )
import Effects.Guide
import Env ( Env, union )
import LogP ( LogP(..), normaliseLogPs )
import PrimDist
import Comp ( discharge, Comp(..), call, weaken, LastMember, Member (..), Members, weakenProg, Handler, handleWith )
import Sampler
import           Trace (GradTrace, ParamTrace, Key(..), Some(..), ValueTrace)
import qualified Trace
import Debug.Trace
import qualified Inference.MC.SIM as SIM
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util
import Inference.MC.LW (joint)


data GradEst a where
  UpdateParam :: [LogP] -> [GradTrace] -> ParamTrace -> GradEst ParamTrace

type GuidedModel es a = Comp (Guide : Observe : Sample : es) a

type GuidedModelHandler es a = ParamTrace -> GuidedModel es a -> Sampler ((a, GradTrace), LogP)

guidedLoop :: (Members [GradEst, Sampler] fs)
  => Int                                     -- ^ number of optimisation steps (T)
  -> Int                                     -- ^ number of samples to estimate the gradient over (L)
  -> GuidedModelHandler es a -> GuidedModel es a
  -> ParamTrace                             -- ^ guide parameters λ_t, model parameters θ_t
  -> Comp fs ParamTrace      -- ^ final guide parameters λ_T
guidedLoop n_timesteps n_samples exec model params = do
  foldr (>=>) pure [guidedStep n_samples exec model  | t <- [1 .. n_timesteps]] params

guidedStep ::  (Members [GradEst, Sampler] fs)
  => Int
  -> GuidedModelHandler es a -> GuidedModel es a
  -> ParamTrace                            -- ^ guide parameters λ_t
  -> Comp fs ParamTrace    -- ^ next guide parameters λ_{t+1}
guidedStep n_samples exec model params = do
  -- | Execute for L iterations
  ((a, δλs), ws) <- first unzip . unzip <$> replicateM n_samples (call $ exec params model)
  -- | Update the parameters λ of the proposal distributions Q
  call (UpdateParam ws δλs params)

-- | Collect the parameters λ_0 of the guide's initial proposal distributions.
collectGuide :: GuidedModel '[Sampler] a -> Sampler ParamTrace
collectGuide = (fst . fst <$>) . handleIO . SIM.defaultSample . SIM.defaultObserve . defaultGuide Trace.empty . loop Trace.empty
  where
  loop :: ParamTrace -> Comp (Guide : es) a -> Comp (Guide : es) ParamTrace
  loop params (Val _)   = pure params
  loop params (Op op k) = case prj op of
    Just (Guide d q α)   -> do let params' = Trace.insert (Key α) q params
                               Op op (loop params' . k)
    Nothing -> Op op (loop params . k)

-- | Sample from each @Guide@ distribution, x ~ Q(X; λ), and record its grad-log-pdf, δlog(Q(X = x; λ)).
defaultGuide :: forall es a. Member Sample es => ParamTrace -> Handler Guide es a ((a, GradTrace), LogP)
defaultGuide param = handleWith (Trace.empty, 0) (\(grads, w) a -> Val ((a, grads), w)) hop where
  hop :: (GradTrace, LogP) -> Guide x -> ((GradTrace, LogP) -> x -> Comp es b) -> Comp es b
  hop (grads, w) (Guide (d :: d) (q :: q) α) k = do
      let q' = fromMaybe q (Trace.lookup (Key α) param)
      x <- call (Sample q' α)
      let d_prior = logProb d  x
          q_prior = logProb q' x
      k (Trace.insert @q (Key α) (gradLogProb q' x) grads, w + d_prior - q_prior) x

prior :: forall es a. Member Sampler es => Handler Sample es a (a, LogP)
prior = handleWith 0 (\lρ x -> Val (x, lρ)) hop
  where
  hop :: LogP -> Sample x -> (LogP -> x -> Comp es b) -> Comp es b
  hop lρ (Sample d α) k = do x <- call $ drawWithSampler d
                             k (lρ + logProb d x) x

gradStep
  :: Double  -- ^ learning rate             η
  -> ParamTrace  -- ^ optimisable distributions Q(λ_t)
  -> GradTrace  -- ^ elbo gradient estimates   E[δelbo]
  -> ParamTrace  -- ^ updated distributions     Q(λ_{t+1})
gradStep η = Trace.intersectLeftWith (\q δλ ->  q `safeAddGrad` (η *| δλ))
