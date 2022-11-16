{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{- | Inclusive KL Divergence Variational Inference.
-}

module Inference.INVI where

import Data.Maybe
import Data.Proxy
import Data.Bifunctor ( Bifunctor(first) )
import Control.Monad ( replicateM, (>=>) )
import Effects.Dist
import Effects.Lift
import Effects.ObsRW ( ObsRW )
import Effects.State ( modify, handleState, State )
import Env ( Env, union )
import LogP ( LogP(..), normaliseLogPs, expLogP )
import Model
import PrimDist
import Prog ( discharge, Prog(..), call, weaken, LastMember, Member (..), Members, weakenProg )
import Sampler
import           Trace (GTrace, DTrace, Key(..), Some(..))
import qualified Trace
import Debug.Trace
import Inference.BBVI (updateParams)
import qualified Inference.SIM as SIM
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util
import qualified Inference.BBVI as BBVI


{- | Top-level wrapper that takes a separate model and guide.
-}
invi :: forall env a b. (Show (Env env))
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> Model env [ObsRW env, Dist] b      -- ^ guide Q(X; λ)
  -> Model env [ObsRW env, Dist] a      -- ^ model P(X, Y)
  -> Env env                            -- ^ model environment (containing only observed data Y)
  -> Sampler DTrace                     -- ^ final proposal distributions Q(λ_T)
invi num_timesteps num_samples guide_model model model_env  = do
  let guide :: Prog '[Learn, Observe, Sample] (b, Env env)
      guide = (BBVI.installLearn . handleCore model_env) guide_model
  -- | Collect initial proposal distributions
  params_0 <- BBVI.collectParams guide
  -- | Run BBVI for T optimisation steps
  handleLift (inviInternal num_timesteps num_samples guide model model_env params_0)

inviInternal :: (LastMember (Lift Sampler) fs, Show (Env env))
  => Int                                          -- ^ number of optimisation steps (T)
  -> Int                                          -- ^ number of samples to estimate the gradient over (L)
  -> Prog [Learn, Observe, Sample] (b, Env env)   -- ^ guide Q(X; λ)
  -> Model env [ObsRW env, Dist] a                -- ^ model P(X, Y)
  -> Env env                                      -- ^ model environment (containing only observed data Y)
  -> DTrace                                       -- ^ initial guide parameters λ_0
  -> Prog fs DTrace                               -- ^ final guide parameters λ_T
inviInternal num_timesteps num_samples guide model model_env  params_0 = do
  foldr (>=>) pure [inviStep t num_samples guide model model_env | t <- [1 .. num_timesteps]] params_0

{- | 1. For L iterations:
         a) Generate posterior samples X from the model P, accumulating:
              - the log model weight: log(P(X, Y))
         b) Execute guide Q under the environment of samples X ~ P, accumulating:
              - the log guide weight: log(Q(X; λ))
              - the gradient log-pdfs of the guide: δlog(Q(X; λ))
     2. Compute the normalised estimate of the gradient
     3. Update the proposal distributions Q(λ) of the guide
-}
inviStep :: (LastMember (Lift Sampler) fs, Show (Env env))
  => Int                                          -- ^ time step index (t)
  -> Int                                          -- ^ number of samples to estimate the gradient over (L)
  -> Prog [Learn, Observe, Sample] (b, Env env)   -- ^ guide Q(X; λ)
  -> Model env [ObsRW env, Dist] a                -- ^ model P(X, Y)
  -> Env env                                      -- ^ model environment (containing only observed data Y)
  -> DTrace                                       -- ^ guide parameters λ_t
  -> Prog fs DTrace                               -- ^ next guide parameters λ_{t+1}
inviStep timestep num_samples guide model model_env params = do
  -- | Execute the guide X ~ Q(X; λ) for (L) iterations
  (((_, guide_envs), guide_logWs), grads)
      <- Util.unzip4 <$> replicateM num_samples ((lift . BBVI.handleGuide guide) params)
  -- | Execute the model P(X, Y) under the union of the model environment Y and guide environment X
  (_               , model_logWs)
      <- Util.unzip3 <$> mapM ((lift . BBVI.handleModel model) . Env.union model_env) guide_envs
  -- | Compute total log-importance-weight, log(P(X, Y)) - log(Q(X; λ))
  let logWs      = zipWith (-) model_logWs guide_logWs
  -- | Compute the ELBO gradient estimates, E[δelbo]
  let δelbos     = normalisingEstimator num_samples logWs grads
  -- | Update the parameters λ of the proposal distributions Q
  let params'    = case δelbos of Just δelbos -> updateParams 1 params δelbos
                                  Nothing -> params

  pure params'

normalisingEstimator :: Int -> [LogP] -> [GTrace] -> Maybe GTrace
normalisingEstimator l_samples logWs traceGs = if (isInfinite norm_c) then Nothing else Just (foldr f Trace.empty vars) where
  vars :: [Some DiffDistribution Key]
  vars = (Trace.keys . head) traceGs
  {- | Store the gradient estimate for a given variable v. -}
  f :: Some DiffDistribution Key -> GTrace -> GTrace
  f (Some kx) = Trace.insert kx (estimateGrad kx traceFs)
  {- | Uniformly scale each iteration's gradient trace G^l by its corresponding unnormalised importance weight W_norm^l -}
  traceFs :: [GTrace]
  traceFs = zipWith (\logW -> Trace.map (\_ δ -> expLogP logW *| δ)) logWs traceGs
  {- | Normalising constant -}
  norm_c :: Double
  norm_c = 1 / (fromIntegral l_samples * sum (map expLogP logWs))
  {- | Compute the mean gradient estimate for a random variable v's associated parameters -}
  estimateGrad :: forall d. ( DiffDistribution d) => Key d -> [GTrace] -> Vec (Arity d) Double
  estimateGrad v traceFs =
    let traceFs_v  = map (fromJust . Trace.lookup v) traceFs
    in  ((*|) norm_c . foldr (|+|) (Vec.zeros (Proxy @(Arity d))) ) traceFs_v
