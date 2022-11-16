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
import qualified Inference.VI as VI
import           Inference.VI as VI (GradDescent(..))


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
  let guide :: Prog '[Learn, Sample] (b, Env env)
      guide = (VI.installLearn . SIM.handleObs . handleCore model_env) guide_model
  -- | Collect initial proposal distributions
  params_0 <- SIM.handleSamp $ VI.collectParams guide
  -- | Run BBVI for T optimisation steps
  (handleLift . handleGradDescent) (VI.viLoop num_timesteps num_samples guide model model_env params_0)

handleGradDescent :: Prog (GradDescent : fs) a -> Prog fs a
handleGradDescent (Val a) = pure a
handleGradDescent (Op op k) = case discharge op of
  Right (GradDescent logWs δGs params) ->
    let params' = case δelbos of Just δelbos' -> updateParams 1 params δelbos'
                                 Nothing      -> params
    in  handleGradDescent (k params')
    where
      δelbos =
        if isInfinite norm_c then Nothing else Just (foldr f Trace.empty vars) where
          vars :: [Some DiffDistribution Key]
          vars = (Trace.keys . head) δGs
          {- | Store the gradient estimate for a given variable v. -}
          f :: Some DiffDistribution Key -> GTrace -> GTrace
          f (Some kx) = Trace.insert kx (estimateGrad kx traceFs)
          {- | Uniformly scale each iteration's gradient trace G^l by its corresponding unnormalised importance weight W_norm^l -}
          traceFs :: [GTrace]
          traceFs = zipWith (\logW -> Trace.map (\_ δ -> expLogP logW *| δ)) logWs δGs
          {- | Normalising constant -}
          norm_c :: Double
          norm_c = 1 / (fromIntegral (length logWs) * sum (map expLogP logWs))
          {- | Compute the mean gradient estimate for a random variable v's associated parameters -}
          estimateGrad :: forall d. ( DiffDistribution d) => Key d -> [GTrace] -> Vec (Arity d) Double
          estimateGrad v traceFs =
            let traceFs_v  = map (fromJust . Trace.lookup v) traceFs
            in  ((*|) norm_c . foldr (|+|) (Vec.zeros (Proxy @(Arity d))) ) traceFs_v
  Left op' -> Op op' (handleGradDescent . k)