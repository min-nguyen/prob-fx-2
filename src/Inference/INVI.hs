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
  -> Model env [ObsRW env, Dist] a      -- ^ model P(X, Y)
  -> Env env                            -- ^ model environment (containing only observed data Y)
  -> Model env [ObsRW env, Dist] b      -- ^ guide Q(X; λ)
  -> Sampler DTrace                     -- ^ final proposal distributions Q(λ_T)
invi num_timesteps num_samples model model_env guide_model = do
  let model' :: Prog '[Observe, Sample] (a, Env env)
      model' = handleCore model_env model
  -- | Collect initial proposal distributions
  params_0 <- collectParams guide_model model'
  -- | Run BBVI for T optimisation steps
  handleLift (inviInternal num_timesteps num_samples model' guide_model params_0)

inviInternal :: (LastMember (Lift Sampler) fs, Show (Env env))
  => Int                                    -- ^ number of optimisation steps (T)
  -> Int                                    -- ^ number of samples to estimate the gradient over (L)
  -> Prog [Observe, Sample] (a, Env env)    -- ^ model P(X, Y)
  -> Model env [ObsRW env, Dist] b          -- ^ guide Q(X; λ)
  -> DTrace                                 -- ^ initial guide parameters λ_0
  -> Prog fs DTrace                         -- ^ final guide parameters λ_T
inviInternal num_timesteps num_samples model guide params_0 = do
  foldr (>=>) pure [inviStep t num_samples model guide | t <- [1 .. num_timesteps]] params_0

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
  -> Prog [Observe, Sample] (a, Env env)          -- ^ model P(X, Y)
  -> Model env [ObsRW env, Dist] b                -- ^ guide Q(X; λ)
  -> DTrace                                       -- ^ guide parameters λ_t
  -> Prog fs DTrace                               -- ^ next guide parameters λ_{t+1}
inviStep timestep num_samples model guide params = do
  -- | Execute the model for (L) iterations
  ((_, model_envs), model_logWs)
      <- Util.unzip3 <$> replicateM num_samples ((lift . handleModel) model)
  -- | Execute the guide under each output model environment
  (((_, _)        , guide_logWs), grads)
      <- Util.unzip4 <$> mapM (lift . flip (handleGuide guide) params) model_envs
  -- | Compute unnormalised log-importance-weights: logW = log(P(X, Y)) - log(Q(X; λ))
  let logWs      = zipWith (-) model_logWs guide_logWs
  -- | Compute the ELBO gradient estimates
  let δelbos     = normalisingEstimator num_samples logWs grads
  -- | Update the parameters of the proposal distributions Q
  let params' = updateParams 1 params δelbos

  pure params'

{-
   We assume a guide model that *only* references latent variables X:
    `guide_model :: Model env [ObsReader env, Dist] a`
   Given an output model environment containing both latent variables X and observations Y, applying
   the handler `handleCore` to the guide_model will produce a guide program:
    `guide :: Prog [Observe, Sample] a` where:
      1. Observe q x α; the distribution q may or may not be a distribution that we can optimise,
                        depending on the distribution specified in the guide
                        the observed value x ~ q must always represents a latent variable X from the posterior P(X | Y)
      2. Sample q α   ; the distributions q are those we are not interested in optimising, but their generated
                        values x ~ q must correspond to latent variables X from P(X | Y)
-}
handleGuide :: forall env a. Model env [ObsRW env, Dist] a -> Env env -> DTrace -> Sampler (((a, Env env), LogP), GTrace)
handleGuide guide_model env params =
  let guide_prog :: Prog '[Learn, Observe, Sample] (a, Env env)
      guide_prog = (installLearn params . handleCore env) guide_model

  in  (SIM.handleSamp . SIM.handleObs . handleLearn . weighGuide) guide_prog

{- | Execute the model P under an environment of observed values Y, producing:
       1. An environment of latent variables X and observed values Y
       2. The total log-weight of all @Sample@ and @Observe@ operations: log(P(X, Y))
-}
handleModel :: ProbProg (a, Env env) -> Sampler ((a, Env env), LogP)
handleModel = SIM.handleSamp . SIM.handleObs . weighModel

{- | Collect all learnable distributions as the initial set of proposals.
-}
collectParams :: Model env '[ObsRW env, Dist] b -> ProbProg (a, Env env) -> Sampler DTrace
collectParams guide model = do
  ((_, env), _) <- handleModel model
  (BBVI.collectParams . installLearn Trace.empty . handleCore env) guide

{- For each latent variable X = x ~ P(X | Y) and Observe q x:
    - If q is a fixed non-learnable distribution, then we retain:
         Observe q x representing Q(X = x; λ)
    - If q is a learnable distribution, then we assume we already have a proposal q' from a previous iteration (unless this is the very first iteration). We then replace with:
         LearnO q' x  where representing Q(X = x; λ)
-}
installLearn :: Member Observe es => DTrace -> Prog es a -> Prog (Learn : es) a
installLearn params = loop where
  loop (Val a)   = pure a
  loop (Op op k) = case op of
    ObsPrj q x α -> case isDifferentiable q of
        Nothing      -> Op (weaken op) (installLearn params . k)
        Just Witness -> do let q' = fromMaybe q (Trace.lookup (Key α) params)
                           x' <- call (LearnO q' x α)
                           (loop . k) x'
    _ -> Op (weaken op) (loop . k)

handleLearn :: forall es a. Member Observe es => Prog (Learn : es) a -> Prog es (a, GTrace)
handleLearn = loop Trace.empty where
  loop :: GTrace -> Prog (Learn : es) a -> Prog es (a, GTrace)
  loop grads (Val a)   = pure (a, grads)
  loop grads (Op op k) = case discharge op of
    Right (LearnS (q :: d) α)   -> error "INVI.handleLearn: Should not happen"
    Right (LearnO (q :: d) x α) -> do
         let grads' = Trace.insert @d (Key α) (gradLogProb q x) grads
         (loop grads' . k) x
    Left op' -> Op op' (loop grads . k)

{- | Compute the log probability over the guide:
        log(Q(X; λ))
-}
weighGuide :: forall es a. (Members [Learn, Observe, Sample] es) => Prog es a -> Prog es (a, LogP)
weighGuide = loop 0 where
  loop :: LogP -> Prog es a -> Prog es (a, LogP)
  loop logW (Val a)   = pure (a, logW)
  loop logW (Op op k) = case  op of
      -- | Compute: log(Q(X; λ)) for proposal distributions
      LearnOPrj q x α ->
                        --  trace ("weighGuide: Learn " ++ show q )
                         Op op (\x -> loop (logW + logProb q x) $ k x)
      -- | Compute: log(Q(X; λ)) for non-differentiable distributions
      ObsPrj q x α    ->
                        --  trace ("weighGuide: Obs " ++ show q )
                         Op op (\x -> loop (logW + logProb q x) $ k x)
      SampPrj q α     ->
                        --  trace ("weighGuide: Samp " ++ show q )
                         Op op (\x -> loop (logW + logProb q x) $ k x)
      _               -> Op op (loop logW . k)

{- | Compute the log probability over the model:
        log(P(X, Y))
-}
weighModel :: forall es a. (Members [Sample, Observe] es) => Prog es a -> Prog es (a, LogP)
weighModel = loop 0 where
  loop :: LogP -> Prog es a -> Prog es (a, LogP)
  loop logW (Val a)   = pure (a, logW)
  loop logW (Op op k) = case op of
      -- | Compute: log(P(Y))
      ObsPrj d y α -> Op op (\x -> loop (logW + logProb d x) $ k x)
      -- | Compute: log(P(X))
      SampPrj d α  -> Op op (\x -> loop (logW + logProb d x) $ k x)
      _            -> Op op (loop logW . k)

normalisingEstimator :: Int -> [LogP] -> [GTrace] -> GTrace
normalisingEstimator l_samples logWs traceGs = trace (show norm_c) foldr f Trace.empty vars where
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
