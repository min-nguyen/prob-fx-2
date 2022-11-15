{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{- | Maximum likelihood estimation.
-}

module Inference.MLE where

import Data.Maybe
import Data.Proxy
import Data.Bifunctor ( Bifunctor(..) )
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
import qualified Inference.SIM as SIM
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util
import qualified Inference.BBVI as BBVI
import qualified Inference.INVI as INVI

mle :: forall env a b. (Show (Env env))
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> Model env [ObsRW env, Dist] a      -- ^ model P(X | Y ; θ)
  -> Env env                            -- ^ model environment (containing only observed data Y)
  -> Model env [ObsRW env, Dist] b      -- ^ model P
  -> Sampler DTrace                     -- ^ final distributions P(λ_T)
mle num_timesteps num_samples model model_env model_p = do
  let model_q' :: Prog '[Observe, Sample] (a, Env env)
      model_q' = (second (Env.union model_env)) <$> handleCore model_env model_q
  -- | Collect initial proposal distributions
  proposals_0 <- collectParams model_p model_q'
  -- | Run BBVI for T optimisation steps
  handleLift (mleInternal num_timesteps num_samples model_q' model_p proposals_0)

mleInternal :: (LastMember (Lift Sampler) fs, Show (Env env))
  => Int                                    -- ^ number of optimisation steps (T)
  -> Int                                    -- ^ number of samples to estimate the gradient over (L)
  -> Prog [Observe, Sample] (a, Env env)    -- ^ model Q
  -> Model env [ObsRW env, Dist] b          -- ^ model P
  -> DTrace                                 -- ^ model initial parameters P(θ_0)
  -> Prog fs DTrace                         -- ^ final model parameters P(θ_T)
mleInternal num_timesteps num_samples model_q model_p params_0 = do
  foldr (>=>) pure [mleStep t num_samples model_q model_p | t <- [1 .. num_timesteps]] params_0

{- | 1. For L iterations:
         a) Generate importance-weighted posterior samples X=x from the model Q(X),
              - the unnormalised log-weight: log(Q(X=x))
         b) Execute the model P(Y=y, X=x; θ) accumulating:
              - the unnormalised log-weight: log(P(Y=y, X=x; θ))
              - the gradient log-pdfs: δlog(P(Y=y, X=x; θ))
     2. Update the model parameters P(θ)
-}
mleStep :: (LastMember (Lift Sampler) fs, Show (Env env))
  => Int                                          -- ^ time step index (t)
  -> Int                                          -- ^ number of samples to estimate the gradient over (L)
  -> Prog [Observe, Sample] (a, Env env)          -- ^ model Q
  -> Model env [ObsRW env, Dist] b                -- ^ model P
  -> DTrace                                       -- ^ model parameters P(θ_t)
  -> Prog fs DTrace                               -- ^ next model parameters P(θ_{t+1})
mleStep timestep num_samples model_q model_p params = do
  -- | Sample the model Q(X) for (L) iterations
  ((_, model_envs), model_logWs)
      <- Util.unzip3 <$> replicateM num_samples ((lift . handleGuide) model_q)
  -- | Execute the model P(X, Y; θ) under each posterior sample
  (((_, _)        , guide_logWs), grads)
      <- Util.unzip4 <$> mapM (lift . flip (handleModel model_p) params) model_envs
  -- | Compute unnormalised log-importance-weights: logW = log(P(X, Y)) - log(Q(X; λ))
  let logWs      = guide_logWs -- zipWith (-)  model_logWs
  -- | Compute the ELBO gradient estimates
  let δelbos     = INVI.normalisingEstimator num_samples logWs grads
  -- | Update the parameters of the proposal distributions Q
  let params'    = BBVI.updateParams 1 params δelbos

  pure params'

{- We assume a model Q from which we can generate importance-weighted posterior samples.
-}
handleGuide :: ProbProg (a, Env env) -> Sampler ((a, Env env), LogP)
handleGuide model_q = (SIM.handleSamp . SIM.handleObs . weighGuide) model_q

{- | Compute the log probability over the model:
        log(P(X, Y))
-}
weighGuide :: forall es a. (Members [Sample, Observe] es) => Prog es a -> Prog es (a, LogP)
weighGuide = loop 0 where
  loop :: LogP -> Prog es a -> Prog es (a, LogP)
  loop logW (Val a)   = pure (a, logW)
  loop logW (Op op k) = case op of
      -- | Compute: log(P(Y))
      ObsPrj d y α -> Op op (\x -> loop (logW + logProb d x) $ k x)
      -- | Compute: log(P(X))
      SampPrj d α  -> Op op (\x -> loop (logW + logProb d x) $ k x)
      _            -> Op op (loop logW . k)

{- We have a model P(X, Y; θ) which we execute under an environment of latent variables X and observed values Y.
-}
handleModel :: forall env a. Model env [ObsRW env, Dist] a -> Env env -> DTrace -> Sampler (((a, Env env), LogP), GTrace)
handleModel model_p env params =
  let guide_prog :: Prog '[Learn, Observe, Sample] (a, Env env)
      guide_prog = (installLearn params . handleCore env) model_p

  in  (SIM.handleSamp . SIM.handleObs . handleLearn . weighModel) guide_prog

{- | Collect all learnable distributions as the initial set of proposals.
-}
collectParams :: Model env '[ObsRW env, Dist] b -> ProbProg (a, Env env) -> Sampler DTrace
collectParams model_p model_q = do
  ((_, env), _) <- handleGuide model_q
  (BBVI.collectProposals . installLearn Trace.empty . handleCore env) model_p

{- For each observed value Y = y and latent variable X = x generated in the model environment,
   this gives rise to Observe p x:
    - If p is a fixed non-learnable distribution, then we retain:
         Observe p x representing P(X = x; θ) or P(Y = y; θ)
    - If p is a learnable distribution, then we assume we already have a proposal p' from a previous iteration (unless this is the very first iteration). We then replace with:
         LearnO p' x representing P(X = x; θ) or P(Y = y; θ)
-}
installLearn :: Member Observe es => DTrace -> Prog es a -> Prog (Learn : es) a
installLearn proposals = loop where
  loop (Val a)   = pure a
  loop (Op op k) = case op of
    ObsPrj p xy α -> case isDifferentiable p of
        Nothing      -> Op (weaken op) (installLearn proposals . k)
        Just Witness -> do let p' = fromMaybe p (Trace.lookup (Key α) proposals)
                           x' <- call (LearnO p' xy α)
                           (loop . k) x'
    _ -> Op (weaken op) (loop . k)

handleLearn :: forall es a. Member Observe es => Prog (Learn : es) a -> Prog es (a, GTrace)
handleLearn = loop Trace.empty where
  loop :: GTrace -> Prog (Learn : es) a -> Prog es (a, GTrace)
  loop grads (Val a)   = pure (a, grads)
  loop grads (Op op k) = case discharge op of
    Right (LearnS (q :: d) α)   -> error "MLE.handleLearn: Should not happen"
    Right (LearnO (q :: d) x α) -> do
         let grads' = Trace.insert @d (Key α) (gradLogProb q x) grads
         (loop grads' . k) x
    Left op' -> Op op' (loop grads . k)

{- | Compute the log probability over the model:
        log(P(X, Y; θ))
-}
weighModel :: forall es a. (Members [Learn, Observe, Sample] es) => Prog es a -> Prog es (a, LogP)
weighModel = loop 0 where
  loop :: LogP -> Prog es a -> Prog es (a, LogP)
  loop logW (Val a)   = pure (a, logW)
  loop logW (Op op k) = case op of
      -- | Compute: log(P(X; θ)) or log(P(Y; θ)) for optimisable dists, where X or Y is provided in the model environment
      LearnOPrj p xy α -> Op op (\xy -> loop (logW + logProb p xy) $ k xy)
      -- | Compute: log(P(X; θ)) or log(P(Y; θ)) for non-differentiable dists, where X or Y is provided in the model environment
      ObsPrj p xy α    -> Op op (\xy -> loop (logW + logProb p xy) $ k xy)
      -- | Compute: log(P(X; θ)), where latent variable X is not provided in the model environment
      SampPrj p α     -> Op op (\x -> loop (logW + logProb p x) $ k x)
      _               -> Op op (loop logW . k)
