{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

{- | Maximum likelihood estimation that directly samples X from the posterior P(X | Y; θ) using some form
     of Bayesian inference (e.g. SMC) and assigns this as the sample's importance weight.
-}

module Inference.VI.Extra.MLE_MCMC where

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
import qualified Inference.MC.SIM as SIM
import qualified Inference.MC.SMC as SMC
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util
import qualified Inference.VI.VI as VI

mle :: forall env a b. (Show (Env env))
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> Model env [ObsRW env, Dist] a      -- ^ model Q ≈ P(X | Y)
  -> Env env                            -- ^ model environment (containing only observed data Y)
  -> Model env [ObsRW env, Dist] b      -- ^ model P(X, Y; θ)
  -> Sampler DTrace                     -- ^ final distributions P(θ_T)
mle num_timesteps num_samples model_q model_env model_p = do
  let model_q' :: Prog '[Observe, Sample] (a, Env env)
      model_q' = second (Env.union model_env) <$> handleCore model_env model_q
  -- | Collect initial proposal distributions
  proposals_0 <- collectParams model_p model_q'
  -- | Run BBVI for T optimisation steps
  handleLift (mleInternal num_timesteps num_samples model_q' model_p proposals_0)

mleInternal :: (HasSampler fs, Show (Env env))
  => Int                                    -- ^ number of optimisation steps (T)
  -> Int                                    -- ^ number of samples to estimate the gradient over (L)
  -> Prog [Observe, Sample] (a, Env env)    -- ^ model Q ≈ P(X | Y)
  -> Model env [ObsRW env, Dist] b          -- ^ model P(X, Y; θ)
  -> DTrace                                 -- ^ model initial parameters P(θ_0)
  -> Prog fs DTrace                         -- ^ final model parameters P(θ_T)
mleInternal num_timesteps num_samples model_q model_p params_0 =
  foldr (>=>) pure [mleStep t model_q smcHandler model_p | t <- [1 .. num_timesteps]] params_0
  where smcHandler = handleLift . SMC.smcInternal num_samples

{- | 1. For L iterations:
         a) Generate importance-weighted posterior samples X=x from the model Q(X),
              - the unnormalised log-weight: log(Q(X=x))
         b) Execute the model P(Y=y, X=x; θ) accumulating:
              - the unnormalised log-weight: log(P(Y=y, X=x; θ))
              - the gradient log-pdfs: δlog(P(Y=y, X=x; θ))
     2. Update the model parameters P(θ)
-}
mleStep :: (HasSampler fs, Show (Env env))
  => Int                                                        -- ^ time step index (t)
  -> ProbProg (a, Env env)                                      -- ^ model Q ≈ P(X | Y)
  -> (ProbProg (a, Env env) -> Sampler [((a, Env env), LogP)])  -- ^ X ~ Q
  -> Model env [ObsRW env, Dist] b                              -- ^ model P(X, Y; θ)
  -> DTrace                                                     -- ^ model parameters P(θ_t)
  -> Prog fs DTrace                                             -- ^ next model parameters P(θ_{t+1})
mleStep timestep model_q hdlPosterior model_p params = do
  -- | Sample the model P(X | Y) for (L) iterations
  ((_, model_envs), logWs) <- Util.unzip3 <$> lift (hdlPosterior model_q)
  -- | Execute the model P(X, Y; θ) under each posterior sample
  ((_, _) , grads)         <- Util.unzip3 <$> mapM (lift . flip (handleModel model_p) params) model_envs

  -- | Compute the ELBO gradient estimates
  let δelbos     = VI.normalisingEstimator logWs grads
  -- | Update the parameters of the proposal distributions Q
  let params'    = case δelbos of Just δelbos -> VI.gradStep 1 params δelbos
                                  Nothing     -> params
  pure params'

{- We have a model P(X, Y; θ) which we execute under an environment of latent variables X and observed values Y.
-}
handleModel :: forall env a. Model env [ObsRW env, Dist] a -> Env env -> DTrace -> Sampler ((a, Env env), GTrace)
handleModel model_p env params =
  let prog :: Prog '[Param, Observe, Sample] (a, Env env)
      prog = (installModelParams params . handleCore env) model_p

  in  (SIM.handleSamp . SIM.handleObs . handleModelParams) prog

{- | Collect all learnable distributions as the initial set of proposals.
-}
collectParams :: Model env '[ObsRW env, Dist] b -> ProbProg (a, Env env) -> Sampler DTrace
collectParams model_p model_q = do
  (_, env) <- (SIM.handleSamp . SIM.handleObs) model_q
  (SIM.handleSamp . SIM.handleObs . collectModelParams . installModelParams Trace.empty . handleCore env) model_p

collectModelParams :: Member Sample es => Prog (Param : es) a -> Prog es DTrace
collectModelParams = ((fst <$>) . handleModelParams) . loop Trace.empty where
  loop :: DTrace -> Prog (Param : es) a -> Prog (Param : es) DTrace
  loop params (Val _)   = pure params
  loop params (Op op k) = case prj op of
    Just (ParamS q α)   -> error "MLE.collectModelParams: Should not happen"
    Just (ParamO q x α) -> do let params' = Trace.insert (Key α) q params
                              Op op (loop params' . k)
    Nothing -> Op op (loop params . k)

{- For each observed value Y = y and latent variable X = x generated in the model environment,
   this gives rise to Observe p x:
    - If p is a fixed non-learnable distribution, then we retain:
         Observe p x representing P(X = x; θ) or P(Y = y; θ)
    - If p is a learnable distribution, then we assume we already have a proposal p' from a previous iteration (unless this is the very first iteration). We then replace with:
         ParamO p' x representing P(X = x; θ) or P(Y = y; θ)
-}
installModelParams :: Member Observe es => DTrace -> Prog es a -> Prog (Param : es) a
installModelParams proposals = loop where
  loop (Val a)   = pure a
  loop (Op op k) = case op of
    ObsPrj p xy α -> case isDifferentiable p of
        Nothing      -> Op (weaken op) (installModelParams proposals . k)
        Just Witness -> do let p' = fromMaybe p (Trace.lookup (Key α) proposals)
                           x' <- call (ParamO p' xy α)
                           (loop . k) x'
    _ -> Op (weaken op) (loop . k)

handleModelParams :: forall es a. Prog (Param : es) a -> Prog es (a, GTrace)
handleModelParams = loop Trace.empty where
  loop :: GTrace -> Prog (Param : es) a -> Prog es (a, GTrace)
  loop grads (Val a)   = pure (a, grads)
  loop grads (Op op k) = case discharge op of
    Right (ParamS (q :: d) α)   -> error "MLE.handleModelParams: Should not happen"
    Right (ParamO (q :: d) x α) -> do
         let grads' = Trace.insert @d (Key α) (gradLogProb q x) grads
         (loop grads' . k) x
    Left op' -> Op op' (loop grads . k)

{- | Compute the log probability over the model:
        log(P(X, Y; θ))
-}
weighModel :: forall es a. (Members [Param, Observe, Sample] es) => Prog es a -> Prog es (a, LogP)
weighModel = loop 0 where
  loop :: LogP -> Prog es a -> Prog es (a, LogP)
  loop logW (Val a)   = pure (a, logW)
  loop logW (Op op k) = case op of
      -- | Compute: log(P(X; θ)) or log(P(Y; θ)) for optimisable dists, where X or Y is provided in the model environment
      ParamOPrj p xy α -> Op op (\xy -> loop (logW + logProb p xy) $ k xy)
      -- | Compute: log(P(X; θ)) or log(P(Y; θ)) for non-differentiable dists, where X or Y is provided in the model environment
      ObsPrj p xy α    -> Op op (\xy -> loop (logW + logProb p xy) $ k xy)
      -- | Compute: log(P(X; θ)), where latent variable X is not provided in the model environment
      SampPrj p α     -> Op op (\x -> loop (logW + logProb p x) $ k x)
      _               -> Op op (loop logW . k)