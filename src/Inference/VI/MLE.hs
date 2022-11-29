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
{-# LANGUAGE TupleSections #-}

{- | Maximum likelihood estimation in terms of VI that samples from the prior P(X) and assigns each simulation an
     importance weight P(Y | X; θ).
-}

module Inference.VI.MLE where

import Data.Maybe ( fromMaybe )
import Data.Bifunctor ( Bifunctor(..) )
import Control.Monad ( replicateM, (>=>) )
import Effects.Dist
import Effects.Lift
import Effects.ObsRW ( ObsRW )
import Effects.State ( modify, handleState, State )
import Env ( Env, Vars, ContainsVars, union, empty, varsToStrs )
import LogP ( LogP(..), normaliseLogPs, expLogP )
import Model
import PrimDist
import Prog ( discharge, Prog(..), call, weaken, LastMember, Member (..), Members, weakenProg )
import           Sampler ( Sampler )
import           Trace (GTrace, DTrace, Key(..), Some(..))
import qualified Trace
import qualified Inference.MC.SIM as SIM
import qualified Inference.VI.BBVI as BBVI
import qualified Inference.VI.VI as VI

mle :: forall env xs a b. (Show (Env env), (env `ContainsVars` xs))
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> Model env [ObsRW env, Dist] a      -- ^ model P(X, Y; θ)
  -> Env env                            -- ^ model environment (containing only observed data Y)
  -> Vars xs                            -- ^ parameter names θ
  -> Sampler DTrace                     -- ^ final parameters θ_T
mle num_timesteps num_samples model model_env vars = do
  -- | Set up a empty dummy guide Q to return the original input model environment
  let guide :: Prog '[Param, Sample] ((), Env env)
      guide = pure ((), model_env)
      guideParams_0 :: DTrace
      guideParams_0 = Trace.empty
  -- | Collect initial model parameters θ
  let tags = Env.varsToStrs @env vars
  modelParams_0 <- collectModelParams tags (handleCore model_env model)
  -- | Run MLE for T optimisation steps
  ((snd <$>) . handleLift . VI.handleNormGradDescent) $
      VI.viLoop num_timesteps num_samples guide handleGuide model (handleModel tags) (guideParams_0, modelParams_0)

-- | Handle the dummy guide Q by returning the original model environment Y and log-importance-weight log(Q(X)) = 0
handleGuide :: Prog [Param, Sample] a -> DTrace -> Sampler ((a, LogP), GTrace)
handleGuide guide _ =
  (SIM.handleSamp . VI.handleGuideParams ) ((, 0) <$> guide)

-- | Handle the model P(X, Y; θ) by returning log-importance-weight P(Y | X; θ)
handleModel :: [Tag] -> Model env [ObsRW env, Dist] a -> DTrace -> Env env -> Sampler (((a, Env env), LogP), GTrace)
handleModel tags model params env  =
  (SIM.handleSamp . SIM.handleObs . handleModelParams . weighModel . installModelParams tags params . handleCore env) model

collectModelParams :: [Tag] -> ProbProg b -> Sampler DTrace
collectModelParams tags =
  SIM.handleSamp . SIM.handleObs . (fst <$>) . handleModelParams . loop Trace.empty . installModelParams tags Trace.empty
  where
  loop :: DTrace -> Prog (Param : es) a -> Prog (Param : es) DTrace
  loop params (Val _)   = pure params
  loop params (Op op k) = case prj op of
    Just (ParamS q α)   -> do let params' = Trace.insert (Key α) q params
                              Op op (loop params' . k)
    Just (ParamO q x α) -> do let params' = Trace.insert (Key α) q params
                              Op op (loop params' . k)
    Nothing -> Op op (loop params . k)

installModelParams :: Members [Observe, Sample] es => [Tag] -> DTrace -> Prog es a -> Prog (Param : es) a
installModelParams tags proposals = loop where
  loop (Val a)   = pure a
  loop (Op op k) = case op of
    SampPrj p α -> case (isDifferentiable p, tag α `elem` tags) of
        (Just Witness, True) -> do
            let p' = fromMaybe p (Trace.lookup (Key α) proposals)
            x' <- call (ParamS p' α)
            (loop . k) x'
        _  -> Op (weaken op) (loop . k)
    ObsPrj p xy α -> case (isDifferentiable p, tag α `elem` tags)  of
        (Just Witness, True) -> do
            let p' = fromMaybe p (Trace.lookup (Key α) proposals)
            x' <- call (ParamO p' xy α)
            (loop . k) x'
        _      -> Op (weaken op) (loop . k)
    _ -> Op (weaken op) (loop . k)

handleModelParams :: forall es a. Member Sample es => Prog (Param : es) a -> Prog es (a, GTrace)
handleModelParams = loop Trace.empty where
  loop :: GTrace -> Prog (Param : es) a -> Prog es (a, GTrace)
  loop grads (Val a)   = pure (a, grads)
  loop grads (Op op k) = case discharge op of
    Right (ParamS (q :: d) α)   -> do
         x <- call (Sample q α)
         let grads' = Trace.insert @d (Key α) (gradLogProb q x) grads
         (loop grads' . k) x
    Right (ParamO (q :: d) x α) -> do
         let grads' = Trace.insert @d (Key α) (gradLogProb q x) grads
         (loop grads' . k) x
    Left op' -> Op op' (loop grads . k)

weighModel :: forall es a. (Members [Param, Observe, Sample] es) => Prog es a -> Prog es (a, LogP)
weighModel = loop 0 where
  loop :: LogP -> Prog es a -> Prog es (a, LogP)
  loop logW (Val a)   = pure (a, logW)
  loop logW (Op op k) = case op of
      -- | Compute: log(P(Y; θ)) for optimisable dists, where Y is provided in the model environment
      ParamOPrj p xy α -> Op op (\xy -> loop (logW + logProb p xy) $ k xy)
      -- | Compute: log(P(Y; θ)) for non-differentiable dists, where Y is provided in the model environment
      ObsPrj p xy α    -> Op op (\xy -> loop (logW + logProb p xy) $ k xy)
      _               -> Op op (loop logW . k)