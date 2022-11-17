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

{- | Maximum likelihood estimation.
-}

module Inference.MLEVI where

import Data.Maybe
import Data.Proxy
import Data.Bifunctor ( Bifunctor(..) )
import Control.Monad ( replicateM, (>=>) )
import Effects.Dist
import Effects.Lift
import Effects.ObsRW ( ObsRW )
import Effects.State ( modify, handleState, State )
import Env ( Env, union, empty )
import LogP ( LogP(..), normaliseLogPs, expLogP )
import Model
import PrimDist
import Prog ( discharge, Prog(..), call, weaken, LastMember, Member (..), Members, weakenProg )
import Sampler
import           Trace (GTrace, DTrace, Key(..), Some(..))
import qualified Trace
import Debug.Trace
import qualified Inference.SIM as SIM
import qualified Inference.SMC as SMC
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util
import qualified Inference.BBVI as BBVI
import qualified Inference.INVI as INVI
import qualified Inference.VI as VI

mle :: forall env a b. (Show (Env env))
  => Int                                -- ^ number of optimisation steps (T)
  -> Int                                -- ^ number of samples to estimate the gradient over (L)
  -> Model env [ObsRW env, Dist] b      -- ^ guide Q(X; λ)
  -> Model env [ObsRW env, Dist] a      -- ^ model P(X, Y)
  -> Env env                            -- ^ model environment (containing only observed data Y)
  -> Sampler DTrace           -- ^ final proposal distributions Q(λ_T)
mle num_timesteps num_samples guide_model model model_env  = do
  let guide :: Prog '[Param, Sample] (b, Env env)
      guide = ((second Env.empty <$>) . BBVI.installGuideParams . handleCore model_env) guide_model
  -- | Collect initial proposal distributions
  guideParams_0 <- BBVI.collectGuideParams guide
  modelParams_0 <- collectModelParams (handleCore model_env model)
  -- | Run BBVI for T optimisation steps
  ((fst <$>) . handleLift . INVI.handleGradDescent) $
    VI.viLoop num_timesteps num_samples guide BBVI.handleGuide model BBVI.handleModel (guideParams_0, Trace.empty)

handleGuide :: Prog [Param, Sample] a -> DTrace -> Sampler ((a, LogP), GTrace)
handleGuide guide params =
  (SIM.handleSamp . BBVI.handleGuideParams ) ((, 0) <$> guide)

handleModel :: forall env a. Model env [ObsRW env, Dist] a -> Env env -> DTrace -> Sampler (((a, Env env), LogP), GTrace)
handleModel model env params =
  let prog :: Prog '[Param, Observe, Sample] (a, Env env)
      prog = (installModelParams params . handleCore env) model

  in  (SIM.handleSamp . SIM.handleObs . handleModelParams . weighModel) prog

collectModelParams ::  ProbProg b -> Sampler DTrace
collectModelParams =
  SIM.handleSamp . SIM.handleObs . (fst <$>) . handleModelParams . loop Trace.empty . installModelParams Trace.empty
  where
  loop :: DTrace -> Prog (Param : es) a -> Prog (Param : es) DTrace
  loop params (Val _)   = pure params
  loop params (Op op k) = case prj op of
    Just (ParamS q α)   -> do let params' = Trace.insert (Key α) q params
                              Op op (loop params' . k)
    Just (ParamO q x α) -> do let params' = Trace.insert (Key α) q params
                              Op op (loop params' . k)
    Nothing -> Op op (loop params . k)

installModelParams :: Members [Observe, Sample] es => DTrace -> Prog es a -> Prog (Param : es) a
installModelParams proposals = loop where
  loop (Val a)   = pure a
  loop (Op op k) = case op of
    SampPrj p α -> case isDifferentiable p of
        Nothing      -> Op (weaken op) (installModelParams proposals . k)
        Just Witness -> do let p' = fromMaybe p (Trace.lookup (Key α) proposals)
                           x' <- call (ParamS p' α)
                           (loop . k) x'
    ObsPrj p xy α -> case isDifferentiable p of
        Nothing      -> Op (weaken op) (installModelParams proposals . k)
        Just Witness -> do let p' = fromMaybe p (Trace.lookup (Key α) proposals)
                           x' <- call (ParamO p' xy α)
                           (loop . k) x'
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
      -- | Compute: log(P(X; θ)) or log(P(Y; θ)) for optimisable dists, where X or Y is provided in the model environment
      ParamOPrj p xy α -> Op op (\xy -> loop (logW + logProb p xy) $ k xy)
      -- | Compute: log(P(X; θ)) or log(P(Y; θ)) for non-differentiable dists, where X or Y is provided in the model environment
      ObsPrj p xy α    -> Op op (\xy -> loop (logW + logProb p xy) $ k xy)
      _               -> Op op (loop logW . k)
