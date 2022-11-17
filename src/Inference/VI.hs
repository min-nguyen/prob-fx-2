{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}

{- | BBVI inference on a model and guide as separate programs.
-}

module Inference.VI
  where

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
import qualified Inference.SIM as SIM
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util

data GradDescent a where
  GradDescent :: [LogP] -> [GTrace] -> DTrace -> GradDescent DTrace

viLoop :: (LastMember (Lift Sampler) fs, Show (Env env))
  => Int                                          -- ^ number of optimisation steps (T)
  -> Int                                          -- ^ number of samples to estimate the gradient over (L)
  -> Prog [Learn, Sample] (b, Env env)            -- ^ guide Q(X; λ)
  -> Model env [ObsRW env, Dist] a                -- ^ model P(X, Y)
  -> Env env                                      -- ^ model environment (containing only observed data Y)
  -> DTrace                                       -- ^ initial guide parameters λ_0
  -> Prog (GradDescent : fs) DTrace               -- ^ final guide parameters λ_T
viLoop num_timesteps num_samples guide model model_env params_0 = do
  foldr (>=>) pure [viStep t num_samples guide model model_env  | t <- [1 .. num_timesteps]] params_0

{- | 1. For L iterations,
        a) Generate values x from the guide Q(X; λ), accumulating:
            - the total guide log-weight:  log(Q(X = x; λ))
            - the gradient log-pdfs of the guide: δlog(Q(X = x; λ))
        b) Execute the model P using values x ~ Q(X; λ) and y, accumulating:
            - the total model log-weight:  log(P(X=x, Y=y))
     2. Compute an estimate of the ELBO gradient: E[δelbo]
     3. Update the parameters λ of the guide
-}
viStep :: (LastMember (Lift Sampler) fs, Show (Env env))
  => Int                                          -- ^ time step index (t)
  -> Int                                          -- ^ number of samples to estimate the gradient over (L)
  -> Prog [Learn, Sample] (b, Env env)            -- ^ guide Q(X; λ)
  -> Model env [ObsRW env, Dist] a                -- ^ model P(X, Y)
  -> Env env                                      -- ^ model environment (containing only observed data Y)
  -> DTrace                                       -- ^ guide parameters λ_t
  -> Prog (GradDescent : fs) DTrace                               -- ^ next guide parameters λ_{t+1}
viStep timestep num_samples guide model model_env params = do
  -- | Execute the guide X ~ Q(X; λ) for (L) iterations
  (((_, guide_envs), guide_logWs), grads)
      <- Util.unzip4 <$> replicateM num_samples ((lift . handleGuide guide) params)
  -- | Execute the model P(X, Y) under the union of the model environment Y and guide environment X
  (_, model_logWs) <- Util.unzip3 <$> mapM (lift . handleModel model . Env.union model_env) guide_envs
  -- | Compute total log-importance-weight, log(P(X, Y)) - log(Q(X; λ))
  let logWs  = zipWith (-) model_logWs guide_logWs
  -- | Update the parameters λ of the proposal distributions Q
  params'    <- call (GradDescent logWs grads params)

  pure params'

{- | Execute the guide Q under a provided set of proposal distributions Q(λ), producing:
      1. The output environment of latent variables X=x (given `env` contains X) generated by @Sample@s
         i.e. fixed non-differentiable dists, and @Learn@s, i.e. learnable proposal dists:
            x ~ Q(X; λ),          where Q can be fixed or learnable
      2. The total log-weight of latent variables X=x, resulting from @Sample@s and @Learn@s:
            log(Q(X=x; λ)),      where Q can be fixed or learnable
      3. The gradients of all proposal distributions Q(λ) at X=x:
            δlog(Q(X=x; λ)),     where Q is learnable
 -}
handleGuide :: Prog [Learn, Sample] (a, Env env) -> DTrace -> Sampler (((a, Env env), LogP), GTrace)
handleGuide guide params =
  (SIM.handleSamp . handleLearn . weighGuide . updateLearn params) guide

{- | Execute the model P under an environment of samples X=x from the guide and observed values Y=y, producing:
       1. An output environment which we discard
       2. The total log-weight of all @Sample@ and @Observe@ operations: log(P(X=x, Y=y))
-}
handleModel :: Model env [ObsRW env, Dist] a -> Env env -> Sampler ((a, Env env), LogP)
handleModel model env =
  (SIM.handleSamp . SIM.handleObs . weighModel . handleCore env) model

{- | Collect all learnable distributions as the initial set of proposals λ_0.
-}
collectParams :: Member Sample es => Prog (Learn : es) a -> Prog es DTrace
collectParams = ((fst <$>) . handleLearn) . loop Trace.empty where
  loop :: DTrace -> Prog (Learn : es) a -> Prog (Learn : es) DTrace
  loop params (Val _)   = pure params
  loop params (Op op k) = case prj op of
    Just (LearnS q α)   -> do let params' = Trace.insert (Key α) q params
                              Op op (loop params' . k)
    Just (LearnO q x α) -> do let proposals' = Trace.insert (Key α) q params
                              Op op (loop proposals' . k)
    Nothing -> Op op (loop params . k)

{- | Replace all differentiable @Sample@ operations, representing the proposal distributions Q(λ),
     with @Learn@ operations.
-}
installLearn :: Member Sample es => Prog es a -> Prog (Learn : es) a
installLearn (Val x)   = pure x
installLearn (Op op k) = case prj op of
  Just (Sample q α) -> case isDifferentiable q of
      Nothing      -> Op (weaken op) (installLearn  . k)
      Just Witness -> do x <- call (LearnS q α)
                         (installLearn  . k) x
  Nothing -> Op (weaken op) (installLearn . k)

{- | Set the proposal distributions Q(λ) of @Learn@ operations.
-}
updateLearn :: forall es a. Member Learn es => DTrace -> Prog es a -> Prog es a
updateLearn proposals = loop where
  loop :: Prog es a -> Prog es a
  loop (Val a)   = pure a
  loop (Op op k) = case prj op of
    Just (LearnS q α) -> do
      let q' = fromMaybe q (Trace.lookup (Key α) proposals)
      x <- call (LearnS q' α)
      (loop . k) x
    Just (LearnO q x α) -> error "VI.update: Should not happen"
    Nothing -> Op op (loop . k)

{- | Handle each @Learn@ operation by sampling from its proposal distribution:
        x ~ Q(X; λ)
     And record the gradient log-pdf at sample x w.r.t parameters λ:
        δlog(Q(X = x; λ))
-}
handleLearn :: forall es a. Member Sample es => Prog (Learn : es) a -> Prog es (a, GTrace)
handleLearn = loop Trace.empty where
  loop :: GTrace -> Prog (Learn : es) a -> Prog es (a, GTrace)
  loop grads (Val a)   = pure (a, grads)
  loop grads (Op op k) = case discharge op of
    Right (LearnS (q :: d) α) -> do
      x <- call (Sample q α)
      let grads' = Trace.insert @d (Key α) (gradLogProb q x) grads
      (loop grads' . k) x
    Right (LearnO (q :: d) x α) ->
      trace "VI.handleLearn: Should not happen unless using collectProposals"
      loop grads (k x)
    Left op' -> Op op' (loop grads . k)

{- | Compute the log probability over the guide:
        log(Q(X; λ))
-}
weighGuide :: forall es a. (Members [Learn, Sample] es) => Prog es a -> Prog es (a, LogP)
weighGuide = loop 0 where
  loop :: LogP -> Prog es a -> Prog es (a, LogP)
  loop logW (Val a)   = pure (a, logW)
  loop logW (Op op k) = case  op of
      -- | Compute: log(Q(X; λ)) for proposal distributions
      LearnSPrj q α   -> Op op (\x -> loop (logW + logProb q x) $ k x)
      -- | Compute: log(Q(X; λ)) for non-differentiable distributions
      SampPrj q α     -> Op op (\x -> loop (logW + logProb q x) $ k x)
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

{- | Update each variable v's parameters λ using their estimated ELBO gradients E[δelbo(v)].
        λ_{t+1} = λ_t + η_t * E[δelbo(v)]
     where the gradient δelbo(v) is implicitly w.r.t λ
-}
updateParams
  :: Double  -- ^ learning rate             η
  -> DTrace  -- ^ optimisable distributions Q(λ_t)
  -> GTrace  -- ^ elbo gradient estimates   E[δelbo]
  -> DTrace  -- ^ updated distributions     Q(λ_{t+1})
updateParams η = Trace.intersectLeftWith (\q δλ ->  q `safeAddGrad` (η *| δλ))
