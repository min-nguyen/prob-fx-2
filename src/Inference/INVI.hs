{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{- | Inclusive KL Variational Inference.
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
import qualified Inference.SIM as SIM
import qualified Vec
import Vec (Vec, (|+|), (|-|), (|/|), (|*|), (*|))
import Util


handleGuide :: Model env [ObsRW env, Dist] a -> Env env -> DTrace -> Sampler (((a, Env env), LogP), GTrace)
handleGuide guide_model env = do
  let guide_prog = handleCore env guide_model

  undefined

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

{- For each latent variable X = x ~ P(X | Y) and Observe q x:
    - If q is a fixed non-learnable distribution, then we retain Observe q x representing Q(X = x; λ)
    - If q is a learnable distribution, then we assume we already have a proposal q' from a previous iteration (unless this is the very first iteration).
-}
installLearn :: Member Observe es => DTrace -> Prog es a -> Prog (Learn : es) a
installLearn proposals = loop where
  loop (Val a)   = pure a
  loop (Op op k) = case op of
    ObsPrj q x α -> case isDifferentiable q of
        Nothing      -> Op (weaken op) (installLearn proposals . k)
        Just Witness -> do let q' = fromMaybe q (Trace.lookup (Key α) proposals)
                           _ <- call (Observe q' x α)
                           _ <- call (Learn q' α)
                           (loop . k) x
    _ -> Op (weaken op) (loop . k)

{- | Compute the log probability over the guide:
        log(Q(X; λ))
-}
weighGuide :: forall es a. (Members [Learn, Sample] es) => Prog es a -> Prog es (a, LogP)
weighGuide = loop 0 where
  loop :: LogP -> Prog es a -> Prog es (a, LogP)
  loop logW (Val x)   = pure (x, logW)
  loop logW (Op op k) = case  op of
      -- | Compute: log(Q(X; λ)) for proposal distributions
      LearnPrj q α    -> Op op (\x -> loop (logW + logProb q x) $ k x)
      -- | Compute: log(Q(X; λ)) for non-differentiable distributions
      SampPrj d α     -> Op op (\x -> loop (logW + logProb d x) $ k x)
      _               -> Op op (loop logW . k)


handleModel :: ProbProg (a, Env env) -> Sampler ((a, Env env), LogP)
handleModel = SIM.handleSamp . SIM.handleObs . weighModel

weighModel :: forall es a. (Members [Sample, Observe] es) => Prog es a -> Prog es (a, LogP)
weighModel = loop 0 where
  loop :: LogP -> Prog es a -> Prog es (a, LogP)
  loop logW (Val x)   = pure (x, logW)
  loop logW (Op op k) = case op of
      -- | Compute: log(P(Y))
      ObsPrj d y α -> Op op (\x -> loop (logW + logProb d x) $ k x)
      -- | Compute: log(P(X))
      SampPrj d α  -> Op op (\x -> loop (logW + logProb d x) $ k x)
      _            -> Op op (loop logW . k)
