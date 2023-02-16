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

type GuidedModel es a = Comp (Param : Observe : Sample : es) a

type GuidedModelHandler es a = ParamTrace -> GuidedModel es a -> Sampler ((a, GradTrace), LogP)

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

-- | Sample from each @Guide@ distribution, x ~ Q(X; λ), and record its grad-log-pdf, δlog(Q(X = x; λ)).
defaultGuide :: forall es a. Member Sample es => ParamTrace -> Handler Guide es a (a, GradTrace)
defaultGuide param = handleWith Trace.empty (\s a -> Val (a, s)) hop where
  hop :: GradTrace -> Guide x -> (GradTrace -> x -> Comp es b) -> Comp es b
  hop grads (Guide (d :: d) (q :: q) α) k = do
      let q' = fromMaybe q (Trace.lookup (Key α) param)
      x <- call (Sample q' α)
      k (Trace.insert @q (Key α) (gradLogProb q' x) grads) x
