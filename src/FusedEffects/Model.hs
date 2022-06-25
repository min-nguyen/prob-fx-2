{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}

module FusedEffects.Model where

import FusedEffects.Algebra
import FusedEffects.Sum
import FusedEffects.Dist
import FusedEffects.ObsReader
import Control.Monad.Bayes.Class
import Env
import PrimDist

newtype Model env es m v =
  Model { runModel :: (Has Dist es m, Has (ObsReader env) es m) => m v } deriving Functor

toMBayes :: (Algebra sig m, MonadInfer m) 
  => Env env -> Model env (ObsReader env :+: (Dist :+: sig)) (ObsReaderC env (DistC m)) a -> m (Env env, a)
toMBayes env m = runDistC (runObsReaderC (runModel m) env)

normal' :: Double -> Double -> Model env es m Double
normal' mu sigma = Model $ do
  send (Dist (NormalDist mu sigma) Nothing Nothing)

normal :: forall env es x m. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es m Double
normal mu sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (Dist (NormalDist mu sigma) maybe_y tag)