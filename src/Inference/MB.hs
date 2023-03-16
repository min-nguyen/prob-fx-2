

{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

{- | An interface that allows models in ProbFX to be translated to probabilistic programs in MonadBayes.
-}

module Inference.MB
  ( handleMBayes
  , handleObs
  , handleSamp
  ) where

import Control.Monad.Bayes.Class as MB ( MonadCond(..), MonadInfer, MonadSample )
import Effects.MulDist ( Sample(..), Observe(..), MulDist(MulDist, getObs, getPrimDist), handleMulDist )
import Effects.EnvRW ( EnvRW )
import Env ( Env )
import Model (MulModel(..), conditionWith)
import Numeric.Log ( Log(Exp) )
import LogP ( LogP(LogP) )
import Dist ( logProb, sampleBayes )
import Comp ( discharge, Comp(..), LastMember )

-- | Translate a ProbFX model under a given model environment to a MonadBayes program
handleMBayes :: MonadInfer m
  -- | model
  => MulModel env [EnvRW env, MulDist, Lift m] a
  -- | input model environment
  -> Env env
  -- | a computation @m@ in MonadBayes that returns a result and an output model environment
  -> m (a, Env env)
handleMBayes model env_in =
   (handleIO . handleSamp . handleObs . conditionWith env_in) model

-- | Handle @Observe@ operations by computing the log-probability and calling the @score@ method of the @MonadCond@ class
handleObs :: (MonadCond m, LastMember (Lift m) es)
  => Comp (Observe : es) a
  -> Comp es a
handleObs (Val x)  = Val x
handleObs (Op u k) = case discharge u of
  Right (Observe d y _) ->
    do let LogP p = logProb d y
       lift (MB.score (Exp p))
       handleObs (k y)
  Left u' -> do
     Op u' (handleObs . k)

-- | Handle @Sample@ operations by calling the sampling methods of the @MonadSample@ class
handleSamp :: (MonadSample m, LastMember (Lift m) es)
 => Comp (Sample : es) a
 -> Comp es a
handleSamp (Val x) = pure x
handleSamp (Op u k) = case discharge u of
  Right (Sample d _) ->
    do y <- lift (sampleBayes d)
       handleSamp (k y)
  Left u' -> do
     Op u' (handleSamp  . k)

-- | Alternative for handling MulDist as the last effect directly into a monad
handleDistMB :: MonadInfer m => Comp '[MulDist] a -> m a
handleDistMB (Val x)  = pure x
handleDistMB (Op u k) = case discharge u of
  Right (MulDist d maybe_y _) ->
    case maybe_y of
      Just y  -> do let LogP p = logProb d y
                    score (Exp p)
                    handleDistMB (k y)
      Nothing -> do y <- sampleBayes d
                    handleDistMB (k y)
  Left  u'  -> error "impossible; MulDist must be the last effect"
