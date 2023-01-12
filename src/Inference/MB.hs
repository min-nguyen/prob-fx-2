

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
import Effects.Dist ( Sample(..), Observe(..), Dist(Dist, getObs, getPrimDist), handleDist )
import Effects.Lift ( handleM, Lift, lift )
import Effects.EnvRW ( EnvRW )
import Env ( Env )
import Model (Model(..), handleCore)
import Numeric.Log ( Log(Exp) )
import LogP ( LogP(LogP) )
import PrimDist ( logProb, sampleBayes )
import Prog ( discharge, Prog(..), LastMember )

-- | Translate a ProbFX model under a given model environment to a MonadBayes program
handleMBayes :: MonadInfer m
  -- | model
  => Model env [EnvRW env, Dist, Lift m] a
  -- | input model environment
  -> Env env
  -- | a computation @m@ in MonadBayes that returns a result and an output model environment
  -> m (a, Env env)
handleMBayes model env_in =
   (handleM . handleSamp . handleObs . handleCore env_in) model

-- | Handle @Observe@ operations by computing the log-probability and calling the @score@ method of the @MonadCond@ class
handleObs :: (MonadCond m, LastMember (Lift m) es)
  => Prog (Observe : es) a
  -> Prog es a
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
 => Prog (Sample : es) a
 -> Prog es a
handleSamp (Val x) = pure x
handleSamp (Op u k) = case discharge u of
  Right (Sample d _) ->
    do y <- lift (sampleBayes d)
       handleSamp (k y)
  Left u' -> do
     Op u' (handleSamp  . k)

-- | Alternative for handling Dist as the last effect directly into a monad
handleDistMB :: MonadInfer m => Prog '[Dist] a -> m a
handleDistMB (Val x)  = pure x
handleDistMB (Op u k) = case discharge u of
  Right (Dist d maybe_y _) ->
    case maybe_y of
      Just y  -> do let LogP p = logProb d y
                    score (Exp p)
                    handleDistMB (k y)
      Nothing -> do y <- sampleBayes d
                    handleDistMB (k y)
  Left  u'  -> error "impossible; Dist must be the last effect"
