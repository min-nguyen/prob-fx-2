{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{- | An interface that allows models in ProbFX to be translated to probabilistic programs in MonadBayes.
-}

module Inference.MB
  (
    toMBayes
  , handleObs
  , handleSamp
  ) where

import Control.Monad.Bayes.Class as MB ( MonadCond(..), MonadInfer, MonadSample )
import Effects.Dist ( Sample(..), Observe(..), Dist(getObs, getPrimDist), handleDist )
import Effects.Lift ( handleLift, Lift, lift )
import Effects.ObsReader ( ObsReader, handleObsRead )
import Env ( Env )
import Model (Model(..))
import Numeric.Log ( Log(Exp) )
import PrimDist ( logProb, sampleBayes )
import Prog ( discharge, Prog(..), LastMember )
import Trace ( traceSamples, FromSTrace(..) )

-- | Translate a ProbFX model under a given model environment to a MonadBayes program.
toMBayes :: (FromSTrace env, MonadInfer m)
  -- | ProbFX model
  => Model env [ObsReader env, Dist, Lift m] a
  -- | Input model environment
  -> Env env
  -- | A monadic computation @m@ in MonadBayes that also returns an output model environment
  -> m (a, Env env)
toMBayes m env =
     fmap (fmap fromSTrace) . handleLift . handleSamp
   . handleObs . traceSamples . handleDist . handleObsRead env $ runModel m

-- | Handle @Observe@ operations by computing the log-probability and calling the @score@ method of the @MonadCond@ class
handleObs :: (MonadCond m, LastMember (Lift m) es)
  => Prog (Observe : es) a
  -> Prog es a
handleObs (Val x)  = Val x
handleObs (Op u k) = case discharge u of
  Left u' -> do
     Op u' (handleObs . k)
  Right (Observe d y _) ->
      do let p = logProb d y
         lift (MB.score (Exp p))
         handleObs (k y)

-- | Handle @Sample@ operations by calling the sampling methods of the @MonadSampl@e class
handleSamp :: (MonadSample m, LastMember (Lift m) es)
 => Prog (Sample : es) a
 -> Prog es a
handleSamp (Val x) = pure x
handleSamp (Op u k) = case discharge u of
  Left u' -> do
     Op u' (handleSamp  . k)
  Right (Sample d _) ->
      do y <- lift (sampleBayes d)
         handleSamp (k y)

-- | Alternative for handling Dist as the last effect directly into a monad
handleDistMB :: MonadInfer m => Prog '[Dist] a -> m a
handleDistMB (Val x)  = pure x
handleDistMB (Op u k) = case discharge u of
    Right d ->
      case getObs d of
          Just y  -> do let p = logProb (getPrimDist d) y
                        score (Exp p)
                        handleDistMB (k y)
          Nothing -> do y <- sampleBayes (getPrimDist d)
                        handleDistMB (k y)
    Left  u'  -> error "impossible; Dist must be the last effect"
