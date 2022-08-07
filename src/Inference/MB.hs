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
  ( toMBayes
  , handleObs
  , handleSamp
  ) where

import Control.Monad.Bayes.Class as MB ( MonadCond(..), MonadInfer, MonadSample )
import Effects.Dist ( Sample(..), Observe(..), Dist(getObs, getPrimDist), handleDist )
import Effects.Lift ( handleLift, Lift, lift )
import Effects.ObsRW
import Env ( Env )
import Model (Model(..), handleCore)
import Numeric.Log ( Log(Exp) )
import LogP
import PrimDist ( logProb, sampleBayes )
import Prog ( discharge, Prog(..), LastMember )
import Trace ( traceSamples, FromSTrace(..) )


-- | Translate a ProbFX model under a given model environment to a MonadBayes program
toMBayes :: MonadInfer m
  -- | model
  => Model env [ObsRW env, Dist, Lift m] a
  -- | input model environment
  -> Env env
  -- | a computation @m@ in MonadBayes that returns a result and an output model environment
  -> m (a, Env env)
toMBayes m env_in =
   (fmap fst . handleLift . handleSamp . handleObs . traceSamples . handleCore env_in) m

-- | Handle @Observe@ operations by computing the log-probability and calling the @score@ method of the @MonadCond@ class
handleObs :: (MonadCond m, LastMember (Lift m) es)
  => Prog (Observe : es) a
  -> Prog es a
handleObs (Val x)  = Val x
handleObs (Op u k) = case discharge u of
  Left u' -> do
     Op u' (handleObs . k)
  Right (Observe d y _) ->
      do let LogP p = logProb d y
         lift (MB.score (Exp p))
         handleObs (k y)

-- | Handle @Sample@ operations by calling the sampling methods of the @MonadSample@ class
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
          Just y  -> do let LogP p = logProb (getPrimDist d) y
                        score (Exp p)
                        handleDistMB (k y)
          Nothing -> do y <- sampleBayes (getPrimDist d)
                        handleDistMB (k y)
    Left  u'  -> error "impossible; Dist must be the last effect"
