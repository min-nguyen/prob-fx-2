{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Inference.MBAYES where

import Control.Monad.Bayes.Class
import Effects.Dist
import Effects.Lift
import Effects.ObsReader
import Env
import Model (Model(..))
import Numeric.Log
import PrimDist
import Prog
import Trace

-- ||| Handle Obs and Sample separately, using the "Lift m" effect and a MTL approach to "m" 
toMBayes :: forall m env a. (FromSTrace env, MonadInfer m) => Model env [ObsReader env, Dist, Lift m] a -> Env env -> m (a, Env env)
toMBayes m env = 
   (fmap (fmap fromSTrace) . handleLift . handleSamp . handleObs . traceSamples . handleDist . handleObsRead env) (runModel m)

handleObs :: forall m es a. MonadCond m => LastMember (Lift m) es => Prog (Observe : es) a -> Prog es a
handleObs (Val x)  = Val x
handleObs (Op u k) = case discharge u of
  Left u' -> do
     Op u' (handleObs . k)
  Right (Observe d y _) ->
      do let p = logProb d y
         lift (score (Exp p))
         handleObs (k y)

handleSamp :: forall m es a. MonadSample m => LastMember (Lift m) es => Prog (Sample : es) a -> Prog es a
handleSamp (Val x) = pure x
handleSamp (Op u k) = case discharge u of
  Left u' -> do
     Op u' (handleSamp  . k)
  Right (Sample d _) ->
      do y <- lift (sampleBayes d)
         handleSamp (k y)

-- ||| Alternative for handling Dist as the last effect directly into a monad
handleDistMB :: forall m es a. MonadInfer m => Prog '[Dist] a -> m a
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
