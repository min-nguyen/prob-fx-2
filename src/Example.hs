
{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, DerivingStrategies, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, PolyKinds, UndecidableSuperClasses, TemplateHaskell, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, OverloadedLabels, UndecidableInstances, FunctionalDependencies, TypeFamilyDependencies #-}

module Example where

import ModelFreeT
import Dist
import Util

exampleModel :: (HasVar s "mu" Double) => Model s Double
exampleModel = do
  let r1 = 5
  x  <- normal' 5 0 mu
  let r2 = 4
  return (r1 + r2)

linearRegression :: (HasVar s "y" Double) =>
                    Double -> Double -> Double -> Model s Double
linearRegression μ σ x = do
  normal' (μ + x) σ y

transitionModel :: Double -> Int -> Model s Int
transitionModel transition_p x_prev = do
  dX <- boolToInt <$> bernoulli transition_p
  let x = x_prev + dX
  return (dX + x)
