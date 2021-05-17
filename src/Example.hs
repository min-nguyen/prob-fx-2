
{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, DerivingStrategies, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, PolyKinds, UndecidableSuperClasses, TemplateHaskell, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, OverloadedLabels, UndecidableInstances, FunctionalDependencies, TypeFamilyDependencies #-}

module Example where

import ModelFreeT
import Dist
import Util
import Control.Monad
import Data.Functor

-- Trivial example
exampleModel :: (HasVar s "mu" Double) => Model s Double
exampleModel = do
  let r1 = 5
  x  <- normal' 5 0 mu
  let r2 = 4
  return (r1 + r2)

-- Linear regression
linearRegression :: (HasVar s "y" Double) =>
  Double -> Double -> Double -> Model s Double
linearRegression μ σ x = do
  normal' (μ + x) σ y

-- Hidden markov model (with parameter y :: Int)
transitionModel :: Double -> Int -> Model s Int
transitionModel transition_p x_prev = do
  dX <- boolToInt <$> bernoulli transition_p Nothing
  let x = x_prev + dX
  return (dX + x)

observationModel :: (HasVar s "y" Int)
  => Double -> Int -> Model s Int
observationModel observation_p x = do
  binomial' x observation_p y

hmm :: (HasVar s "y" Int) 
  => Double -> Double -> Int -> Model s Int
hmm transition_p observation_p x_prev = do
  x_n <- transitionModel transition_p x_prev
  y_n <- observationModel observation_p x_n
  return x_n

hmmNSteps :: (HasVar s "y" Int) 
  => Double -> Double -> Int -> (Int -> Model s Int)
hmmNSteps transition_p observation_p n =
  foldl (>=>) return (replicate n (hmm transition_p observation_p))

-- Hidden markov model (with parameter ys :: [Int])
transitionModel' :: Double -> Int -> Model s Int
transitionModel' transition_p x_prev = do
  dX <- boolToInt <$> bernoulli transition_p Nothing
  let x_i = x_prev + dX
  return (dX + x_i)

observationModel' :: (HasVar s "ys" [Int])
  => Double -> Int -> Int -> Model s Int
observationModel' observation_p i x_i = do
  y_i <- access ys <&> fmap (!! i)
  binomial x_i observation_p y_i

hmm' :: (HasVar s "ys" [Int]) 
  => Double -> Double -> Int -> Int -> Model s Int
hmm' transition_p observation_p i x_prev = do
  x_i <- transitionModel' transition_p x_prev
  y_i <- observationModel' observation_p i x_i
  return x_i

hmmNSteps' :: (HasVar s "ys" [Int]) 
  => Double -> Double -> Int -> (Int -> Model s Int)
hmmNSteps' transition_p observation_p n =
  foldl (>=>) return 
    (replicate n (hmm' transition_p observation_p) <*> [0..])
