{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

module Examples.BayesianNN where

import Control.Monad
import Model
import Env
import Sampler
import Examples.DataSets
import Inference.SIM as Simulate
import Inference.MH as MH
import Util

-- | Bayesian network
type NNEnv =
    '[  "yObs"     ':= Double,
        "weight"   ':= Double,
        "bias"     ':= Double,
        "sigma"    ':= Double
     ]

data NN = NN { biases  :: [Double],
               weights :: [Double],
               sigm    :: Double } deriving Show

dot :: [Double] -> [Double] -> Double
dot [] _ = 0
dot _ [] = 0
dot (x:xs) (y:ys) = x * y + dot xs ys

-- | Neural network formulation for linear regression
forwardNNLin :: NN -> Double -> Double
forwardNNLin (NN bs ws _) x =
  (ws `dot` map (x -) bs) / 20

likelihoodNNLin :: Observable env "yObs" Double => NN -> Double -> Model env es Double
likelihoodNNLin nn x = do
  let ySigma = sigm nn
      yMean  = forwardNNLin nn x
  normal yMean ySigma #yObs

priorNN :: (Observables env '["weight", "bias", "sigma"] Double)
  => Int -> Model env es NN
priorNN n_nodes = do
  bias   <- replicateM n_nodes (uniform 0 10 #bias)
  weight <- replicateM n_nodes (uniform (-10) 10 #weight)
  sigma  <- uniform 0.5 1.5 #sigma
  return $ NN bias weight sigma

nnLinModel :: (Observables env '["weight", "bias", "sigma", "yObs"] Double)
  => Int -> Double -> Model env es (Double, Double)
nnLinModel n x = do
  nn <- priorNN n
  y  <- likelihoodNNLin nn x
  return (x, y)

-- | Alternative neural network formulation using activation
forwardNNStep :: NN -> Double -> Double
forwardNNStep (NN bs ws _) x =
  ws `dot` map (activation . (x -)) bs
  where activation x = if x < 0 then 0 else 1

likelihoodNNStep :: Observable env "yObs" Double
 => NN -> Double -> Model env es Double
likelihoodNNStep nn x = do
  let ySigma = sigm nn
      yMean  = forwardNNStep nn x
  normal yMean ySigma #yObs

nnStepModel :: (Observables env '["weight", "bias", "sigma", "yObs"] Double)
 => Int -> Double -> Model env es (Double, Double)
nnStepModel n x = do
  nn <- priorNN n
  y <- likelihoodNNStep nn x
  return (x, y)

-- | Another neural network formulation

type NNLogEnv =
    '[  "yObs"     ':= Bool,
        "weight"   ':= Double
     ]

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp((-1) * x))

nnLogModel :: (Observable env "weight" Double, Observable env "yObs" Bool)
  => Int -> (Double, Double) -> Model env es ((Double, Double), Bool)
nnLogModel n_nodes (x, y)  = do
  let xs = [x, y]
  weightsA <- replicateM2 (length xs) n_nodes (normal 0 1 #weight)
  let outputA = map2 tanh (dotProd [xs] weightsA)
  weightsB <- replicateM2 n_nodes n_nodes (normal 0 1 #weight)
  let outputB = map2 tanh (dotProd outputA weightsB)
  weightsC <- replicateM2 n_nodes 1 (normal 0 1 #weight)
  let outputC =  sigmoid . head . head $ dotProd outputB weightsC
  label <- bernoulli outputC #yObs
  return ((x, y), label)
