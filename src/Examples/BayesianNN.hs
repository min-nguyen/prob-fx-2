{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Fuse mapM/map" #-}

module Examples.BayesianNN where

import Control.Monad
import Model
import Env
import Sampler
import Examples.DataSets
import Inference.SIM as SIM
import Inference.LW as LW
import Inference.MH as MH
import Util


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

-- | Bayesian linear regression network
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

simNNLin :: Sampler  [(Double, Double)]
simNNLin = do
  let env =  #yObs := [] <:> #weight := [1, 5, 8] <:> #bias := [2, -5, 1] <:> #sigma := [4.0] <:> ENil
  bs <- mapM (\x -> SIM.simulate (nnLinModel 3 x) env) (map (/1) [0 .. 300])
  return $ map fst bs
