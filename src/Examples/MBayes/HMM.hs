
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use camelCase" #-}

module Examples.MBayes.HMM where

import Effects.ObsReader
import Effects.Writer
import Model
import Inference.MBAYES
import Effects.Dist
import Effects.Lift
import Control.Monad
import Env
import Util
import Control.Monad.Bayes.Class (MonadInfer)
import Control.Monad.Bayes.Weighted ( prior, runWeighted )
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced
import Numeric.Log

type HMMEnv =
  '[ "y"       ':= Int,
     "trans_p" ':= Double,
     "obs_p"   ':= Double
   ]

transitionModel ::  Double -> Int -> Model env ts Int
transitionModel transition_p x_prev = do
  dX <- boolToInt <$> bernoulli' transition_p
  return (dX + x_prev)

observationModel :: (Observable env "y" Int)
  => Double -> Int -> Model env ts Int
observationModel observation_p x = do
  binomial x observation_p #y

hmmNode :: (Observable env "y" Int)
  => Double -> Double -> Int -> Model env ts Int
hmmNode transition_p observation_p x_prev = do
  x_n <- transitionModel  transition_p x_prev
  y_n <- observationModel observation_p x_n
  return x_n

hmm :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double)
  => Int -> (Int -> Model env ts Int)
hmm n x = do
  trans_p <- uniform 0 1 #trans_p
  obs_p   <- uniform 0 1 #obs_p
  foldr (<=<) return  (replicate n (hmmNode trans_p obs_p)) x

mbayesHMM :: (MonadInfer m, Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double)
  => Int -> Int -> Env env -> m Int
mbayesHMM n x = toMBayes (hmm n x)

hmm_data :: [Int]
hmm_data = [0,1,1,3,4,5,5,5,6,5,6,8,8,9,7,8,9,8,10,10,7,8,10,9,10,10,14,14,14,15,14,15,14,17,17,17,16,17,14,15,16,18,17,19,20,20,20,22,23,22,23,25,21,21,23,25,24,26,28,23,25,23,27,28,28,25,28,29,28,24,27,28,28,32,32,32,33,31,33,34,32,31,33,36,37,39,36,36,32,38,38,38,38,37,40,38,38,39,40,42]

simHMM :: Int -> Int -> IO [Int]
simHMM n_samples n_steps = do
  let x   = 0
      env = (#y := []) <:> (#trans_p := [0.5]) <:> #obs_p := [0.9] <:>  eNil
  yss <- sampleIO $ prior $ replicateM n_samples $ mbayesHMM n_steps x env
  return yss

-- Note: running inference a Wasabaye model using Monad Bayes will only yield the return values of the model; also returning any sampled parameters of interest could be done by using a Writer effect in the model.
lwHMM :: Int -> Int -> IO [(Int, Log Double)]
lwHMM n_samples n_steps = do
  let x   = 0
      env = (#y := hmm_data) <:> (#trans_p := []) <:> #obs_p := [] <:>  eNil
  sampleIO $ replicateM n_samples $ runWeighted $ mbayesHMM n_steps x env

mhHMM :: Int -> Int -> IO [Int]
mhHMM n_samples n_steps = do
  let x   = 0
      env = (#y := hmm_data) <:> (#trans_p := []) <:> #obs_p := [] <:>  eNil
  sampleIO $ prior $ mh n_samples (mbayesHMM n_steps x env)