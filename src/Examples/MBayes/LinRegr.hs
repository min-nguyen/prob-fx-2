
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
module Examples.MBayes.LinRegr where

import Effects.ObsReader
import Effects.Writer
import Model
import Inference.MBAYES
import Effects.Dist
import Effects.Lift
import Control.Monad
import Env
import Trace
import Util
import Control.Monad.Bayes.Class (MonadInfer)
import Control.Monad.Bayes.Weighted ( prior, runWeighted )
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced
import Numeric.Log

type LinRegrEnv =
  '[ "m" ':= Double,
     "c" ':= Double,
     "σ" ':= Double,
     "y" ':= Double
   ]

{-  Linear regression  -}
linRegr :: forall env rs .
  Observables env '["y", "m", "c", "σ"] Double =>
  [Double] -> Model env rs [Double]
linRegr xs = do
  m <- normal 0 3 #m
  c <- normal 0 5 #c
  σ <- uniform 1 3 #σ
  ys <- mapM (\x -> do
                    y <- normal (m * x + c) σ #y
                    return y) xs
  return ys

mbayesLinRegr :: (FromSTrace env, MonadInfer m, Observables env '["y", "m", "c", "σ"] Double) =>
 [Double] -> Env env -> m ([Double], Env env)
mbayesLinRegr xs = toMBayes (linRegr xs)

{- Executing Lin Regr -}

simLinRegr :: Int -> Int -> IO [([Double], Env LinRegrEnv)]
simLinRegr n_samples n_datapoints = do
  let xs  = [0 .. fromIntegral n_datapoints]
      env = (#m := [3.0]) <:> (#c := [0]) <:> (#σ := [1]) <:> (#y := []) <:> eNil
  sampleIO $ prior $ replicateM n_samples (mbayesLinRegr xs env)

lwLinRegr :: Int -> Int -> IO [(([Double], Env LinRegrEnv), Log Double)]
lwLinRegr n_datapoints n_samples = do
  let n_datapoints' = fromIntegral n_datapoints
      xs            = [0 .. n_datapoints']
      env           = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x | x <- xs]) <:> eNil
  sampleIO $ replicateM n_samples (runWeighted $ mbayesLinRegr xs env)

mhLinRegr :: Int -> Int -> IO [([Double], Env LinRegrEnv)]
mhLinRegr n_samples n_datapoints = do
  let n_datapoints' = fromIntegral n_datapoints
      xs            = [0 .. n_datapoints']
      env           = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x | x <- xs]) <:> eNil
  mhtrace <- sampleIO (prior $ mh n_samples (mbayesLinRegr xs env))
  let (outpts, envs) = unzip mhtrace
      mus = concatMap (get #m) envs
  print mus
  return mhtrace