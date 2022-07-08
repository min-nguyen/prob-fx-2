{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}

module Examples.LogRegr where

import Control.Monad
import Model
import Env
import Sampler
import Inference.SIM as SIM
import Inference.MH as MH
import Inference.LW as LW

-- | Logistic regression environment
type LogRegrEnv =
    '[  "label" ':= Bool,   -- output
        "m"     ':= Double, -- mean
        "b"     ':= Double  -- intercept
     ]

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp((-1) * x))

-- | Log regression model on individual data points
logRegr :: forall rs env.
 (Observable env "label" Bool, Observables env '["m", "b"] Double) => 
 Double -> Model env rs Bool
logRegr x = do
  m     <- normal 0 8 #m   
  b     <- normal 0 3 #b     
  sigma <- gamma' 1 1       
  y <- normal' (m * x + b) sigma
  l <- bernoulli (sigmoid y) #label
  pure l

simLogRegr :: Int -> Sampler [(Double, Bool)]
simLogRegr n_datapoints = do
  let xs = map ((/ fromIntegral n_datapoints) . fromIntegral) [(-n_datapoints) .. n_datapoints]
      env = (#label := []) <:> (#m := [8]) <:> (#b := [-3]) <:> nil
  ys_envs <- mapM (\x -> SIM.simulate (logRegr x) env) xs
  let ys = map fst ys_envs
  pure (zip xs ys)

lwLogRegr :: Int -> Int ->  Sampler [((Double, Double), Double)]
lwLogRegr n_samples n_datapoints = do
  xys <- simLogRegrs n_datapoints
  let xys' = [(x, env) | (x, y) <- xys, let env = (#label := [y]) <:> (#m := []) <:> (#b := []) <:> nil]
  lwTrace <- mapM (LW.lw n_samples logRegr) xys'
  let (env_outs, ps) = unzip $ concat lwTrace
      mus = concatMap (get #m) env_outs
      bs  = concatMap (get #b) env_outs
  pure $ zip (zip mus bs) ps

mhLogRegr ::  Int -> Int ->  Sampler ([Double], [Double])
mhLogRegr n_mhsteps n_datapoints = do
  xys <- simLogRegrs n_datapoints
  let xys' = [(x, env) | (x, y) <- xys, let env = (#label := [y]) <:> (#m := []) <:> (#b := []) <:> nil]
  mhTrace <- concat <$> mapM (\xy -> MH.mh n_mhsteps logRegr xy ["m", "c"]) xys'
  let mus = concatMap (get #m) mhTrace
      bs  = concatMap (get #b) mhTrace
  pure (mus, bs)

--------

-- | Log regression model on many data points at once
logRegrs :: forall rs env.
 -- Specify the "observable variables" that may later be provided observed values
 (Observable env "label" Bool, Observables env '["m", "b"] Double) => 
 [Double] -> Model env rs [Bool]
logRegrs xs = do
  -- Specify model parameter distributions
  m     <- normal 0 8 #m    -- Annotating with the observable variable #m lets us later provide observed values for m
  b     <- normal 0 3 #b     
  sigma <- gamma' 1 1       -- One can use primed variants of distributions to disable later providing observed values to that variable
  -- Specify model output distributions
  ls    <- foldM (\ls x -> do
                     y <- normal' (m * x + b) sigma
                     l <- bernoulli (sigmoid y) #label
                     return (l:ls)) [] xs
  return (reverse ls)

-- | SIM from logistic regression
simLogRegrs :: Int -> Sampler [(Double, Bool)]
simLogRegrs n_datapoints = do
  -- First declare the model inputs
  let xs = map ((/ fromIntegral n_datapoints) . fromIntegral) [(-n_datapoints) .. n_datapoints]
  -- Define a model environment to simulate from, providing observed values for the model parameters
      env = (#label := []) <:> (#m := [8]) <:> (#b := [-3]) <:> nil
  -- Call simulate on logistic regression
  (ys, envs) <- SIM.simulate (logRegrs xs) env 
  return (zip xs ys)

-- | Likelihood-weighting over logistic regression
lwLogRegrs :: Int -> Int -> Sampler [((Double, Double), Double)]
lwLogRegrs n_samples n_datapoints = do
  -- Get values from simulating log regr
  (xs, ys) <- unzip <$> simLogRegrs n_datapoints
  -- Define environment for inference, providing observed values for the model outputs
  let env = (#label := ys) <:> (#m := []) <:> (#b := []) <:> nil
  -- Run LW inference for 20000 iterations
  lwTrace <- LW.lw n_samples logRegrs (xs, env)
  let -- Get output of LW, extract mu samples, and pair with likelihood-weighting ps
      (env_outs, ps) = unzip lwTrace
      mus = concatMap (get #m) env_outs
      bs  = concatMap (get #b) env_outs
  pure $ zip (zip mus bs) ps

-- | Metropolis-Hastings inference over logistic regression
mhLogRegrs :: Int -> Int -> Sampler ([Double], [Double])
mhLogRegrs n_mhsteps n_datapoints = do
  -- Get values from simulating log regr
  (xs, ys) <- unzip <$> simLogRegrs n_datapoints
  let -- Define an environment for inference, providing observed values for the model outputs
      env = (#label := ys) <:> (#m := []) <:> (#b := []) <:> nil
  -- Run MH inference for 20000 iterations; the ["m", "b"] is optional for indicating interest in learning #m and #b in particular, causing other variables to not be resampled (unless necessary) during MH.
  mhTrace <- MH.mh n_mhsteps logRegrs (xs, env) ["m", "b"]
  -- Retrieve values sampled for #m and #b during MH
  let m_samples = concatMap (get #m) mhTrace
      b_samples = concatMap (get #b) mhTrace
  pure (m_samples, b_samples)
