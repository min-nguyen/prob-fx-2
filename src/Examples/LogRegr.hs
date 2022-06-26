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
    ( Observables,
      Observable(get),
      Assign((:=)),
      ObsVars(ONil),
      (⋮),
      eNil,
      (<:>) )
import Sampler
import Inference.SIM as SIM
import Inference.MH as MH
import Inference.LW as LW

-- | Logistic regression model
type LogRegrEnv =
    '[  "label" ':= Bool,   -- output
        "m"     ':= Double, -- mean
        "b"     ':= Double  -- interce[t]
     ]

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp((-1) * x))

logRegr :: forall rs env.
 (Observable env "label" Bool, Observables env '["m", "b"] Double) =>
 [Double] -> Model env rs [Bool]
logRegr xs = do
  -- Specify model parameter distributions
  m     <- normal 0 5 #m
  b     <- normal 0 1 #b
  sigma <- gamma' 1 1
  -- Specify model output distributions
  ls    <- foldM (\ls x -> do
                     y <- normal' (m * x + b) sigma
                     l <- bernoulli (sigmoid y) #label
                     pure (l:ls)) [] xs
  pure (reverse ls)

-- | Alternative logistic regression formulation, for  a single data point
logRegressionOne :: forall rs env.
 (Observable env "label" Bool, Observables env '["m", "b"] Double) =>
 Double -> Model env rs Bool
logRegressionOne x = do
  m     <- normal 0 5 #m
  b     <- normal 0 1 #b
  sigma <- gamma' 1 1
  y     <- normal' (m * x + b) sigma
  l     <- bernoulli (sigmoid y) #label
  pure l

-- | SIM from logistic regression
simLogRegr :: Sampler [(Double, Bool)]
simLogRegr = do
  -- First declare the model inputs
  let xs  = map (/50) [(-50) .. 50]
  -- Define a model environment to simulate from.
      env = (#label := []) <:> (#m := [2]) <:> (#b := [-0.15]) <:> eNil
  -- Call simulate on logistic regression
  (ys, envs) <- SIM.simulate (logRegr xs) env
  pure (zip xs ys)

-- | Likelihood-weighting over logistic regression
lwLogRegr :: Sampler [(Double, Double)]
lwLogRegr = do
  -- Get values from simulating log regr
  (xs, ys) <- unzip <$> simLogRegr
  -- Define environment for inference
  let env = (#label := ys) <:> (#m := []) <:> (#b := []) <:> eNil
  -- Run LW inference for 20000 iterations
  lwTrace <- LW.lwTopLevel 20000 (logRegr xs) env
  let -- Get output of LW, extract mu samples, and pair with likelihood-weighting ps
      (env_outs, ps) = unzip lwTrace
      mus = concatMap (get #m) env_outs
  pure $ zip mus ps

-- | Metropolis-Hastings inference over logistic regression
mhLogRegr :: Sampler [(Double, Double)]
mhLogRegr = do
  -- Get values from simulating log regr
  (xs, ys) <- unzip <$> simLogRegr
  let -- Define an environment for inference
      env = (#label := ys) <:> (#m := []) <:> (#b := []) <:> eNil
  -- Run MH inference for 20000 iterations; the ["m", "b"] is optional for indicating interest in learning #m and #b in particular, causing other variables to not be resampled (unless necessary) during MH.
  mhTrace <- MH.mhTopLevel 20000 (logRegr xs) env (#m ⋮ #b ⋮ONil)
  -- Retrieve values sampled for #m and #b during MH
  let m_samples = concatMap (get #m) mhTrace
      b_samples = concatMap (get #b) mhTrace
  pure (zip m_samples b_samples)