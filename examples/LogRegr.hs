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
{-# HLINT ignore "Redundant return" #-}

{- | A logistic regression model, modelling the probability of an event occurring or not.
-}

module LogRegr where

import Control.Monad ( foldM )
import Model ( bernoulli, Model, gamma', normal, normal' )
import Env ( Env(..), Observables, Observable, Assign ((:=)), (<:>), enil, (<#>), vnil, get)
import Sampler ( Sampler )
import Inference.MC.SIM as SIM ( simulate )
import Inference.MC.MH as MH ( mh )
import Inference.MC.LW as LW ( lw )

{- | Logistic regression environment.
     This type definition is for readability purposes and is not used anywhere.
-}
type LogRegrEnv =
    '[  "y" ':= Bool,   -- output
        "m"     ':= Double, -- mean
        "b"     ':= Double  -- intercept
     ]

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp((-1) * x))

{- | Logistic regression model.
-}
logRegr
 -- Specify the "observable variables" that may later be provided observed values
 :: (Observable env "y" Bool, Observables env '["m", "b"] Double)
 -- | Model inputs
 => [Double]
 -- | Event occurrences
 -> Model env rs [Bool]
logRegr xs = do
  -- Specify model parameter distributions
  {- Annotating with the observable variable #m lets us later provide observed
     values for m. -}
  m     <- normal 0 5 #m
  b     <- normal 0 1 #b
  {- One can use primed variants of distributions which don't require observable
     variables to be provided. This disables being able to later provide
     observed values to that variable. -}
  sigma <- gamma' 1 1
  -- Specify model output distributions
  ys    <- foldM (\ys x -> do
                    -- probability of event occurring
                    p <- normal' (m * x + b) sigma
                    -- generate as output whether the event occurs
                    y <- bernoulli (sigmoid p) #y
                    return (y:ys)) [] xs
  return (reverse ys)

-- | SIM from logistic regression
simLogRegr :: Int -> Sampler [(Double, Bool)]
simLogRegr n_datapoints = do
  -- First declare the model inputs
  let xs = map ((/ fromIntegral n_datapoints) . fromIntegral) [(-n_datapoints) .. n_datapoints]
  -- Define a model environment to simulate from, providing observed values for the model parameters
      env_in = (#y := []) <:> (#m := [8]) <:> (#b := [-3]) <:> enil
  -- Call simulate on logistic regression
  (ys, env_outs) <- SIM.simulate (logRegr xs) env_in
  return (zip xs ys)

-- | Likelihood-weighting over logistic regression
lwLogRegr
  :: Int
  -> Int
  -> Sampler [((Double, Double), Double)]
lwLogRegr n_lwsteps n_datapoints = do
  -- Get values from simulating log regr
  (xs, ys) <- unzip <$> simLogRegr n_datapoints
  -- Define environment for inference, providing observed values for the model outputs
  let env_in = (#y := ys) <:> (#m := []) <:> (#b := []) <:> enil
  -- Run LW inference for n_lwsteps iterations
  lwTrace <- LW.lw n_lwsteps (logRegr xs) env_in
  let -- Get output of LW, extract sampled parameters for #m and #b, and pair with likelihood-weighting ps
      (env_outs, ps) = unzip lwTrace
      mus = concatMap (get #m) env_outs
      bs  = concatMap (get #b) env_outs
  pure $ zip (zip mus bs) ps

-- | Metropolis-Hastings inference over logistic regression
mhLogRegr
  :: Int
  -> Int
  -> Sampler ([Double], [Double])
mhLogRegr n_mhsteps n_datapoints = do
  -- Get values from simulating log regr
  (xs, ys) <- unzip <$> simLogRegr n_datapoints
  let -- Define an environment for inference, providing observed values for the model outputs
      env_in = (#y := ys) <:> (#m := []) <:> (#b := []) <:> enil
  -- Run MH inference for n_mhsteps iterations
  {- The agument (#m <#> #b <#> vnil) is optional for indicating interest in learning #m and #b in particular,
     causing other variables to not be resampled (unless necessary) during MH. -}
  env_outs <- MH.mh n_mhsteps (logRegr xs) env_in (#m <#> #b <#> vnil)
  -- Retrieve values sampled for #m and #b during MH
  let m_samples = concatMap (get #m) env_outs
      b_samples = concatMap (get #b) env_outs
  pure (m_samples, b_samples)

{- | Log regression model on individual data points.
-}
logRegrOnce :: forall rs env.
 (Observable env "label" Bool, Observables env '["m", "b"] Double) =>
 Double -> Model env rs Bool
logRegrOnce x = do
  m     <- normal 0 8 #m
  b     <- normal 0 3 #b
  sigma <- gamma' 1 1
  y <- normal' (m * x + b) sigma
  l <- bernoulli (sigmoid y) #label
  pure l

simLogRegrOnce :: Int -> Sampler [(Double, Bool)]
simLogRegrOnce n_datapoints = do
  let xs = map ((/ fromIntegral n_datapoints) . fromIntegral) [(-n_datapoints) .. n_datapoints]
      env_in = (#label := []) <:> (#m := [8]) <:> (#b := [-3]) <:> enil
  ys_envs <- mapM (\x -> SIM.simulate (logRegrOnce x) env_in) xs
  let ys = map fst ys_envs
  pure (zip xs ys)

lwLogRegrOnce :: Int -> Int ->  Sampler [((Double, Double), Double)]
lwLogRegrOnce n_samples n_datapoints = do
  xys <- simLogRegrOnce n_datapoints
  let xys' = [(x, env_in) | (x, y) <- xys, let env_in = (#label := [y]) <:> (#m := []) <:> (#b := []) <:> enil]
  lwTrace <- mapM (\(x, env_in) -> LW.lw n_samples (logRegrOnce  x) env_in) xys'
  let (env_outs, ps) = unzip $ concat lwTrace
      mus = concatMap (get #m) env_outs
      bs  = concatMap (get #b) env_outs
  pure $ zip (zip mus bs) ps

mhLogRegrOnce ::  Int -> Int ->  Sampler ([Double], [Double])
mhLogRegrOnce n_mhsteps n_datapoints = do
  xys <- simLogRegrOnce n_datapoints
  let xys' = [(x, env_in) | (x, y) <- xys, let env_in = (#label := [y]) <:> (#m := []) <:> (#b := []) <:> enil]
  mhTrace <- concat <$> mapM (\(x, y) -> MH.mh n_mhsteps (logRegrOnce x) y  (#m <#> #b <#> vnil)) xys'
  let mus = concatMap (get #m) mhTrace
      bs  = concatMap (get #b) mhTrace
  pure (mus, bs)
