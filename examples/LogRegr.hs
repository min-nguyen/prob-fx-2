{-# LANGUAGE RankNTypes #-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Redundant return" #-}

{- | A logistic regression model, modelling the probability of an event occurring or not.
-}

module LogRegr where

import Control.Monad ( foldM )
import Model ( bernoulli, MulModel, gamma', normal, normal' )
import Env ( Env(..), Observables, Observable, Assign ((:=)), (<:>), enil, (<#>), vnil, get)
import Sampler ( Sampler )
import Inference.MC.SIM as SIM ( simulateWith )
import Inference.MC.SSMH as SSMH ( ssmhWith )
import Inference.MC.LW as LW ( lwWith )

{- | Logistic regression environment.
     This type definition is for readability purposes and is not used anywhere.
-}
type LogRegrEnv =
    '[  "y" ':= Bool,   -- output
        "m" ':= Double, -- mean
        "c" ':= Double  -- intercept
     ]

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp((-1) * x))

{- | Logistic regression model.
-}
logRegr
 -- Specify the "observable variables" that may later be provided observed values
 :: (Observable env "y" Bool, Observables env '["m", "c"] Double)
 -- | MulModel inputs
 => [Double]
 -- | Event occurrences
 -> MulModel env rs [Bool]
logRegr xs = do
  -- Specify model parameter distributions
  {- Annotating with the observable variable #m lets us later provide observed
     values for m. -}
  m     <- normal 0 5 #m
  b     <- normal 0 1 #c
  {- One can use primed variants of distributions which don't require observable
     variables to be provided. This disables being able to later provide
     observed values to that variable. -}
  sigma <- gamma' 1 1
  -- Specify model output distributions
  ys    <- mapM (\x -> do
                    -- probability of event occurring
                    p <- normal' (m * x + b) sigma
                    -- generate as output whether the event occurs
                    y <- bernoulli (sigmoid p) #y
                    pure y) xs
  pure ys

-- | SIM from logistic regression
simLogRegr :: Int -> Sampler [(Double, Bool)]
simLogRegr n_datapoints = do
  -- First declare the model inputs
  let xs = map ((/ fromIntegral n_datapoints) . fromIntegral) [(-n_datapoints) .. n_datapoints]
  -- Define a model environment to simulateWith from, providing observed values for the model parameters
      env_in = (#y := []) <:> (#m := [8]) <:> (#c := [-3]) <:> enil
  -- Call simulateWith on logistic regression
  (ys, env_outs) <- SIM.simulateWith (logRegr xs) env_in
  pure (zip xs ys)

-- | Likelihood-weighting over logistic regression
lwLogRegr
  :: Int
  -> Int
  -> Sampler [((Double, Double), Double)]
lwLogRegr n_lwsteps n_datapoints = do
  -- Get values from simulating log regr
  (xs, ys) <- unzip <$> simLogRegr n_datapoints
  -- Define environment for inference, providing observed values for the model outputs
  let env_in = (#y := ys) <:> (#m := []) <:> (#c := []) <:> enil
  -- Run LW inference for n_lwsteps iterations
  lwTrace <- LW.lwWith n_lwsteps (logRegr xs) env_in
  let -- Get output of LW, extract sampled parameters for #m and #c, and pair with likelihood-weighting ps
      (env_outs, ps) = unzip lwTrace
      mus = concatMap (get #m) env_outs
      bs  = concatMap (get #c) env_outs
  pure $ zip (zip mus bs) ps

-- | Metropolis-Hastings inference over logistic regression
ssmhLogRegr
  :: Int
  -> Int
  -> Sampler ([Double], [Double])
ssmhLogRegr n_mhsteps n_datapoints = do
  -- Get values from simulating log regr
  (xs, ys) <- unzip <$> simLogRegr n_datapoints
  let -- Define an environment for inference, providing observed values for the model outputs
      env_in = (#y := ys) <:> (#m := []) <:> (#c := []) <:> enil
  -- Run SSMH inference for n_mhsteps iterations
  {- The agument (#m <#> #c <#> vnil) is optional for indicating interest in learning #m and #c in particular,
     causing other variables to not be resampled (unless necessary) during SSMH. -}
  env_outs <- SSMH.ssmhWith n_mhsteps (logRegr xs) env_in (#m <#> #c <#> vnil)
  -- Retrieve values sampled for #m and #c during SSMH
  let m_samples = concatMap (get #m) env_outs
      b_samples = concatMap (get #c) env_outs
  pure (m_samples, b_samples)

{- | Log regression model on individual data points.
-}
logRegrOnce :: forall rs env.
 (Observable env "y" Bool, Observables env '["m", "c"] Double) =>
 Double -> MulModel env rs Bool
logRegrOnce x = do
  m     <- normal 0 8 #m
  b     <- normal 0 3 #c
  sigma <- gamma' 1 1
  y <- normal' (m * x + b) sigma
  l <- bernoulli (sigmoid y) #y
  pure l

simLogRegrOnce :: Int -> Sampler [(Double, Bool)]
simLogRegrOnce n_datapoints = do
  let xs = map ((/ fromIntegral n_datapoints) . fromIntegral) [(-n_datapoints) .. n_datapoints]
      env_in = (#y := []) <:> (#m := [8]) <:> (#c := [-3]) <:> enil
  ys_envs <- mapM (\x -> SIM.simulateWith (logRegrOnce x) env_in) xs
  let ys = map fst ys_envs
  pure (zip xs ys)

lwLogRegrOnce :: Int -> Int ->  Sampler [((Double, Double), Double)]
lwLogRegrOnce n_samples n_datapoints = do
  xys <- simLogRegrOnce n_datapoints
  let xys' = [(x, env_in) | (x, y) <- xys, let env_in = (#y := [y]) <:> (#m := []) <:> (#c := []) <:> enil]
  lwTrace <- mapM (\(x, env_in) -> LW.lwWith n_samples (logRegrOnce  x) env_in) xys'
  let (env_outs, ps) = unzip $ concat lwTrace
      mus = concatMap (get #m) env_outs
      bs  = concatMap (get #c) env_outs
  pure $ zip (zip mus bs) ps

ssmhLogRegrOnce ::  Int -> Int ->  Sampler ([Double], [Double])
ssmhLogRegrOnce n_mhsteps n_datapoints = do
  xys <- simLogRegrOnce n_datapoints
  let xys' = [(x, env_in) | (x, y) <- xys, let env_in = (#y := [y]) <:> (#m := []) <:> (#c := []) <:> enil]
  mhTrace <- concat <$> mapM (\(x, y) -> SSMH.ssmhWith n_mhsteps (logRegrOnce x) y  (#m <#> #c <#> vnil)) xys'
  let mus = concatMap (get #m) mhTrace
      bs  = concatMap (get #c) mhTrace
  pure (mus, bs)
