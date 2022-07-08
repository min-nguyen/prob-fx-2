
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
module Tests.Examples.LinRegr where

import Prog
import Effects.ObsReader
import Effects.Writer
import Model
import Inference.SIM as SIM
import Inference.LW as LW
import Inference.MH as MH
import Effects.Dist
import Effects.Lift
import Sampler
import Control.Monad
import Data.Kind (Constraint)
import Env
import Util

-- | Linear regression environment
type LinRegrEnv =
  '[ "m" ':= Double,
     "c" ':= Double,
     "σ" ':= Double,
     "y" ':= Double
   ]

-- ||| (Section 1) Linear regression
linRegrOnce :: forall env rs . 
  Observables env '["y", "m", "c", "σ"] Double =>
  Double -> Model env rs Double
linRegrOnce x = do
  m <- normal 0 3 #m
  c <- normal 0 5 #c
  σ <- uniform 1 3 #σ
  y <- normal (m * x + c) σ #y
  pure y

-- ||| (Section 1, Fig 1a) SIM from linear regression
simLinRegrOnce :: Int -> Sampler [(Double, Double)]
simLinRegrOnce n_datapoints = do
  let xs  = [0 .. fromIntegral n_datapoints]
      env = (#m := [3.0]) <:> (#c := [0]) <:> (#σ := [1]) <:> (#y := []) <:> enil
  ys_envs <- mapM (\x -> SIM.simulate (linRegrOnce x) env) xs
  let ys = map fst ys_envs
  pure (zip xs ys)
 
-- ||| (Section 1, Fig 1b) Perform likelihood weighting over linear regression; returns sampled mu values and associated likelihood weightings
lwLinRegrOnce :: Int -> Int ->  Sampler [(Double, Double)]
lwLinRegrOnce n_samples n_datapoints = do
  let xs  = [0 .. fromIntegral n_datapoints]
      xys = [(x, env) | x <- xs, let env = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x]) <:> enil]
  lwTrace <- mapM (\(x, env) -> LW.lw n_samples (linRegrOnce  x) env) xys
  let -- Get output of LW and extract mu samples
      (env_outs, ps) = unzip $ concat lwTrace
      mus = concatMap (get #m) env_outs
  pure $ zip mus ps

-- Perform Metropolis-Hastings inference over linear regression
mhLinRegrOnce :: Int -> Int -> Sampler ([Double], [Double])
mhLinRegrOnce n_mhsteps n_datapoints = do
  let xs  = [0 .. fromIntegral n_datapoints]
      xys = [(x, env) | x <- xs, let env = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x]) <:> enil]
  mhTrace <- concat <$> mapM (\(x, y) -> MH.mh n_mhsteps (linRegrOnce x) y ["m", "c"]) xys
  let -- Get output of MH and extract mu samples
      mus = concatMap (get #m) mhTrace
      cs  = concatMap (get #c) mhTrace
  pure (mus, cs)


-- ||| Linear regression on many data points at once
linRegr :: forall env rs .
  Observables env '["y", "m", "c", "σ"] Double =>
  [Double] -> Model env rs [Double]
linRegr xs = do
  m <- normal 0 3 #m
  c <- normal 0 5 #c
  σ <- uniform 1 3 #σ
  ys <- mapM (\x -> do
                    y <- normal (m * x + c) σ #y
                    pure y) xs
  pure ys

simLinRegr :: Int -> Sampler [(Double, Double)]
simLinRegr n_datapoints = do
  let xs  = [0 .. fromIntegral n_datapoints]
      env = (#m := [3.0]) <:> (#c := [0]) <:> (#σ := [1]) <:> (#y := []) <:> enil
  bs :: ([Double], Env LinRegrEnv) <- SIM.simulate (linRegr xs) env 
  pure $ zip xs (fst bs)

lwLinRegr ::  Int -> Int ->  Sampler [(Double, Double)]
lwLinRegr n_samples n_datapoints = do
  let xs            = [0 .. fromIntegral n_datapoints]
      env           = (#y := [3*x | x <- xs]) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  enil
  (env_outs, ps) <- unzip <$> LW.lw n_samples (linRegr xs) env
  let mus = concatMap (get #m) env_outs
  pure (zip mus ps)

mhLinRegr ::  Int -> Int ->  Sampler ([Double], [Double])
mhLinRegr n_mhsteps n_datapoints = do
  let xs            = [0 .. fromIntegral n_datapoints]
      env           = (#y := [3*x | x <- xs]) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  enil
  env_mh_outs <- MH.mh n_mhsteps (linRegr xs) (env) ["m", "c"]
  let mus = concatMap (get #m) env_mh_outs
  let cs = concatMap (get #c) env_mh_outs
  pure (mus, cs)
