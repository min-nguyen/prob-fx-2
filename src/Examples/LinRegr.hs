
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
module Examples.LinRegr where

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
                    pure y) xs
  pure ys

simLinRegr :: Int -> Int -> Sampler [(Double, Double)]
simLinRegr n_datapoints n_samples = do
  let xs  = [0 .. fromIntegral n_datapoints]
      env = (#m := [3.0]) <:> (#c := [0]) <:> (#σ := [1]) <:> (#y := []) <:> eNil
  bs :: ([Double], Env LinRegrEnv) <- SIM.simulate (linRegr xs) env
  pure $ zip xs (fst bs)

lwLinRegr :: Int -> Int -> Sampler [(Double, Double)]
lwLinRegr n_datapoints n_samples = do
  let n_datapoints' = fromIntegral n_datapoints
      xs            = [0 .. n_datapoints']
      env           = (#y := [3*x | x <- xs]) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  eNil
  (env_outs, ps) <- unzip <$> LW.lwTopLevel n_samples (linRegr xs) env
  let mus = concatMap (get #m) env_outs
  pure (zip mus ps)

mhLinRegr :: Int -> Int -> Sampler [Double]
mhLinRegr n_datapoints n_samples = do
  let n_datapoints' = fromIntegral n_datapoints
      xs            = [0 .. n_datapoints']
      env           = (#y := [3*x | x <- xs]) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  eNil
  env_mh_outs <- MH.mhTopLevel n_samples (linRegr xs) env (#m ⋮ #c ⋮ #σ ⋮ ONil)
  let mus = concatMap (get #m) env_mh_outs
  pure mus

{- (Section 1) Linear regression once -}
linRegrOnce :: forall env rs .
  Observables env '["y", "m", "c", "σ"] Double =>
  Double -> Model env rs Double
linRegrOnce x = do
  m <- normal 0 3 #m
  c <- normal 0 5 #c
  σ <- uniform 1 3 #σ
  y <- normal (m * x + c) σ #y
  pure y

-- | (Fig 1. a) SIM from linear regression
simLinRegrOnce :: Sampler [(Double, Double)]
simLinRegrOnce  = do
  let xs  = [0 .. 100]
      env = (#m := [3.0]) <:> (#c := [0]) <:> (#σ := [1]) <:> (#y := []) <:> eNil
  ys_envs <- mapM (\x -> SIM.simulate (linRegrOnce  x) env) xs
  let ys = map fst ys_envs
  pure (zip xs ys)

-- | (Fig 1. b) Perform likelihood weighting over linear regression; returns sampled mu values and associated likelihood weightings
lwLinRegrOnce  :: Sampler [(Double, Double)]
lwLinRegrOnce  = do
  let xs  = [0 .. 100]
      xys = [(x, env) | x <- xs, let env = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x]) <:> eNil]
  lwTrace <- mapM (\(x, env) -> LW.lwTopLevel 200 (linRegrOnce  x) env) xys
  let -- Get output of LW and extract mu samples
      (env_outs, ps) = unzip $ concat lwTrace
      mus = concatMap (get #m) env_outs
  pure $ zip mus ps

-- | Perform Metropolis-Hastings inference over linear regression
mhLinRegrOnce  :: Sampler [Double]
mhLinRegrOnce  = do
  let xs  = [0 .. 100]
      xys = [(x, env) | x <- xs, let env = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x]) <:> eNil]
  mhTrace <- concat <$> mapM (\(x, env) -> MH.mhTopLevel 100 (linRegrOnce  x) env ONil) xys
  let -- Get output of LW and extract mu samples
      mus = concatMap (get #m) mhTrace
  pure mus

