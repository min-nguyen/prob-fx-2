
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
module Examples.LinRegr where

import Prog
import Effects.ObsReader
import Effects.Writer
import Model
import Inference.SIM as Simulate
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

{- (Section 1) Linear regression -}
linRegr :: forall env rs .
  Observables env '["y", "m", "c", "σ"] Double =>
  Double -> Model env rs Double
linRegr x = do
  m <- normal 0 3 #m
  c <- normal 0 5 #c
  σ <- uniform 1 3 #σ
  y <- normal (m * x + c) σ #y
  return y

-- | (Fig 1. a) Simulate from linear regression
simulateLinRegr :: Sampler [(Double, Double)]
simulateLinRegr = do
  let xs  = [0 .. 100]
      env = (#m := [3.0]) <:> (#c := [0]) <:> (#σ := [1]) <:> (#y := []) <:> eNil
  ys_envs <- mapM (Simulate.simulate linRegr env) xs
  let ys = map fst ys_envs
  return (zip xs ys)

-- | (Fig 1. b) Perform likelihood weighting over linear regression; returns sampled mu values and associated likelihood weightings
inferLwLinRegr :: Sampler [(Double, Double)]
inferLwLinRegr = do
  let xs  = [0 .. 100]
      xys = [(x, env) | x <- xs, let env = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x]) <:> eNil]
  lwTrace <- mapM (\(x, env) -> LW.lwTopLevel 200 (linRegr x) env) xys
  let -- Get output of LW and extract mu samples
      (env_outs, ps) = unzip $ concat lwTrace
      mus = concatMap (get #m) env_outs
  return $ zip mus ps

-- | Perform Metropolis-Hastings inference over linear regression
inferMhLinRegr :: Sampler [Double]
inferMhLinRegr = do
  let xs  = [0 .. 100]
      xys = [(x, env) | x <- xs, let env = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x]) <:> eNil]
  mhTrace <- concat <$> mapM (\(x, env) -> MH.mhTopLevel 100 (linRegr x) env ONil) xys
  let -- Get output of LW and extract mu samples
      mus = concatMap (get #m) mhTrace
  return mus

{-  Linear regression many -}
linRegrMany :: forall env rs .
  Observables env '["y", "m", "c", "σ"] Double =>
  [Double] -> Model env rs [Double]
linRegrMany xs = do
  m <- normal 0 3 #m
  c <- normal 0 5 #c
  σ <- uniform 1 3 #σ
  ys <- mapM (\x -> do
                    y <- normal (m * x + c) σ #y
                    return y) xs
  return ys

testLinRegrSim :: Int -> Int -> Sampler [(Double, Double)]
testLinRegrSim n_datapoints n_samples = do
  let xs  = [0 .. fromIntegral n_datapoints]
      env = (#m := [3.0]) <:> (#c := [0]) <:> (#σ := [1]) <:> (#y := []) <:> eNil
  bs :: ([Double], Env LinRegrEnv) <- Simulate.simulate linRegrMany env xs
  return $ zip xs (fst bs)