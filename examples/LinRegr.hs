
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}

{- | A linear regression model, modelling a linear relationship between data points x and y.
-}

module LinRegr where

import Model ( Model, normal, uniform )
import Inference.SIM as SIM ( simulate )
import Inference.LW as LW ( lw )
import Inference.MH as MH ( mh )
import Inference.SMC as SMC ( smc )
import Inference.RMSMC as RMSMC ( rmsmc )
import Inference.PMMH as PMMH ( pmmh )
import Inference.MB as MB ( handleMBayes )
import Sampler ( Sampler )
import Control.Monad ( replicateM )
import Data.Kind (Constraint)
import Env ( Observables, Observable(..), Assign((:=)), Env, enil, (<:>), vnil, (<#>) )
import Numeric.Log ( Log )
import qualified Control.Monad.Bayes.Class as Bayes
import qualified Control.Monad.Bayes.Weighted as Bayes
import qualified Control.Monad.Bayes.Traced as Bayes
import qualified Control.Monad.Bayes.Sampler as Bayes

{- | Linear regression environment.
-}
type LinRegrEnv =
    '[  "m" ':= Double, -- ^ gradient
        "c" ':= Double, -- ^ intercept
        "σ" ':= Double, -- ^ noise
        "y" ':= Double    -- ^ output
     ]

{- | Linear regression model.
-}
linRegr :: Observables env '["y", "m", "c", "σ"] Double
  => [Double]               -- ^ x datapoints
  -> Model env rs [Double]  -- ^ y datapoints
linRegr xs = do
  -- Draw model parameters from prior
  m <- normal 0 3 #m
  c <- normal 0 5 #c
  σ <- uniform 1 3 #σ
  -- Generate outputs ys
  mapM (\x -> normal (m * x + c) σ #y) xs

-- | Simulate from linear regression
simLinRegr :: Int -> Sampler [(Double, Double)]
simLinRegr n_datapoints = do
  -- Specify model inputs
  let xs  = [0 .. fromIntegral n_datapoints]
  -- Specify model environment
      env_in = (#m := [3.0]) <:> (#c := [0]) <:> (#σ := [1]) <:> (#y := []) <:> enil
  -- Simulate linear regression for each input x
  bs :: ([Double], Env LinRegrEnv) <- SIM.simulate (linRegr xs) env_in
  pure $ zip xs (fst bs)

-- | Likelihood weighting over linear regression
lwLinRegr ::  Int -> Int ->  Sampler [(Double, Double)]
lwLinRegr n_lwsteps n_datapoints = do
  -- Specify model inputs
  let xs            = [0 .. fromIntegral n_datapoints]
  -- Specify model environment
      env_in           = (#y := [3*x | x <- xs]) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  enil
   -- Get the sampled values of mu and their likelihood-weighting
  (env_outs, ps) <- unzip <$> LW.lw n_lwsteps (linRegr xs) env_in
  let mus = concatMap (get #m) env_outs
  pure (zip mus ps)

-- | Metropolis-Hastings over linear regression
mhLinRegr ::  Int -> Int ->  Sampler ([Double], [Double])
mhLinRegr n_mhsteps n_datapoints = do
  -- Specify model inputs
  let xs            = [0 .. fromIntegral n_datapoints]
  -- Specify model environment
      env_in        = (#y := [3*x | x <- xs]) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  enil
  -- Run MH
  env_outs <- MH.mh n_mhsteps (linRegr xs) env_in (#m <#> #c <#> vnil)
  -- Get the sampled values of mu and c
  let mus = concatMap (get #m) env_outs
  let cs = concatMap (get #c) env_outs
  pure (mus, cs)

-- | SMC over linear regression
smcLinRegr ::  Int -> Int ->  Sampler ([Double], [Double])
smcLinRegr n_particles n_datapoints = do
  -- Specify model inputs
  let xs            = [0 .. fromIntegral n_datapoints]
  -- Specify model environment
      env_in        = (#y := [3*x | x <- xs]) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  enil
  -- Run SMC
  env_outs <- SMC.smc n_particles (linRegr xs) env_in
  -- Get the sampled values of mu and c for each particle
  let mus = concatMap (get #m) env_outs
      cs = concatMap (get #c) env_outs
  pure (mus, cs)

-- | SMC over linear regression
rmsmcLinRegr :: Int -> Int -> Int -> Sampler ([Double], [Double])
rmsmcLinRegr n_particles n_mhsteps n_datapoints = do
  -- Specify model inputs
  let xs            = [0 .. fromIntegral n_datapoints]
  -- Specify model environment
      env_in        = (#y := [3*x | x <- xs]) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  enil
  -- Run SMC
  env_outs <- RMSMC.rmsmc n_particles n_mhsteps (linRegr xs) env_in
  -- Get the sampled values of mu and c for each particle
  let mus = concatMap (get #m) env_outs
      cs  = concatMap (get #c) env_outs
  pure (mus, cs)

-- | PMMH over linear regression
pmmhLinRegr :: Int -> Int -> Int -> Sampler ([Double], [Double])
pmmhLinRegr n_mhsteps n_particles  n_datapoints = do
  -- Specify model inputs
  let xs            = [0 .. fromIntegral n_datapoints]
  -- Specify model environment
      env_in        = (#y := [3*x | x <- xs]) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  enil
  -- Run SMC
  env_outs <- PMMH.pmmh n_mhsteps n_particles (linRegr xs) env_in  (#m <#> #c <#> vnil)
  -- Get the sampled values of mu and c for each particle
  let mus = concatMap (get #m) env_outs
      cs  = concatMap (get #c) env_outs
  pure (mus, cs)

{- | Linear regression model on individual data points at a time.
-}
linRegrOnce :: Observables env ["y", "m", "c", "σ"] Double
  => Double              -- ^ x datapoint
  -> Model env rs Double -- ^ y datapoint
linRegrOnce x = do
  -- Draw prior
  m <- normal 0 3 #m
  c <- normal 0 5 #c
  σ <- uniform 1 3 #σ
  -- Generate output y
  normal (m * x + c) σ #y

-- | Simulate from linear regression
simLinRegrOnce :: Int -> Sampler [(Double, Double)]
simLinRegrOnce n_datapoints = do
  -- Specify model inputs
  let xs  = [0 .. fromIntegral n_datapoints]
  -- Specify model environment
      env_in = (#m := [3.0]) <:> (#c := [0]) <:> (#σ := [1]) <:> (#y := []) <:> enil
  -- Simulate linear regression for each input x
  ys_envs <- mapM (\x -> SIM.simulate (linRegrOnce x) env_in) xs
  let ys = map fst ys_envs
  pure (zip xs ys)

-- | Likelihood weighting over linear regression
lwLinRegrOnce :: Int -> Int ->  Sampler [(Double, Double)]
lwLinRegrOnce n_samples n_datapoints = do
  -- Specify model inputs
  let xs  = [0 .. fromIntegral n_datapoints]
  -- Specify model environments and pair with model input
      xys = [(x, env_in) | x <- xs, let env_in = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x]) <:> enil]
  -- Run LW for n_samples on each pair of model input and environment
  lwTrace <- mapM (\(x, env_in) -> LW.lw n_samples (linRegrOnce  x) env_in) xys
  -- Get the sampled values of mu and their likelihood-weighting
  let (env_outs, ps) = unzip $ concat lwTrace
      mus = concatMap (get #m) env_outs
  pure $ zip mus ps

-- | Metropolis-Hastings over linear regression
mhLinRegrOnce :: Int -> Int -> Sampler ([Double], [Double])
mhLinRegrOnce n_mhsteps n_datapoints = do
  -- Specify model inputs
  let xs  = [0 .. fromIntegral n_datapoints]
  -- Specify model environments and pair with model input
      xys = [(x, env_in) | x <- xs, let env_in = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x]) <:> enil]
  -- Run MH for n_mhsteps iterations on each pair of model input and environment
  mhTrace <- concat <$> mapM (\(x, y) -> MH.mh n_mhsteps (linRegrOnce x) y  (#m <#> #c <#> vnil)) xys
  -- Get the sampled values of mu and c
  let mus = concatMap (get #m) mhTrace
      cs  = concatMap (get #c) mhTrace
  pure (mus, cs)

{- | Executing linear regression model using monad-bayes.
-}
mbayesLinRegr :: (Bayes.MonadInfer m, Observables env '["y", "m", "c", "σ"] Double) =>
 [Double] -> Env env -> m ([Double], Env env)
mbayesLinRegr xs = handleMBayes (linRegr xs)

simLinRegrMB :: Int -> Int -> IO [([Double], Env LinRegrEnv)]
simLinRegrMB n_samples n_datapoints = do
  let xs  = [0 .. fromIntegral n_datapoints]
      env_in = (#m := [3.0]) <:> (#c := [0]) <:> (#σ := [1]) <:> (#y := []) <:> enil
  Bayes.sampleIO $ Bayes.unweighted $ replicateM n_samples (mbayesLinRegr xs env_in)

lwLinRegrMB :: Int -> Int -> IO [(([Double], Env LinRegrEnv), Log Double)]
lwLinRegrMB n_datapoints n_samples = do
  let n_datapoints' = fromIntegral n_datapoints
      xs            = [0 .. n_datapoints']
      env_in           = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x | x <- xs]) <:> enil
  Bayes.sampleIO $ replicateM n_samples (Bayes.runWeighted $ mbayesLinRegr xs env_in)

mhLinRegrMB :: Int -> Int -> IO [([Double], Env LinRegrEnv)]
mhLinRegrMB n_samples n_datapoints = do
  let n_datapoints' = fromIntegral n_datapoints
      xs            = [0 .. n_datapoints']
      env_in           = (#m := []) <:> (#c := []) <:> (#σ := []) <:> (#y := [3*x | x <- xs]) <:> enil
  mhtrace <- Bayes.sampleIO (Bayes.unweighted $ Bayes.mh n_samples (mbayesLinRegr xs env_in))
  let (outputs, env_outs) = unzip mhtrace
      mus = concatMap (get #m) env_outs
  print mus
  pure mhtrace