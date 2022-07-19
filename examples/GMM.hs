
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}

{- | Gaussian mixture model (GMM) for a two-dimensional space.
     For simplicity, the mean along the x and y axis for a given cluster is the same.
-}

module GMM where

import Model ( Model, dirichlet', discrete, normal )
import Inference.SIM as SIM ( simulate )
import Inference.MH as MH ( mh )
import Sampler ( Sampler )
import Control.Monad ( replicateM )
import Data.Kind (Constraint)
import Data.List as List ( elemIndex )
import Data.Maybe ( fromJust )
import Env ( Observables, Observable(..), Assign(..), vnil, (<#>), enil, (<:>) )

{- | Gaussian Mixture Model environment.
-}
type GMMEnv = '[
    "mu"   ':= Double,  -- ^ cluster mean (for both x and y)
    "mu_k" ':= Double,  -- ^ cluster index
    "x"    ':= Double,  -- ^ x data point
    "y"    ':= Double   -- ^ y data point
  ]

{- | Gaussian Mixture Model.
-}
gmm :: Observables env '["mu", "mu_k", "x", "y"] Double
  => Int -- ^ num clusters
  -> Int -- ^ num data points
  -> Model env es [((Double, Double), Int)] -- ^ data points and their assigned cluster index
gmm k n = do
  cluster_ps <- dirichlet' (replicate k 1)
  mus        <- replicateM k (normal 0 5 #mu)
  replicateM n (do mu_k <- discrete (zip mus cluster_ps) #mu_k
                   let i = fromJust $ elemIndex mu_k mus
                   x    <- normal mu_k 1 #x
                   y    <- normal mu_k 1 #y
                   pure ((x, y), i))

-- | Simulate from a GMM, with means (-2.0, -2.0) and (3.5, 3.5)
simGMM
  :: Int  -- ^ num data points
  -> Sampler [((Double, Double), Int)] -- ^ data points and their assigned cluster index
simGMM n_datapoints = do
  -- | Assume two clusters
  let n_clusters = 2
  -- | Specify model environment of two clusters with mean (-2.0, -2.0) and (3.5, 3.5)
      env =  #mu := [-2.0, 3.5] <:> #mu_k := [] <:> #x := [] <:> #y := [] <:> enil
  bs <- SIM.simulate (gmm n_clusters n_datapoints) env
  pure $ fst bs

mhGMM
  :: Int -- ^ num MH iterations
  -> Int -- ^ num data points
  -> Sampler [[Double]]
mhGMM n_mhsteps n_datapoints = do
  bs <- simGMM n_datapoints
  let (xs, ys) = unzip (map fst bs)
      env =  #mu := [] <:> #mu_k := [] <:> #x := xs <:> #y := ys <:> enil
  env_mh_out <- MH.mh n_mhsteps (gmm 2 n_datapoints) env (#mu <#> vnil)
  let mus = map (get #mu) env_mh_out
  pure mus
