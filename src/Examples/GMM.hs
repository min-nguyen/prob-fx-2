
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}

module Examples.GMM where

import Model
import Inference.SIM as SIM
import Inference.LW as LW
import Inference.MH as MH
import Sampler
import Control.Monad
import Data.Kind (Constraint)
import Data.List as List
import Data.Maybe
import Env

-- ||| Gaussian Mixture Model
type GMMEnv = '[
    "mu"   ':= Double,
    "mu_k" ':= Double,
    "x"    ':= Double,
    "y"    ':= Double
  ]

gmm :: Observables env '["mu", "mu_k", "x", "y"] Double
  => Int -- num clusters
  -> Int -- num data points
  -> Model env es [((Double, Double), Int)]
gmm k n = do
  cluster_ps <- dirichlet' (replicate k 1)
  mus        <- replicateM k (normal 0 5 #mu)
  replicateM n (do mu_k <- discrete (zip mus cluster_ps) #mu_k
                   let i = fromJust $ elemIndex mu_k mus
                   x    <- normal mu_k 1 #x
                   y    <- normal mu_k 1 #y
                   pure ((x, y), i))

simGMM :: Int -> Sampler [((Double, Double), Int)]
simGMM n_datapoints = do
  let n_clusters = 2
      env =  #mu := [-2.0, 3.5] <:> #mu_k := [] <:> #x := [] <:> #y := [] <:> enil
  bs <- SIM.simulate (gmm 2 n_datapoints) env
  pure $ fst bs

mhGMM :: Int -> Int -> Sampler [[Double]]
mhGMM n_mhsteps n_datapoints = do
  bs <- simGMM n_datapoints
  let (xs, ys) = unzip (map fst bs)
      env =  #mu := [] <:> #mu_k := [] <:> #x := xs <:> #y := ys <:> enil
  env_mh_out <- MH.mh n_mhsteps (gmm 2 n_datapoints) env (#mu <#> vnil)
  let mus = map (get #mu) env_mh_out
  pure mus
