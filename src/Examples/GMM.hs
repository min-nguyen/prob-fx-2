
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}

module Examples.GMM where

import Model
import Inference.SIM as Simulate
import Inference.LW as LW
import Sampler
import Control.Monad
import Data.Kind (Constraint)
import Data.List as List
import Data.Maybe
import Env
import Util
-- | Gaussian Mixture Model

type GMMEnv = '[
    "mu" ':= Double,
    "mu_k" ':= Double,
    "x"  ':= Double,
    "y"  ':= Double
  ]

gmm :: Observables env '["mu", "mu_k", "x", "y"] Double
  => Int -- num clusters
  -> Int -- num data points
  -> Model env es [((Double, Double), Int)]
gmm k n = do
  cluster_ps <- dirichlet' (replicate k 1)
  mus        <- replicateM k (normal 0 5 #mu)
  replicateM n (do mu_k <- categorical (zip mus cluster_ps) #mu_k
                   let i = fromJust $ elemIndex mu_k mus
                   x    <- normal mu_k 1 #x
                   y    <- normal mu_k 1 #y
                   return ((x, y), i))

