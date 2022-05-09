
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
import Util
-- | Gaussian Mixture Model

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
  replicateM n (do mu_k <- categorical (zip mus cluster_ps) #mu_k
                   let i = fromJust $ elemIndex mu_k mus
                   x    <- normal mu_k 1 #x
                   y    <- normal mu_k 1 #y
                   return ((x, y), i))

simGMM :: Sampler [((Double, Double), Int)]
simGMM = do
  let env =  #mu := [-2.0, 3.5] <:> #mu_k := [] <:> #x := [] <:> #y := [] <:> eNil
  bs <- SIM.simulate (gmm 2 20) env
  return $ fst bs

mhGMM :: Sampler [[Double]]
mhGMM = do
  bs <- simGMM
  let (xs, ys) = unzip (map fst bs)
      env =  #mu := [] <:> #mu_k := [] <:> #x := xs <:> #y := ys <:> eNil
  env_mh_out <- MH.mhTopLevel 4000 (gmm 2 20) env  (#mu ⋮  ONil)
  let mus = map (get #mu) env_mh_out
  printS mus
  return mus
