
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}

module Examples.HierarchicalSchool where

import Model
import Inference.SIM as Simulate
import Inference.LW as LW
import Sampler
import Control.Monad
import Data.Kind (Constraint)
import Env
import Util

-- | Hierarchical School Model
type SchEnv = '[
    "mu"    ':= Double,
    "theta" ':= [Double],
    "y"     ':= Double
  ]

schoolModel :: (Observables env '["mu", "y"] Double, Observable env "theta" [Double])
  => Int -> [Double] -> Model env es [Double]
schoolModel n_schools σs = do
  μ   <- normal 0 10 #mu
  τ   <- halfNormal' 10
  ηs  <- replicateM n_schools (normal' 0 1)
  θs  <- deterministic (map ((μ +) . (τ *)) ηs) #theta
  ys  <- mapM (\(θ, σ) -> normal θ σ #y) (zip θs σs)
  let h = ""
  return θs

testSchBasic :: Sampler [Double]
testSchBasic = do
  let n_schools = 8
      ys        = [28, 8, -3,   7, -1,  1, 18, 12]
      sigmas    = [15, 10, 16, 11,  9, 11, 10, 18]
      env       = #mu := [] <:> #theta := [] <:> #y := ys <:> ENil
  bs <- Simulate.simulate (schoolModel n_schools) env sigmas
  return $ fst bs

-- testSchMHPost :: Sampler ([(Addr, [Double])], [(Addr, [[Double]])])
-- testSchMHPost = do
--   let n_schools = 8
--       ys        = [28, 8, -3,   7, -1,  1, 18, 12]
--       sigmas    = [15, 10, 16, 11,  9, 11, 10, 18]
--   mhTrace <- MH.mh 2000 (Example.schoolModel n_schools) []
--               [sigmas] [mkRecordSch ([], [], ys)]
--   let mhTrace'   = processMHTrace mhTrace
--       thetas     = map fst3 mhTrace
--       muTrace    = extractPostParams (Proxy @Double) [("mu", 0)] mhTrace'
--       thetaTrace = extractPostParams (Proxy @[Double]) [("theta", 0)] mhTrace'
--   return (muTrace, thetaTrace)