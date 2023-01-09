
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE OverloadedLabels #-}

{- | The Gelman and Hill [8-schools case study](https://cran.r-project.org/web/packages/rstan/vignettes/rstan.html),
     which quantifies the effect of coaching programs from 8 different schools on students' SAT-V scores.
-}

module School where

import Model ( Model, deterministic, normal, normal', halfNormal' )
import Inference.MC.MH as MH ( mh )
import Sampler ( Sampler )
import Control.Monad ( replicateM )
import Data.Kind (Constraint)
import Env ( Env(..), Observables, Observable, Assign ((:=)), (<:>), enil, (<#>), vnil, get)

-- | School model environment
type SchEnv = '[
    "mu"    ':= Double,   -- ^ effect of general coaching programs on SAT scores
    "theta" ':= [Double], -- ^ variation of each program's effect on SAT scores
    "y"     ':= Double    -- ^ effectiveness on SAT scores
  ]

-- | School model
schoolModel :: (Observables env '["mu", "y"] Double, Observable env "theta" [Double])
  -- | number of schools
  => Int
  -- | standard errors of each school
  -> [Double]
  -- | effectiveness of each school
  -> Model env es [Double]
schoolModel n_schools σs = do
  μ   <- normal 0 10 #mu
  τ   <- halfNormal' 10
  ηs  <- replicateM n_schools (normal' 0 1)
  θs  <- deterministic (map ((μ +) . (τ *)) ηs) #theta
  ys  <- mapM (\(θ, σ) -> normal θ σ #y) (zip θs σs)
  return θs

-- | Perform MH inference
mhSchool :: Int ->  Sampler ([Double], [[Double]])
mhSchool n_mhsteps = do
  -- Specify model inputs
  let n_schools = 8
      ys        = [28, 8, -3,   7, -1,  1, 18, 12]
      sigmas    = [15, 10, 16, 11,  9, 11, 10, 18]
  -- Specify model environment
      env_in       = #mu := [] <:> #theta := [] <:> #y := ys <:> ENil
  -- Run MH inference for 10000 iterations
  env_outs <- MH.mh n_mhsteps (schoolModel n_schools sigmas) env_in (#mu <#> #theta <#> vnil)
  -- Retrieve and returns the trace of model parameters mu and theta
  let mus    = concatMap (get #mu) env_outs
      thetas = concatMap (get #theta) env_outs
  return (mus, thetas)
