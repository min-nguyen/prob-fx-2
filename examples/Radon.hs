
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

{- | A [case study](https://docs.pymc.io/en/v3/pymc-examples/examples/case_studies/multilevel_modeling.html)
    by Gelman and Hill as a hierarchical linear regression model, modelling the relationship between radon levels
    in households in different counties and whether these houses contain basements.
-}

module Radon where

import Control.Monad ( replicateM )
import Model ( MulModel, halfCauchy', halfNormal, normal )
import Env ( Env(..), Observables, Observable, Assign ((:=)), (<:>), enil, (<#>), vnil, get)
import Sampler ( Sampler )
import DataSets ( n_counties, logRadon, countyIdx, dataFloorValues )
import Inference.MC.SIM as SIM ( simulateWith )
import Inference.MC.SSMH as SSMH ( ssmhWith )
import Util ( findIndexes )

-- | Radon model environment
type RadonEnv =
  '[ "mu_a" ':= Double      -- ^ hyperprior mean for county intercept
   , "mu_b" ':= Double      -- ^ hyperprior mean for county gradient
   , "sigma_a" ':= Double   -- ^ hyperprior noise for county intercept
   , "sigma_b" ':= Double   -- ^ hyperprior noise for county gradient
   , "a" ':= Double         -- ^ intercept for a county
   , "b" ':= Double         -- ^ gradient for a county
   , "log_radon" ':= Double -- ^ log-radon level for a house
   ]

-- | Prior distribution over model hyperparameters
radonPrior :: Observables env '["mu_a", "mu_b", "sigma_a", "sigma_b"] Double
  => MulModel env es (Double, Double, Double, Double)
radonPrior = do
  mu_a    <- normal 0 10 #mu_a
  sigma_a <- halfNormal 5 #sigma_a
  mu_b    <- normal 0 10 #mu_b
  sigma_b <- halfNormal 5 #sigma_b
  return (mu_a, sigma_a, mu_b, sigma_b)

-- | The Radon model
--   We have predefined parameters: n counties = 85, len(floor_x) = 919, len(county_idx) = 919
radonModel :: Observables env '["mu_a", "mu_b", "sigma_a", "sigma_b", "a", "b", "log_radon"] Double
  -- | number of counties
  => Int
  -- | whether each house has a basement (1) or not (0)
  -> [Int]
  -- | the county (as an integer) each house belongs to
  -> [Int]
  -> MulModel env es [Double]
radonModel n_counties floor_x county_idx = do
  (mu_a, sigma_a, mu_b, sigma_b) <- radonPrior
  -- Intercept for each county
  a <- replicateM n_counties (normal mu_a sigma_a #a)  -- length = 85
  -- Gradient for each county
  b <- replicateM n_counties (normal mu_b sigma_b #b)  -- length = 85
  -- MulModel error
  eps <- halfCauchy' 5
  let -- Get county intercept for each datapoint
      a_county_idx = map (a !!) county_idx
      -- Get county gradient for each datapoint
      b_county_idx = map (b !!) county_idx
      floor_values = map fromIntegral floor_x
      -- Get radon estimate for each data point
      radon_est = zipWith (+) a_county_idx (zipWith (*) b_county_idx floor_values)
  -- Sample radon amount for each data point
  radon_like <- mapM (\rad_est -> normal rad_est eps #log_radon) radon_est
  return radon_like


mkRadonEnv :: ([Double], [Double], [Double], [Double], [Double], [Double], [Double]) -> Env RadonEnv
mkRadonEnv (mua, mub, siga, sigb, a, b, lograds) =
   #mu_a := mua <:> #mu_b := mub <:> #sigma_a := siga <:> #sigma_b := sigb <:> #a := a <:> #b := b <:> #log_radon := lograds <:> enil

-- | Simulate from the Radon model
simRadon :: Sampler ([Double], [Double])
simRadon = do
  -- Specify model environment
  let env_in = mkRadonEnv ([1.45], [-0.68], [0.3], [0.2], [], [], [])
  -- Simulate from model
  (bs, env_out) <- SIM.simulateWith  (radonModel n_counties dataFloorValues countyIdx) env_in
  -- Retrieve and return the radon-levels for houses with basements and those without basements
  let basementIdxs      = findIndexes dataFloorValues 0
      noBasementIdxs    = findIndexes dataFloorValues 1
      basementPoints    = map (bs !!) basementIdxs
      nobasementPoints  = map (bs !!) noBasementIdxs
  return (basementPoints, nobasementPoints)

-- | Run SSMH inference and return posterior for hyperparameter means of intercept and gradient
ssmhRadon :: Int -> Sampler ([Double], [Double])
  -- Specify model environment
ssmhRadon n_mhsteps = do
  let env_in = mkRadonEnv ([], [], [], [], [], [], logRadon)
  -- Run SSMH inference
  env_outs <- SSMH.ssmhWith n_mhsteps (radonModel n_counties dataFloorValues countyIdx) env_in
                              (#mu_a <#> #mu_b <#> #sigma_a <#> #sigma_b <#> vnil)
  -- Retrieve sampled values for model hyperparameters mu_a and mu_b
  let mu_a   = concatMap (get #mu_a)  env_outs
      mu_b   = concatMap (get #mu_b)  env_outs
  return (mu_a, mu_b)

-- | Run SSMH inference and return predictive posterior for intercepts and gradients
ssmhPredRadon :: Int -> Sampler ([Double], [Double])
ssmhPredRadon n_mhsteps = do
  -- Specify model environment
  let env_in = mkRadonEnv ([], [], [], [], [], [], logRadon)
  -- Run SSMH inference
  env_outs <- SSMH.ssmhWith n_mhsteps (radonModel n_counties dataFloorValues countyIdx ) env_in
                              (#mu_a <#> #mu_b <#> #sigma_a <#> #sigma_b <#> vnil)
  -- Retrieve most recently sampled values for each house's intercept and gradient, a and b
  let env_pred   = head env_outs
      as         = get #a env_pred
      bs         = get #b env_pred
  return (as, bs)