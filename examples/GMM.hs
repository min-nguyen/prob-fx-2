

{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

{- | Gaussian mixture model (GMM) for a two-dimensional space.
     For simplicity, the mean along the x and y axis for a given cluster is the same.
-}

module GMM where

import Data.Type.Nat
import Data.Typeable
import Data.Proxy
import Model ( MulModel, dirichlet', discrete, normal )
import Inference.MC.SIM as SIM ( simulateWith )
import Inference.MC.SSMH as SSMH ( ssmhWith )
import Sampler ( Sampler )
import Control.Monad ( replicateM )
import Data.Kind (Constraint)
import Data.List as List ( elemIndex )
import Data.Maybe ( fromJust )
import Env ( Observables, Observable(..), Assign(..), vnil, (<#>), enil, (<:>) )
import qualified Vec

{- | Gaussian Mixture MulModel environment.
-}
type GMMEnv = '[
    "mu"   ':= Double,  -- ^ cluster mean (for both x and y)
    "mu_k" ':= Double,  -- ^ cluster index
    "x"    ':= Double,  -- ^ x data point
    "y"    ':= Double   -- ^ y data point
  ]

{- | Gaussian Mixture MulModel.
-}
gmm :: forall env n es. (Observables env '["mu", "mu_k", "x", "y"] Double, SNatI n, Typeable n)
  => SNat n -- ^ num clusters
  -> Int -- ^ num data points
  -> MulModel env es [((Double, Double), Int)] -- ^ data points and their assigned cluster index
gmm k n = do
  cluster_ps <- dirichlet' (Vec.replicate k 1)
  mus        <- replicateM (reflectToNum k) (normal 0 8 #mu)
  replicateM n (do mu_k <- discrete (zip mus (Vec.toList cluster_ps) ) #mu_k
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
  let n_clusters = snat @(FromGHC 2)
  -- | Specify model environment of two clusters with mean (-2.0, -2.0) and (3.5, 3.5)
      env_in =  #mu := [-4.0, 3.5] <:> #mu_k := [] <:> #x := [] <:> #y := [] <:> enil
  bs <- SIM.simulateWith (gmm n_clusters n_datapoints) env_in
  pure $ fst bs

mhGMM
  :: Int -- ^ num SSMH iterations
  -> Int -- ^ num data points
  -> Sampler [[Double]]
mhGMM n_mhsteps n_datapoints = do
  bs <- simGMM n_datapoints
  let (xs, ys) = unzip (map fst bs)
      env =  #mu := [] <:> #mu_k := [] <:> #x := xs <:> #y := ys <:> enil
  env_out <- SSMH.ssmhWith n_mhsteps (gmm (snat @(FromGHC 2)) n_datapoints) env (#mu <#> vnil)
  let mus = map (get #mu) env_out
  pure mus
