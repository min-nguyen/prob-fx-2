

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MonoLocalBinds #-}

{-# LANGUAGE TypeApplications #-}

{- | A linear regression model, modelling a linear relationship between data points x and y.
-}

module VIExamples where

import Inference.MC.SIM as SIM ( simulate )
import Inference.MC.SMC2 as SMC2 ( smc2 )
import Inference.VI.VI
import qualified Inference.VI.BBVI as BBVI
import qualified Inference.VI.MLE as MLE
import qualified Inference.VI.MAP as MAP
import Sampler ( Sampler, sampleIO, liftIO, sampleIOFixed )
import qualified Trace
import           Trace (Key(..))
import Control.Monad ( replicateM, (>=>) )
import Data.Kind (Constraint)
import Env ( Observables, Observable(..), Assign((:=)), Env, enil, (<:>), vnil, (<#>) )
import Effects.Dist
import PrimDist
import Data.Maybe
import HMM (simHMM)
import Prog
import Model


{- | Linear regression environment.
-}
type LinRegrEnv =
    '[  "m" ':= Double, -- ^ gradient
        "c" ':= Double, -- ^ intercept
        "σ" ':= Double, -- ^ noise
        "y" ':= Double    -- ^ output
     ]

{- | Linear regression as a probabilistic program for inference -}
linRegr :: forall env. Observables env '["m", "c"] Double
  => [(Double, Double)] -> VIModel env (Double, Double)  -- ^ y datapoints
linRegr xys = do
  -- Draw model parameters from prior
  m <- sample' @env (mkNormal 0 3) (#m, 0)
  c <- sample' @env (mkNormal 0 5) (#c, 0)
  -- Generate outputs ys
  mapM_ (\((x, y), idx) -> observe (mkNormal (m * x + c) 1) y (Addr "y" idx)) (zip xys [0 ..])
  return (m, c)

linRegrGuide :: forall env. Observables env '["m", "c"] Double
  => VIGuide env ()
linRegrGuide = do
  m <- param' @env (mkNormal 0 3) (#m, 0)
  c <- param' @env (mkNormal 0 5) (#c, 0)
  return ()

bbviLinRegr :: Int -> Int -> Int -> Sampler ([Double], [Double])
bbviLinRegr t_steps l_samples n_datapoints = do
  let xys          = [ (x, 2 * x) | x <- [1 .. fromIntegral n_datapoints]]
      empty_env    = (#y := []) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  enil
  traceQ <- BBVI.bbvi t_steps l_samples linRegrGuide (linRegr xys) empty_env
  let m_dist = toList . fromJust $ Trace.lookupBy @Normal ((== "m") . tag ) traceQ
      c_dist = toList . fromJust $ Trace.lookupBy @Normal ((== "c") . tag ) traceQ
  pure (m_dist, c_dist)

mleLinRegr :: Int -> Int -> Int -> Sampler ([Double], [Double])
mleLinRegr t_steps l_samples n_datapoints = do
  let xys          = [ (x, 2 * x) | x <- [1 .. fromIntegral n_datapoints]]
      empty_env    = (#y := []) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  enil
  traceQ <- MLE.mle t_steps l_samples linRegrGuide (linRegr xys) empty_env
  let m_dist = toList . fromJust $ Trace.lookupBy @Normal ((== "m") . tag ) traceQ
      c_dist = toList . fromJust $ Trace.lookupBy @Normal ((== "c") . tag ) traceQ
  pure (m_dist, c_dist)

mapLinRegr :: Int -> Int -> Int -> Sampler ([Double], [Double])
mapLinRegr t_steps l_samples n_datapoints = do
  let xys          = [ (x, 2 * x) | x <- [1 .. fromIntegral n_datapoints]]
      empty_env    = (#y := []) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  enil
  traceQ <- MAP.map t_steps l_samples linRegrGuide (linRegr xys) empty_env
  let m_dist = toList . fromJust $ Trace.lookupBy @Normal ((== "m") . tag ) traceQ
      c_dist = toList . fromJust $ Trace.lookupBy @Normal ((== "c") . tag ) traceQ
  pure (m_dist, c_dist)

-- | Chain of HMM nodes
hmm :: forall env. (Observables env '["obs_p", "trans_p"] Double)
  -- | number of HMM nodes
  => Int
  -- | initial latent state
  -> Int
  -- | Observations
  -> [Int]
  -- | final latent state
  -> VIModel env (Int, Int)
hmm n x0 ys = do
  trans_p <- sample' @env (mkBeta 2 2) (#trans_p, 0)
  obs_p   <- sample' @env (mkBeta 2 2) (#obs_p, 0)
  let hmmNode (x, idx) = do  b <- fromEnum <$> sample (mkBernoulli trans_p) (Addr "x" idx)
                             let x_i = x + b
                             y_i <- observe (mkBinomial x_i obs_p) (ys !! idx) (Addr "y" idx)
                             return (x_i, idx + 1)
  foldr (>=>) return (replicate n hmmNode) (x0, 0)

hmmGuide :: forall env. (Observables env '["obs_p", "trans_p"] Double)
  => VIGuide env ()
hmmGuide = do
  trans_p <- param' @env (mkBeta 2 2) (#trans_p, 0)
  obs_p   <- param' @env (mkBeta 2 2) (#obs_p, 0)
  pure ()

bbviHMM :: Int -> Int -> Int -> Sampler ([Double], [Double])
bbviHMM t_steps l_samples hmm_length = do
  ys <- simHMM hmm_length
  liftIO (print ys)
  let empty_env  = #trans_p := [] <:> #obs_p := [] <:> enil

  traceQ <- BBVI.bbvi t_steps l_samples hmmGuide (hmm hmm_length 0 ys) empty_env
  let trans_dist = toList . fromJust $ Trace.lookupBy @Beta  ((== "trans_p") . tag ) traceQ
      obs_dist   = toList . fromJust $ Trace.lookupBy @Beta  ((== "obs_p") . tag ) traceQ
  pure (trans_dist, obs_dist)

