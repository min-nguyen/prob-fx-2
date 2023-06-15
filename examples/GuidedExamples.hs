

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MonoLocalBinds #-}

{-# LANGUAGE TypeApplications #-}

{- | A linear regression model, modelling a linear relationship between data points x and y.
-}

module GuidedExamples where

import Inference.MC.SIM as SIM ( simulateWith )
import Inference.MC.SMC2 as SMC2 ( smc2 )
import Inference.Guided.Guided
import Inference.Guided.BBVI as BBVI
import Inference.Guided.MLE as MLE
import Sampler ( Sampler, sampleIO, liftIO, sampleIOFixed )
import qualified Trace
import           Trace (Key(..), runIdentity)
import Control.Monad ( replicateM, (>=>) )
import Data.Kind (Constraint)
import Env ( Observables, Observable(..), Assign((:=)), Env, enil, (<:>), vnil, (<#>) )
import Effects.MulDist
import Effects.GuidedSample
import Dist
import Data.Type.Nat
import Data.Maybe
import HMM (simHMM)
import Comp
import Model
import Vec (Vec, TypeableSNatI)
import Data.Proxy
import qualified LDA

{- | Linear regression as a probabilistic program for inference -}
linRegr :: Members [GuidedSample, Observe, Sample] es
  => [(Double, Double)] -> Comp es (Double, Double)  -- ^ y datapoints
linRegr xys = do
  -- Draw model parameters from prior
  m <- guidedSample  (mkNormal 0 3) (mkNormal 0 3) (Addr "m" 0)
  c <- sample (mkNormal 0 1) (Addr "c" 0)
  -- Generate outputs ys
  mapM_ (\((x, y), idx) -> observe (mkNormal (m * x + c) 1) y (Addr "y" idx)) (zip xys [0 ..])
  return (m, c)

bbviLinRegr :: Int -> Int -> Int -> Sampler [Double]
bbviLinRegr t_steps l_samples n_datapoints = do
  let xys          = [ (x, 2 * x) | x <- [1 .. fromIntegral n_datapoints]]
  traceQ <- BBVI.bbvi t_steps l_samples (linRegr xys)
  let m_dist = toList . runIdentity . fromJust $ Trace.lookupByAddr @Normal ((== "m") . tag ) traceQ
      -- c_dist = toList . fromJust $ Trace.lookupByAddr @Normal ((== "c") . tag ) traceQ
  pure m_dist

mleLinRegr :: Int -> Int -> Int -> Sampler [Double]
mleLinRegr t_steps l_samples n_datapoints = do
  let xys          = [ (x, 2 * x) | x <- [1 .. fromIntegral n_datapoints]]
  traceQ <- MLE.mle t_steps l_samples (linRegr xys)
  let m_dist = toList . runIdentity . fromJust $ Trace.lookupByAddr @Normal ((== "m") . tag ) traceQ
      -- c_dist = toList . fromJust $ Trace.lookupByAddr @Normal ((== "c") . tag ) traceQ
  pure m_dist
