{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications, UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Extensible.Test where

import qualified Data.Map as Map
import qualified Extensible.Example as Example
import Extensible.Dist
import qualified Extensible.Inference.Basic as Basic
import qualified Extensible.Inference.LW as LW
import qualified Extensible.Inference.MH as MH
import Extensible.OpenSum
import Extensible.Inference.Inf
import Extensible.Model
import Extensible.Sampler
import Data.Extensible

{- Linear Regression -}

mkRecordLinRegr :: (Maybe Double, Maybe Double, Maybe Double, Maybe Double) -> MRec Example.LinRegrEnv
mkRecordLinRegr (y_val, m_val, c_val, σ_val) =
  y @= y_val <: m @= m_val <: c @= c_val <: σ @= σ_val <: nil

mkRecordLinRegrY :: Double -> MRec Example.LinRegrEnv
mkRecordLinRegrY y_val =
 y @= Just y_val <: m @= Nothing <: c @= Nothing <: σ @= Nothing <: nil

testLinRegrBasic :: Sampler [(Double, Double)]
testLinRegrBasic = do
  let -- Run basic simulation over linearRegression
      {- This should generate a set of points on the y-axis for each given point on the x-axis -}
      bs   = Basic.basic 3 Example.linearRegression
                           [0, 1, 2, 3, 4]
                           (repeat $ mkRecordLinRegr (Nothing, Just 1, Just 0, Just 1))
      {- This should output the provided fixed set of data points on the x and y axis. -}
      bs'  = Basic.basic 3 Example.linearRegression
                    [0, 1, 2, 3, 4]
                    (map mkRecordLinRegrY [-0.3, 0.75, 2.43, 3.5, 3.2])
  output <- bs
  liftS $ print $ show output
  return output

-- | [(datapoints, samples, likelihood)]
testLinRegrLW :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
testLinRegrLW = do
  let -- Run likelihood weighting simulation over linearRegression
      {- This should generate a set of points on the y-axis for each given point on the x-axis
         where every point has the same likelihood (because we don't count the probability for sampling the y data point, and all other observe probabilities are the same every iteration). -}
      lws = LW.lw 3 Example.linearRegression
                    [0, 1, 2, 3, 4]
                    (repeat $ mkRecordLinRegr (Nothing, Just 1, Just 0, Just 1))
      -- Run likelihood weighting inference over linearRegression
      {- This should output the provided fixed set of data points on the x and y axis, where each point has a different probability (due to us observing the probability of given y's). -}
      lws' = LW.lw 3 Example.linearRegression
                    [0, 1, 2, 3, 4]
                    (map mkRecordLinRegrY [-0.3, 0.75, 2.43, 3.5, 3.2])
  output <- lws'
  let output' = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob)) output
  liftS $ print $ show output'
  return output'

-- | [(datapoints, samples, logps)]
testLinRegrMH :: Sampler [((Double, Double), [(Addr, OpenSum MH.Vals)], [(Addr, Double)])]
testLinRegrMH = do
  let -- Run mh simulation over linearRegression
      mhs  = MH.mh 3 Example.linearRegression [1,2,3]
                     (repeat $ mkRecordLinRegr (Nothing, Just 1, Just 0, Just 1))
      -- Run mh inference over linearRegression
      mhs' = MH.mh 3 Example.linearRegression [1,2,3]
                     (map mkRecordLinRegrY [-0.3, 1.6, 3.5])
  output <- mhs'
  let output' = map (\(xy, samples, logps) ->
       let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
           logps'   = Map.toList logps
       in  (xy, samples', logps') ) output
  liftS $ print $ show output'
  return output'

{- Logistic Regression -}

mkRecordLogRegr :: (Maybe Bool, Maybe Double, Maybe Double) -> MRec Example.LogRegrEnv
mkRecordLogRegr (label_val, m_val, b_val) =
  label @= label_val <: m @= m_val <: b @= b_val <: nil

mkRecordLogRegrL :: Bool -> MRec Example.LogRegrEnv
mkRecordLogRegrL label_val =
 label @= Just label_val<: m @= Nothing <: b @= Nothing <: nil

testLogRegrBasic :: Sampler  [(Double, Bool)]
testLogRegrBasic = do
  let -- Run basic simulation over logisticRegression
      bs = Basic.basic 3 Example.logisticRegression
                         [0, 1, 2, 3, 4]
                         (repeat $ mkRecordLogRegr (Nothing, Just 0.3, Just (-0.2)))
      bs' = Basic.basic 3 Example.logisticRegression
                         [0, 1, 2, 3, 4]
                         (repeat $ mkRecordLogRegr (Nothing, Nothing, Nothing))
  output <- bs'
  liftS $ print $ show output
  return output
