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
import Extensible.RecordReader
import Data.Extensible

{- Linear Regression -}

mkRecordLinRegr :: ([Double],  [Double],  [Double],  [Double])
  -> LRec Example.LinRegrEnv
mkRecordLinRegr (y_vals, m_vals, c_vals, σ_vals) =
  y @= y_vals <: m @= m_vals <: c @= c_vals <: σ @= σ_vals <: nil

mkRecordLinRegrY :: [Double] -> LRec Example.LinRegrEnv
mkRecordLinRegrY y_vals =
 y @= y_vals <: m @= [] <: c @= [] <: σ @= [] <: nil

testLinRegrBasic :: Sampler [((Double, Double), LRec Example.LinRegrEnv)]
testLinRegrBasic = do
  let -- Run basic simulation over linearRegression
      {- This should generate a set of points on the y-axis for each given point on the x-axis -}
      bs   = Basic.basic 3 Example.linearRegression
                           [0, 1, 2, 3, 4]
                           ((repeat $ mkRecordLinRegr ([], [1.0, 10], [0.0], [1.0])) :: [LRec Example.LinRegrEnv])
      {- This should output the provided fixed set of data points on the x and y axis. -}
      bs'  = Basic.basic 3 Example.linearRegression
                    [0, 1, 2, 3, 4]
                    (map mkRecordLinRegrY [[-0.3], [0.75], [2.43], [3.5], [3.2]])
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
                    (repeat $ mkRecordLinRegr ([], [1], [0], [1]))
      -- Run likelihood weighting inference over linearRegression
      {- This should output the provided fixed set of data points on the x and y axis, where each point has a different probability (due to us observing the probability of given y's). -}
      lws' = LW.lw 3 Example.linearRegression
                    [0, 1, 2, 3, 4]
                    (map mkRecordLinRegrY [[-0.3], [0.75], [2.43], [3.5], [3.2]])
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
                     (repeat $ mkRecordLinRegr ([], [1], [0], [1]))
      -- Run mh inference over linearRegression
      mhs' = MH.mh 3 Example.linearRegression [1,2,3]
                     (map mkRecordLinRegrY [[-0.3], [1.6], [3.5]])
  output <- mhs'
  let output' = map (\(xy, samples, logps) ->
       let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
           logps'   = Map.toList logps
       in  (xy, samples', logps') ) output
  liftS $ print $ show output'
  return output'

{- Logistic Regression -}

mkRecordLogRegr :: ([Bool], [Double], [Double]) -> LRec Example.LogRegrEnv
mkRecordLogRegr (label_vals, m_vals, b_vals) =
  label @= label_vals <: m @= m_vals <: b @= b_vals <: nil

mkRecordLogRegrL :: [Bool] -> LRec Example.LogRegrEnv
mkRecordLogRegrL label_val =
 label @= label_val <: m @= [] <: b @= [] <: nil

testLogRegrBasic :: Sampler [((Double, Bool), LRec Example.LogRegrEnv)]
testLogRegrBasic = do
  let -- Run basic simulation over logisticRegression
      bs = Basic.basic 1 Example.logisticRegression
                         (map (/50) [(-100) .. 100])
                         (repeat $ mkRecordLogRegr ([], [-0.7], [-0.15]))
      bs' = Basic.basic 3 Example.logisticRegression
                         [0, 1, 2, 3, 4]
                         (map mkRecordLogRegrL [[False], [False], [True], [False], [True]])
  output <- bs
  liftS $ print $ show output
  return output

testLogRegrLW :: Sampler [((Double, Bool), [(Addr, OpenSum LW.Vals)], Double)]
testLogRegrLW = do
  let -- Run basic simulation over logisticRegression
      lws  = LW.lw 3 Example.logisticRegression
                         (map (/50) [(-100) .. 100])
                         (repeat $ mkRecordLogRegr ([], [-0.7], [-0.15]))
      lws' = LW.lw 3 Example.logisticRegression
                         [0, 1, 2, 3, 4]
                         (map mkRecordLogRegrL [[False], [False], [True], [False], [True]])
  output <- lws'
  let output' = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob)) output
  liftS $ print $ show output'
  return output'

testLogRegrMH :: Sampler [((Double, Bool), [(Addr, OpenSum MH.Vals)], [(Addr, Double)])]
testLogRegrMH = do
  let -- Run basic simulation over logisticRegression
      mhs  = MH.mh 3 Example.logisticRegression
                         (map (/50) [(-100) .. 100])
                         (repeat $ mkRecordLogRegr ([], [-0.7], [-0.15]))
      mhs' = MH.mh 3 Example.logisticRegression
                         [0, 1, 2, 3, 4]
                         (map mkRecordLogRegrL [[False], [False], [True], [False], [True]])
  output <- mhs'
  let output' = map (\(xy, samples, logps) ->
       let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
           logps'   = Map.toList logps
       in  (xy, samples', logps') ) output
  liftS $ print $ show output'
  return output'

-- {- Bayesian Neural Network -}

mkRecordNN :: ([Double], [Double], [Double], [Double])
           -> LRec Example.NNEnv
mkRecordNN (yobs_vals, weight_vals, bias_vals, sigm_vals) =
  yObs @= yobs_vals <: weight @= weight_vals <: bias @= bias_vals <: sigma @= sigm_vals <: nil

mkRecordNNy :: Double
           -> MRec Example.NNEnv
mkRecordNNy yobs_val =
  yObs @= Just yobs_val <: weight @= Nothing <: bias @= Nothing <: sigma @= Nothing <: nil

testNNBasic :: Sampler  [(Double, Double)]
testNNBasic = do
  let -- Run basic simulation over logisticRegression
      bs = Basic.basic 1 (Example.nnModel 3)
                         (map (/1) [0 .. 300])
                         (repeat $ mkRecordNN ([], [1, 5, 8],
                                                   [2, -5, 1],
                                                   [0.2]))
      -- bs' = Basic.basic 3 Example.logisticRegression
      --                    [0, 1, 2, 3, 4]
      --                    (map mkRecordLogRegrL [False, False, True, False, True])
  output <- map fst <$> bs
  liftS $ print $ show output
  return output
