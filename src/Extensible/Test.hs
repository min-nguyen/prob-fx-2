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

import Data.Maybe
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
import Util

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
           -> LRec Example.NNEnv
mkRecordNNy yobs_val =
  yObs @= [yobs_val] <: weight @= [] <: bias @= [] <: sigma @= [] <: nil

testNNBasic :: Sampler  [(Double, Double)]
testNNBasic = do
  let -- Run basic simulation over neural network
      bs = Basic.basic 1 (Example.nnModel 3)
                         (map (/1) [0 .. 300])
                         (repeat $ mkRecordNN ([], [1, 5, 8],
                                                   [2, -5, 1],
                                                   [4.0]))
      -- bs' = Basic.basic 3 Example.logisticRegression
      --                    [0, 1, 2, 3, 4]
      --                    (map mkRecordLogRegrL [False, False, True, False, True])
  output <- map fst <$> bs
  liftS $ print $ show output
  return output

testNNLW :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
testNNLW = do
  let xs  = concat [ replicate 11 x | x <- [0 .. 10]]
      -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a uniform area
      lws = LW.lw 1 (Example.nnModel 3)
                    xs
                    (concat $ repeat $ map (\y -> mkRecordNN ([y], [1, 5, 8],
                                              [2, -5, 1],
                                              [2.0])) [0 .. 10]   )
      -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a sine curve
      lws' = LW.lw 1  (Example.nnModel 3)
                      (map (/50) [0 .. 300])
                      (map (\y -> mkRecordNN ([y], [1, 5, 8],
                                                   [2, -5, 1],
                                                   [2.0]))
                           [ sin x | x <- map (/50) [0 .. 300] ])
      -- lws' = LW.lw 3 (Example.nnModel 3)
      --                (map (/1) [0 .. 300])
      --                (map mkRecordNNy [0 .. 300])
  output <- lws'
  let output' = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob)) output
  liftS $ print $ show output'
  return output'

-- Run this with nn-basic, as it returns a predictive distribution rather than a posterior one.
testNNMH :: Sampler [(Double, Double)]
testNNMH = do
  let -- Run mh over data representing a line with gradient 1 and intercept 0
      mhs = MH.mh 40 (Example.nnModel 3)
                    (map (/50) [0 .. 300])
                      (map mkRecordNNy
                           [ x | x <- map (/50) [0 .. 300] ])
  output <- mhs
  let output' = map (\(xy, samples, logps) ->
       let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
           logps'   = Map.toList logps
       in  (xy, samples', logps') ) output
  -- Get the most recent accepted model parameters from the posterior
  let postParams = map (fromJust . prj @Double . snd . snd)
                       ((Map.toList . snd3 . head) output)
      (bias, postParams') = splitAt 3 postParams
      (weights, sigma)    = splitAt 3 postParams'
  liftS $ print $ show (weights, bias, sigma)
  -- Using these parameters, simulate data from the predictive. We can see that the predictive data becomes more accurate with more mh steps.
  let bs = Basic.basic 1 (Example.nnModel 3)
                         (map (/1) [0 .. 300])
                         (repeat $ mkRecordNN ([], bias,
                                                   weights,
                                                   sigma))
  map fst <$> bs

-- -- testNNMH :: Sampler [((Double, Double), [(Addr, OpenSum MH.Vals)], [(Addr, Double)])]
-- testNNMH :: Sampler [(Double, Double)]
-- testNNMH = do
--   let xs  = concat [ replicate 11 x | x <- [0 .. 10]]
--       -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a uniform area
--       mhs = MH.mh 40 (Example.nnModel 3)
--                     (map (/50) [0 .. 300])
--                       (map mkRecordNNy
--                            [ x | x <- map (/50) [0 .. 300] ])
--       -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a sine curve
--       mhs' = MH.mh 1  (Example.nnModel 3)
--                       (map (/50) [0 .. 300])
--                       (map mkRecordNNy
--                            [ sin x | x <- map (/50) [0 .. 300] ])
--       -- lws' = LW.lw 3 (Example.nnModel 3)
--       --                (map (/1) [0 .. 300])
--       --                (map mkRecordNNy [0 .. 300])
--   output <- mhs
--   let output' = map (\(xy, samples, logps) ->
--        let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
--            logps'   = Map.toList logps
--        in  (xy, samples', logps') ) output

--   let postParams = map (fromJust . prj @Double . snd . snd)
--                        ((Map.toList . snd3 . head) output)
--       (bias, postParams') = splitAt 3 postParams
--       (weights, sigma)    = splitAt 3 postParams'

--   let bs = Basic.basic 1 (Example.nnModel 3)
--                          (map (/1) [0 .. 300])
--                          (repeat $ mkRecordNN ([], bias,
--                                                    weights,
--                                                    sigma))
--   liftS $ print $ show (weights, bias, sigma)
--   -- return output'
--   map fst <$> bs