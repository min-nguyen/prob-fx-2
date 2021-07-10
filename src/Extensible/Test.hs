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
import Extensible.AffineReader
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

testLinRegrBasic :: Sampler [(Double, Double)]
testLinRegrBasic = do
  let -- Run basic simulation over linearRegression
      {- This should generate a set of points on the y-axis for each given point on the x-axis -}
      bs   = Basic.basic 3 Example.linearRegression
                           [0, 1, 2, 3, 4]
                           ((repeat $ mkRecordLinRegr ([], [1.0], [0.0], [1.0])) :: [LRec Example.LinRegrEnv])
      {- This should output the provided fixed set of data points on the x and y axis. -}
      bs'  = Basic.basic 3 Example.linearRegression
                    [0, 1, 2, 3, 4]
                    (map mkRecordLinRegrY [[-0.3], [0.75], [2.43], [3.5], [3.2]])
  map fst <$> bs

testLinRegrLWSim :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
testLinRegrLWSim = do
  let -- Run linear model with fixed parameters, inputs, and outputs, to get likelihood of every data point over a uniform area
      xs  = concat [ replicate 11 x | x <- [0 .. 10]]
      lws = LW.lw 3 Example.linearRegression
                    xs
                    (concat $ repeat $ map (\y -> mkRecordLinRegr ([y], [1], [2], [2.0]))
                      [0 .. 10])
  output <- lws
  let output' = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob)) output
  liftS $ print $ show output'
  return output'

-- | [(datapoints, samples, likelihood)]
testLinRegrLWInf :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
testLinRegrLWInf = do
  let -- Run likelihood weighting inference over linearRegression
      {- This should output the provided fixed set of data points on the x and y axis, where each point has a different probability (due to us observing the probability of given y's). Also returns a trace of parameters and their likelihoods -}
      lws' = LW.lw 1 Example.linearRegression
                    [0 .. 100]
                    (map (mkRecordLinRegrY . (:[]) ) [0 .. 100])
  output <- lws'
  let output' = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob)) output
  liftS $ print $ show output'
  return output'

-- | Returns trace of model parameter samples
testLinRegrMHPost :: Sampler [((Double, Double), [(Addr, OpenSum MH.Vals)], [(Addr, Double)])]
testLinRegrMHPost = do
  let -- Run mh inference over linearRegression for data representing a line with gradient 1 and intercept 0
      mhs' = MH.mh 20 Example.linearRegression [0 .. 100]
                     (map (mkRecordLinRegrY . (:[]) ) [0 .. 100])
  -- Reformat MH trace
  mhTrace <- mhs'
  let mhTrace' = map (\(xy, samples, logps) ->
       let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
           logps'   = Map.toList logps
       in  (xy, samples', logps') ) mhTrace
  liftS $ print $ show mhTrace'
  return mhTrace'

-- | Use mh posterior to output predictive distribution
testLinRegrMHPred :: Sampler [(Double, Double)]
testLinRegrMHPred = do
  mhTrace <- testLinRegrMHPost
  liftS $ print $ show mhTrace
  -- Get the most recent accepted model parameters from the posterior
  let postParams = map (fromJust . prj @Double . snd)
                       ((snd3 . head) mhTrace)
      (mu, postParams') = splitAt 1 postParams
      (c, sigma)        = splitAt 1 postParams'
  -- Using these parameters, simulate data from the predictive.
  let bs = Basic.basic 1 Example.linearRegression
                         (map (/1) [0 .. 100])
                         (repeat $ mkRecordLinRegr ([], mu, c, sigma))
  map fst <$> bs

{- Logistic Regression -}
mkRecordLogRegr :: ([Bool], [Double], [Double]) -> LRec Example.LogRegrEnv
mkRecordLogRegr (label_vals, m_vals, b_vals) =
  label @= label_vals <: m @= m_vals <: b @= b_vals <: nil

mkRecordLogRegrL :: [Bool] -> LRec Example.LogRegrEnv
mkRecordLogRegrL label_val =
 label @= label_val <: m @= [] <: b @= [] <: nil

testLogRegrBasic :: Sampler [(Double, Bool)]
testLogRegrBasic = do
  let -- This should generate a set of points on the y-axis for each given point on the x-axis
      bs = Basic.basic 1 Example.logisticRegression
                         (map (/50) [(-100) .. 100])
                         (repeat $ mkRecordLogRegr ([], [2], [-0.15]))
      -- This should output the provided fixed set of data points on the x and y axis.
      bs' = Basic.basic 3 Example.logisticRegression
                         [0, 1, 2, 3, 4]
                         (map mkRecordLogRegrL [[False], [False], [True], [False], [True]])
  map fst <$> bs

testLogRegrLWSim :: Sampler [((Double, Bool), [(Addr, OpenSum LW.Vals)], Double)]
testLogRegrLWSim = do
  let -- Run logistic model with fixed parameters, inputs, and outputs, to get likelihood of every data point over a uniform area
      lws  = LW.lw 3 Example.logisticRegression
                         (concatMap ((\x -> [x, x]) . (/50)) [(-100) .. 100])
                         (concat $ repeat $ map mkRecordLogRegr $ ([True], [2], [-0.15]) : ([False], [-0.7], [-0.15]) : [])
  output <- lws
  let output' = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob)) output
  liftS $ print $ show output'
  return output'

testLogRegrLWInf :: Sampler [((Double, Bool), [(Addr, OpenSum LW.Vals)], Double)]
testLogRegrLWInf = do
  -- Using fixed model parameters, generate some sample data points to learn
  output <- map fst <$> Basic.basic 1 Example.logisticRegression
                         (map (/50) [(-100) .. 100])
                         (repeat $ mkRecordLogRegr ([], [2], [-0.15]))
  let (xs, ys) = (map fst output, map snd output)
  -- Perform inference against these data points
  let lws' = LW.lw 3 Example.logisticRegression xs (map (mkRecordLogRegrL . (:[])) ys)
  output <- lws'
  let output' = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob)) output
  return output'

testLogRegrMHPost :: Sampler [((Double, Bool), [(Addr, OpenSum MH.Vals)], [(Addr, Double)])]
testLogRegrMHPost = do
  output <- map fst <$> Basic.basic 1 Example.logisticRegression
                         (map (/50) [(-100) .. 100])
                         (repeat $ mkRecordLogRegr ([], [2], [-0.15]))
  let (xs, ys) = (map fst output, map snd output)
  let mhs' = MH.mh 70 Example.logisticRegression
                         xs
                         (map (mkRecordLogRegrL . (:[])) ys)
  output <- mhs'
  let output' = map (\(xy, samples, logps) ->
       let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
           logps'   = Map.toList logps
       in  (xy, samples', logps') ) output
  -- liftS $ print $ show output'
  return output'

testLogRegrMHPred :: Sampler [(Double, Bool)]
testLogRegrMHPred = do
  mhTrace <- testLogRegrMHPost
  let postParams = map (fromJust . prj @Double . snd)
                      ((snd3 . head) mhTrace)
      (mu, postParams') = splitAt 1 postParams
      (b, _)        = splitAt 1 postParams'
  liftS $ print $ "mu is " ++ show mu ++ " b is " ++ show b
  let bs = Basic.basic 1 Example.logisticRegression
                         (map (/50) [(-100) .. 100])
                         (repeat $ mkRecordLogRegr ([], mu, b))
  map fst <$> bs

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
      bs = Basic.basic 1 (Example.nnModel1 3)
                         (map (/1) [0 .. 300])
                         (repeat $ mkRecordNN ([], [1, 5, 8],
                                                   [2, -5, 1],
                                                   [4.0]))
  output <- map fst <$> bs
  liftS $ print $ show output
  return output

testNNLWSim :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
testNNLWSim = do
  let xs  = concat [ replicate 11 x | x <- [0 .. 10]]
      -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a uniform area
      lws = LW.lw 1 (Example.nnModel1 3)
                    xs
                    (concat $ repeat $ map (\y -> mkRecordNN ([y], [1, 5, 8],
                                              [2, -5, 1],
                                              [2.0])) [0 .. 10])
  output <- lws
  let output' = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob)) output
  liftS $ print $ show output'
  return output'

testNNLWInf :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
testNNLWInf = do
  let -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a sine curve
      lws = LW.lw 1  (Example.nnModel1 3)
                      (map (/50) [0 .. 300])
                      (map (\y -> mkRecordNN ([y], [],
                                                   [],
                                                   []))
                           [ x | x <- map (/50) [0 .. 300] ])
  output <- lws
  let output' = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob)) output
  liftS $ print $ show output'
  return output'

-- Run this with nn-basic, as it returns a predictive distribution rather than a posterior one.
testNNMHPost :: Sampler [((Double, Double), [(Addr, OpenSum MH.Vals)], [(Addr, Double)])]
testNNMHPost = do
  let -- Run mh over data representing a line with gradient 1 and intercept 0
      mhs = MH.mh 40 (Example.nnModel1 3)
                    (map (/50) [0 .. 300])
                      (map mkRecordNNy
                           [ x | x <- map (/50) [0 .. 300] ])
  mhTrace <- mhs
  let mhTrace' = map (\(xy, samples, logps) ->
       let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
           logps'   = Map.toList logps
       in  (xy, samples', logps') ) mhTrace
  return mhTrace'

testNNMHPred :: Sampler [(Double, Double)]
testNNMHPred = do
  mhTrace <- testNNMHPost
  -- Get the most recent accepted model parameters from the posterior
  let postParams = map (fromJust . prj @Double . snd)
                       ((snd3 . head) mhTrace)
      (bias, postParams') = splitAt 3 postParams
      (weights, sigma)    = splitAt 3 postParams'
  liftS $ print $ show (weights, bias, sigma)
  -- Using these parameters, simulate data from the predictive. We can see that the predictive data becomes more accurate with more mh steps.
  let bs = Basic.basic 1 (Example.nnModel1 3)
                         (map (/1) [0 .. 300])
                         (repeat $ mkRecordNN ([], bias,
                                                   weights,
                                                   sigma))
  map fst <$> bs

{- Bayesian neural network 2 -}

testNNSinBasic :: Sampler  [(Double, Double)]
testNNSinBasic = do
  let -- Run basic simulation over neural network
      bs = Basic.basic 1 (Example.nnModel2 3)
                         (map (/1) [0 .. 300])
                         (repeat $ mkRecordNN ([], [1, 5, 8],
                                                   [2, -5, 1],
                                                   [4.0]))
  output <- map fst <$> bs
  liftS $ print $ show output
  return output

testNNSinLWSim :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
testNNSinLWSim = do
  let xs  = concat [ replicate 31 x | x <- [-10 .. 10]]
      -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a uniform area
      lws = LW.lw 1 (Example.nnModel2 3)
                    xs
                    (concat $ repeat $ map (\y -> mkRecordNN ([y], [1, 5, 8],
                                              [2, -5, 1],
                                              [2.0])) [-20 .. 10])
  output <- lws
  let output' = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob)) output
  liftS $ print $ show output'
  return output'

testNNSinLWInf :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
testNNSinLWInf = do
  let -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a sine curve
      lws = LW.lw 1  (Example.nnModel1 3)
                      (map (/50) [0 .. 300])
                      (map (\y -> mkRecordNN ([y], [1, 5, 8],
                                                   [2, -5, 1],
                                                   [2.0]))
                           [ sin x | x <- map (/50) [0 .. 300] ])
  output <- lws
  let output' = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob)) output
  liftS $ print $ show output'
  return output'

-- Run this with nn-basic, as it returns a predictive distribution rather than a posterior one.
testNNMHSin :: Sampler [(Double, Double)]
testNNMHSin = do
  let -- Run mh over data representing a sine curve
      mhs' = MH.mh 20  (Example.nnModel2 3)
                      (map (/20) [-200 .. 200])
                      (map mkRecordNNy
                           [ sin x | x <- map (/20) [-200 .. 200] ])
  output <- mhs'
  let output' = map (\(xy, samples, logps) ->
       let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
           logps'   = Map.toList logps
       in  (xy, samples', logps') ) output
  -- Get the most recent accepted model parameters from the posterior
  let postParams = map (fromJust . prj @Double . snd . snd)
                       ((Map.toList . snd3 . head) output)
      (bias, postParams') = splitAt 3 postParams
      (weights, sigma)    = splitAt 3 postParams'
  -- Using these parameters, simulate data from the predictive.
  let bs = Basic.basic 1 (Example.nnModel2 3)
                         ([-200 .. 200])
                        -- (map mkRecordNNy
                        --    [ sin x | x <- map (/20) [-200 .. 200] ])
                         (repeat $ mkRecordNN ([], bias,
                                                   weights,
                                                   sigma))
  liftS $ print $ show (weights, bias, sigma)
  -- return output'
  map fst <$> bs

{- Sine -}

testSinBasic :: Sampler [(Double, Double)]
testSinBasic = do
  let -- Simulate a sine curve
      output = Basic.basic 1  (Example.sineModel)
                       [0 .. 100]
                       (repeat $ mkRecordLinRegr ([], [0.1], [0], [0.1]))
  map fst <$> output

testSinLWSim :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
testSinLWSim = do
  -- Run sine model with fixed parameters, inputs, and outputs, to get likelihood of every data point over a uniform area
  let xs  = concat [ replicate 101 x | x <- [0 .. 100]]
      lws = LW.lw 1 Example.sineModel
                    xs
                    (concat $ repeat $ map (\y -> mkRecordLinRegr ([y], [0.1], [0], [0.1]))
                      (map (/100) [-100 .. 100]))
  output <- lws
  let output' = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob)) output
  liftS $ print $ show output'
  return output'

testSinLWInf :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
testSinLWInf = do
  -- Generate data points from sine model with fixed parameters
  let xs  = [  x | x <- [0 .. 100]]
      bs = Basic.basic 1 Example.sineModel
                    xs
                    (repeat $ mkRecordLinRegr ([], [0.1], [0], [0.1]))
  xys <- map fst <$> bs
  let (xs, ys) = (map fst xys, map snd xys)
  -- Perform inference against these data points to try and learn sine model parameters
  let lws = LW.lw 3 Example.sineModel xs (map (mkRecordLinRegrY . (:[])) ys)
  output <- lws
  let output' = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob)) output
  return output'


-- Run this with nn-basic, as it returns a predictive distribution rather than a posterior one.
testSinMHPost :: Sampler [((Double, Double), [(Addr, OpenSum MH.Vals)], [(Addr, Double)])]
testSinMHPost = do
  -- Generate data points from sine model with fixed parameters
  let xs  = [  x | x <- [0 .. 100]]
      bs = Basic.basic 1 Example.sineModel
                    xs
                    (repeat $ mkRecordLinRegr ([], [0.1], [0], [0.1]))
  xys <- map fst <$> bs
  let (xs, ys) = (map fst xys, map snd xys)
      -- Run mh over data representing a line with gradient 1 and intercept 0
      mhs = MH.mh 40 Example.sineModel xs (map (mkRecordLinRegrY . (:[])) ys)
  mhTrace <- mhs
  let mhTrace' = map (\(xy, samples, logps) ->
       let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
           logps'   = Map.toList logps
       in  (xy, samples', logps') ) mhTrace
  return mhTrace'
