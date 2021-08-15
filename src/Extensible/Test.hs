{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications, UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}

module Extensible.Test where

import Data.Maybe
import qualified Data.Map as Map
import qualified Extensible.Example as Example
import Extensible.DataSets
import Extensible.Dist
import qualified Extensible.Inference.Simulate as Simulate
import qualified Extensible.Inference.LW as LW
import qualified Extensible.Inference.MH as MH
import Extensible.OpenSum
import qualified Extensible.OpenSum as OpenSum
import Extensible.Inference.Inf
import Extensible.State
import Extensible.Model
import Extensible.Sampler
import Extensible.AffineReader
-- import Data.Extensible
import Extensible.OpenProduct
import Util
import Debug.Trace

{- Util -}

-- Draw the most recent sampled parameters from a post param mh trace
drawPredParam :: Tag -> [(Addr, [a])] -> [a]
drawPredParam tag xs = [ last v | ((t, i), v) <- xs, t == tag]

-- Returns a list of (addr, [p]) of all unique samples for addresses of interest in the mh trace
extractPostParams :: forall p a. (Eq p, Member p PrimVal) => Proxy p -> [Addr] -> [(a, [(Addr, OpenSum PrimVal)], [(Addr, Double)])] -> [(Addr, [p])]
extractPostParams _ addrs mhTrace =
  let sampleMap = map snd3 mhTrace
      paramTrace = [ (addr, xs) | addr <- addrs,
        let xs = map (\smap -> let p = fromJust $ lookup addr smap
                                   d = fromJust $ prj @p p
                               in  d) sampleMap ]
      paramTraceUnique = map (\(addr, xs) -> (addr, removeDuplicates xs)) paramTrace
  in  paramTraceUnique

processLWTrace :: [(a, Map.Map Addr (OpenSum PrimVal), Double)]
               -> [(a, [(Addr, OpenSum PrimVal)], Double)]
processLWTrace = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob))

processMHTrace :: [(a, Map.Map Addr (DistInfo, OpenSum PrimVal), Map.Map Addr Double)]
               -> [(a, [(Addr, OpenSum PrimVal)], [(Addr, Double)])]
processMHTrace = map (\(xy, samples, logps) ->
  let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
      logps'   = Map.toList logps
  in  (xy, samples', logps') )

{- Linear Regression -}

mkRecordLinRegr :: ([Double],  [Double],  [Double],  [Double]) -> LRec Example.LinRegrEnv
mkRecordLinRegr (y_vals, m_vals, c_vals, σ_vals) =
  (#y :> y_vals) <: (#m :> m_vals) <: (#c :> c_vals) <: (#σ :> σ_vals) <: nil

mkRecordLinRegrY :: [Double] -> LRec Example.LinRegrEnv
mkRecordLinRegrY y_vals =
  (#y @= y_vals) <: (#m @= []) <: (#c @= []) <: (#σ @= []) <: nil

testLinRegrBasic :: Sampler [(Double, Double)]
testLinRegrBasic = do
  let n_samples = 3
      -- Run simulate simulation over linearRegression
      {- This should generate a set of points on the y-axis for each given point on the x-axis -}
  bs <- Simulate.simulate n_samples Example.linearRegression
                    [0, 1, 2, 3, 4]
                    (repeat $ mkRecordLinRegr ([], [1.0], [0.0], [1.0]))
      {- This should output the provided fixed set of data points on the x and y axis. -}
  bs' <- Simulate.simulate n_samples Example.linearRegression
                    [0, 1, 2, 3, 4]
                    (map mkRecordLinRegrY [[-0.3], [0.75], [2.43], [3.5], [3.2]])
  return $ bs


testLinRegrLWSim :: Sampler [((Double, Double), [(Addr, OpenSum PrimVal)], Double)]
testLinRegrLWSim = do
  -- Run linear model with fixed parameters, inputs, and outputs, to get likelihood of every data point over a uniform area
  let lw_n_iterations = 3
      xs  = concat [ replicate 11 x | x <- [0 .. 10]]
  lwTrace <- LW.lw lw_n_iterations Example.linearRegression
                    xs
                    (concat $ repeat $ map (\y -> mkRecordLinRegr ([y], [3], [2], [2.5]))
                      [0 .. 10])
  let lwTrace' = processLWTrace lwTrace
  return lwTrace'

-- | [(datapoints, samples, likelihood)]
testLinRegrLWInf :: Sampler [((Double, Double), [(Addr, OpenSum PrimVal)], Double)]
testLinRegrLWInf = do
  -- Run likelihood weighting inference over linearRegression
  {- This should output the provided fixed set of data points on the x and y axis, where each point has a different probability (due to us observing the probability of given y's). Also returns a trace of parameters and their likelihoods -}
  let  lw_n_iterations = 100
  lwTrace <- LW.lw lw_n_iterations Example.linearRegression
                    [0 .. 100]
                    (map (mkRecordLinRegrY . (:[]) ) (map ((+2) . (*3)) [0 .. 100]))
  let lwTrace' = processLWTrace lwTrace
  return lwTrace'

-- | Returns trace of model parameter samples
testLinRegrMHPost :: Sampler [(Addr, [Double])]
testLinRegrMHPost = do
  -- Run mh inference over linearRegression for data representing a line with gradient 3 and intercept 4
  let mh_n_iterations = 500
  mhTrace <- MH.mh mh_n_iterations Example.linearRegression [] [0 .. 100]
                    (map (mkRecordLinRegrY . (:[]) ) (map ((+4) . (*3)) [0 .. 100]))
  -- Reformat MH trace
  let mhTrace'  = processMHTrace mhTrace
      postParams = extractPostParams (Proxy @Double) [("m", 0), ("c", 0), ("σ", 0)] mhTrace'
  return postParams

-- | Use mh posterior to output predictive distribution
testLinRegrMHPred :: Sampler [(Double, Double)]
testLinRegrMHPred = do
  mhTrace <- testLinRegrMHPost
  -- Get the most recent accepted model parameters from the posterior
  let mu         = drawPredParam "m" mhTrace
      c          = drawPredParam "c" mhTrace
      sigma      = drawPredParam "σ" mhTrace
  liftS $ print $ "Using parameters " ++ show (mu, c, sigma)
  -- Using these parameters, simulate data from the predictive.
  bs <- Simulate.simulate 1 Example.linearRegression
                         (map (/1) [0 .. 100])
                         (repeat $ mkRecordLinRegr ([], mu, c, sigma))
  return $ bs

{- Logistic Regression -}
mkRecordLogRegr :: ([Bool], [Double], [Double]) -> LRec Example.LogRegrEnv
mkRecordLogRegr (label_vals, m_vals, b_vals) =
  #label @= label_vals <: #m @= m_vals <: #b @= b_vals <: nil

mkRecordLogRegrL :: [Bool] -> LRec Example.LogRegrEnv
mkRecordLogRegrL label_val =
 #label @= label_val <: #m @= [] <: #b @= [] <: nil

testLogRegrBasic :: Sampler [(Double, Bool)]
testLogRegrBasic = do
  -- This should generate a set of points on the y-axis for each given point on the x-axis
  let n_samples = 1
  bs <- Simulate.simulate n_samples Example.logisticRegression
                         (map (/50) [(-100) .. 100])
                         (repeat $ mkRecordLogRegr ([], [2], [-0.15]))
  -- This should output the provided fixed set of data points on the x and y axis.
  bs' <- Simulate.simulate 3 Example.logisticRegression
                         [0, 1, 2, 3, 4]
                         (map mkRecordLogRegrL [[False], [False], [True], [False], [True]])
  return $ bs

testLogRegrLWSim :: Sampler [((Double, Bool), [(Addr, OpenSum PrimVal)], Double)]
testLogRegrLWSim = do
  -- Run logistic model with fixed parameters, inputs, and outputs, to get likelihood of every data point over a uniform area
  bs <- Simulate.simulate 1 Example.logisticRegression
                         (map (/50) [(-100) .. 100])
                         (repeat $ mkRecordLogRegr ([], [2], [-0.15]))
  let (xs, ys) = unzip bs
  let lw_n_iterations = 3
  lwTrace <- LW.lw lw_n_iterations Example.logisticRegression
             xs (map (\y -> mkRecordLogRegr ([y], [2], [-0.15])) ys)
  let lwTrace' = processLWTrace lwTrace
  return lwTrace'

testLogRegrLWInf :: Sampler [((Double, Bool), [(Addr, OpenSum PrimVal)], Double)]
testLogRegrLWInf = do
  -- Using fixed model parameters, generate some sample data points to learn
  let n_samples = 1
  bs <- Simulate.simulate n_samples Example.logisticRegression
                         (map (/50) [(-100) .. 100])
                         (repeat $ mkRecordLogRegr ([], [2], [-0.15]))
  let (xs, ys) = (map fst bs, map snd bs)
  -- Perform inference against these data points
  lwTrace <- LW.lw 10 Example.logisticRegression xs (map (mkRecordLogRegrL . (:[])) ys)
  let lwTrace' = processLWTrace lwTrace
  return lwTrace'

testLogRegrMHPost :: Sampler [(Addr, [Double])]
testLogRegrMHPost = do
  let n_samples = 1
  bs <- Simulate.simulate n_samples Example.logisticRegression
                         (map (/50) [(-100) .. 100])
                         (repeat $ mkRecordLogRegr ([], [2], [-0.15]))
  let (xs, ys) = (map fst bs, map snd bs)
      mh_n_iterations = 70
  mhTrace <- MH.mh mh_n_iterations Example.logisticRegression []
                   xs (map (mkRecordLogRegrL . (:[])) ys)
  let mhTrace' = processMHTrace mhTrace
      postParams = extractPostParams (Proxy @Double) [("m", 0), ("b", 0)] mhTrace'
  return postParams

testLogRegrMHPred :: Sampler [(Double, Bool)]
testLogRegrMHPred = do
  mhTrace <- testLogRegrMHPost
  let mu         = drawPredParam "m" mhTrace
      b          = drawPredParam "b" mhTrace
  liftS $ print $ "Using parameters " ++ show (mu, b)
  bs <- Simulate.simulate 1 Example.logisticRegression
                      (map (/50) [(-100) .. 100])
                      (repeat $ mkRecordLogRegr ([], mu, b))
  return $ bs

-- {- Bayesian Neural Network for linear regression -}

mkRecordNN :: ([Double], [Double], [Double], [Double])
           -> LRec Example.NNEnv
mkRecordNN (yobs_vals, weight_vals, bias_vals, sigm_vals) =
  #yObs @= yobs_vals <: #weight @= weight_vals <: #bias @= bias_vals <: #sigma @= sigm_vals <: nil

mkRecordNNy :: Double
           -> LRec Example.NNEnv
mkRecordNNy yobs_val =
  #yObs @= [yobs_val] <: #weight @= [] <: #bias @= [] <: #sigma @= [] <: nil

testNNLinBasic :: Sampler  [(Double, Double)]
testNNLinBasic = do
  -- Run simulate simulation over neural network
  bs <- Simulate.simulate 1 (Example.nnLinModel 3)
                         (map (/1) [0 .. 300])
                         (repeat $ mkRecordNN ([], [1, 5, 8],
                                                   [2, -5, 1],
                                                   [4.0]))
  return $ bs

testNNLinLWSim :: Sampler [((Double, Double), [(Addr, OpenSum PrimVal)], Double)]
testNNLinLWSim = do
  -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a uniform area
  let xs  = concat [ replicate 11 x | x <- [0 .. 10]]
  lwTrace <- LW.lw 1 (Example.nnLinModel 3)
                    xs
                    (concat $ repeat $ map (\y -> mkRecordNN ([y], [1, 5, 8],
                                              [2, -5, 1],
                                              [2.0])) [0 .. 10])
  let lwTrace' = processLWTrace lwTrace
  return lwTrace'

testNNLinLWInf :: Sampler [((Double, Double), [(Addr, OpenSum PrimVal)], Double)]
testNNLinLWInf = do
  -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a sine curve
  lwTrace <- LW.lw 1  (Example.nnLinModel 3)
                      (map (/50) [0 .. 300])
                      (map (\y -> mkRecordNN ([y], [], [], []))
                           [ x | x <- map (/50) [0 .. 300] ])
  let lwTrace' = processLWTrace lwTrace
  return lwTrace'

-- Run this with nn-simulate, as it returns a predictive distribution rather than a posterior one.
testNNLinMHPost :: Sampler [(Addr, [Double])]
testNNLinMHPost = do
  -- Run mh over data representing a line with gradient 1 and intercept 0
  let mh_n_iterations = 40
  mhTrace <- MH.mh mh_n_iterations (Example.nnLinModel 3) []
                    (map (/50) [0 .. 300])
                    (map mkRecordNNy [ x | x <- map (/50) [0 .. 300] ])
  let mhTrace' = processMHTrace mhTrace
      matrix_size = 3
      addrs      = [ ("weight", i) | i <- [0..(matrix_size - 1)]] ++
                   [ ("bias", i)   | i <- [0..(matrix_size - 1)]] ++
                   [ ("sigma", 0)]
      postParams = extractPostParams (Proxy @Double) addrs mhTrace'
  return postParams

testNNLinMHPred :: Sampler [(Double, Double)]
testNNLinMHPred = do
  mhTrace <- testNNLinMHPost
  -- Get the most recent accepted model parameters from the posterior
  let weights    = drawPredParam "weight" mhTrace
      bias       = drawPredParam "bias" mhTrace
      sigma      = drawPredParam "sigma" mhTrace
  liftS $ print $ "Using parameters: " ++ show (weights, bias, sigma)
  -- Using these parameters, simulate data from the predictive. We can see that the predictive data becomes more accurate with more mh steps.
  bs <- Simulate.simulate 1 (Example.nnLinModel 3)
                         (map (/1) [0 .. 300])
                         (repeat $ mkRecordNN ([], bias,
                                                   weights,
                                                   sigma))
  return $ bs

{- Bayesian neural network v2 -}
testNNStepBasic :: Sampler  [(Double, Double)]
testNNStepBasic = do
  let n_samples = 1 -- Run simulate simulation over neural network
  bs <- Simulate.simulate n_samples (Example.nnStepModel 3)
                              (map (/1) [-100 .. 100])
                              (repeat $ mkRecordNN ([], [1, 5, 8],
                                                        [2, -5, 1],
                                                        [4.0]))
  return $ bs

testNNStepLWSim :: Sampler [((Double, Double), [(Addr, OpenSum PrimVal)], Double)]
testNNStepLWSim = do
  let xs  = concat [ replicate 31 x | x <- [-10 .. 10]]
      -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a uniform area
  lwTrace <- LW.lw 1 (Example.nnStepModel 3)
                    xs
                    (concat $ repeat $ map (\y -> mkRecordNN ([y], [1, 5, 8],
                                              [2, -5, 1],
                                              [2.0])) [-20 .. 10])
  let lwTrace' = processLWTrace lwTrace
  return lwTrace'

testNNStepLWSim2 :: Sampler [((Double, Double), [(Addr, OpenSum PrimVal)], Double)]
testNNStepLWSim2 = do
  -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a sine curve
  lwTrace <- LW.lw 1  (Example.nnLinModel 3)
                      (map (/50) [-200 .. 200])
                      (map (\y -> mkRecordNN ([y], [1, 5, 8],
                                                   [2, -5, 1],
                                                   [2.0]))
                           [ sin x | x <- map (/50) [-200 .. 200] ])
  let lwTrace' = processLWTrace lwTrace
  return lwTrace'

testNNStepLWInf :: Sampler [((Double, Double), [(Addr, OpenSum PrimVal)], Double)]
testNNStepLWInf = do
  -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a sine curve
  lwTrace <- LW.lw 1  (Example.nnLinModel 3)
                      (map (/50) [-200 .. 200])
                      (map (\y -> mkRecordNN ([y], [], [], []))
                           [ sin x | x <- map (/50) [-200 .. 200] ])
  let lwTrace' = processLWTrace lwTrace
  return lwTrace'

testNNStepMHPost :: Sampler [(Addr, [Double])]
testNNStepMHPost = do
  mhTrace <- MH.mh 20  (Example.nnStepModel 3) []
                       (map (/20) [-200 .. 200])
                       (map mkRecordNNy
                           [  x | x <- map (/20) [-200 .. 200] ])
  let mhTrace' = processMHTrace mhTrace
      matrix_size = 3
      addrs      = [ ("weight", i) | i <- [0..(matrix_size - 1)]] ++
                   [ ("bias", i)   | i <- [0..(matrix_size - 1)]] ++
                   [ ("sigma", 0)]
      postParams = extractPostParams (Proxy @Double) addrs mhTrace'
  return postParams

testNNStepMHPred :: Sampler [(Double, Double)]
testNNStepMHPred = do
  mhTrace <- testNNStepMHPost
  -- Get the most recent accepted model parameters from the posterior
  let weights    = drawPredParam "weight" mhTrace
      bias       = drawPredParam "bias" mhTrace
      sigma      = drawPredParam "sigma" mhTrace
  -- Using these parameters, simulate data from the predictive.
  liftS $ print $ "Using parameters: " ++ show (weights, bias, sigma)
  bs <- Simulate.simulate 1 (Example.nnStepModel 3)
                         [-200 .. 200]
                         (repeat $ mkRecordNN ([], bias,
                                                   weights,
                                                   sigma))
  return $ bs

-- | Another neural network variation for logistic regression
mkRecordNNLog :: ([Bool], [Double])
           -> LRec Example.NNLogEnv
mkRecordNNLog (yobs_vals, weight_vals) =
  #yObs @= yobs_vals <: #weight @= weight_vals <: nil

mkRecordNNLogy :: Bool -> LRec Example.NNLogEnv
mkRecordNNLogy yobs_val =
  #yObs @= [yobs_val] <: #weight @= [] <: nil

testNNLogBasic :: Sampler [((Double, Double), Bool)]
testNNLogBasic = do
  let -- Run simulate simulation over neural network
      w1 = [0.18, 0.36, -1.29, 0.094, -1.64, 0.65]
      w2 = [0.147, -0.417, -0.278, -1.275,0.568,-0.785,0.074,0.351,0.732]
      w3 = [0.295, 0.414, -0.834]
  bs <- Simulate.simulate 1 (Example.nnLogModel 3)
                      nnLogX
                      (repeat $ mkRecordNNLog ([], w1 ++ w2 ++ w3))
  return $ bs

testNNLogBasic' :: Sampler [((Double, Double), Bool)]
testNNLogBasic' = do
  let -- Run simulate simulation over neural network
      w1 = [0.18, 0.36, -1.29, 0.094, -1.64, 0.65]
      w2 = [0.147, -0.417, -0.278, -1.275,0.568,-0.785,0.074,0.351,0.732]
      w3 = [0.295, 0.414, -0.834]
  bs <- Simulate.simulate 1 (Example.nnLogModel 3)
                      nnLogX
                      (map (\y -> mkRecordNNLog ([y], w1 ++ w2 ++ w3)) nnLogY)
  return $ bs

testNNLogMHPost :: Sampler [(Addr, [Double])]
testNNLogMHPost = do
  mhTrace <- MH.mh 200 (Example.nnLogModel 3) []
                      nnLogX
                      (map mkRecordNNLogy nnLogY)
  let weight_length = 18
      mhTrace'   = processMHTrace mhTrace
      addrs      = [ ("weight", i) | i <- [0..(weight_length - 1)]]
      postParams = extractPostParams (Proxy @Double) addrs mhTrace'
  return postParams

testNNLogMHPred :: Sampler [((Double, Double), Bool)]
testNNLogMHPred = do
  mhTrace <- testNNLogMHPost
  let weights = drawPredParam "weight" mhTrace
  bs <- Simulate.simulate 1 (Example.nnLogModel 3)
                         nnLogX
                         (repeat $ mkRecordNNLog ([], weights))
  return $ bs

{- Sine Model -}
testSinBasic :: Sampler [(Double, Double)]
testSinBasic = do
  -- Simulate a sine curve
  bs <- Simulate.simulate 1 Example.sineModel
                       (map (/50) [0 .. 200])
                       (repeat $ mkRecordLinRegr ([], [5], [0], [0.1]))
  return $ bs

testSinLWSim :: Sampler [((Double, Double), [(Addr, OpenSum PrimVal)], Double)]
testSinLWSim = do
  -- Run sine model with fixed parameters, inputs, and outputs, to get likelihood of every data point over a uniform area
  let xs  = concat [ replicate 101 x | x <- [0 .. 100]]
  lwTrace <- LW.lw 1 Example.sineModel
                    xs
                    (concat $ repeat $ map (\y -> mkRecordLinRegr ([y], [0.1], [0], [0.1]))
                    (map (/100) [-100 .. 100]))
  let lwTrace' = processLWTrace lwTrace
  return lwTrace'

testSinLWInf :: Sampler [((Double, Double), [(Addr, OpenSum PrimVal)], Double)]
testSinLWInf = do
  -- Generate data points from sine model with fixed parameters
  let xs = map (/50) [0 .. 200]
  bs <- Simulate.simulate 1 Example.sineModel
                    xs
                    (repeat $ mkRecordLinRegr ([], [2], [0], [0.1]))
  let (xs, ys) = (map fst bs, map snd bs)
  -- Perform inference against these data points to try and learn sine model parameters
  lwTrace <- LW.lw 3 Example.sineModel xs (map (mkRecordLinRegrY . (:[])) ys)
  let lwTrace' = processLWTrace lwTrace
  return lwTrace'

testSinMHPost :: Sampler [(Addr, [Double])]
testSinMHPost = do
  -- Generate data points from sine model with fixed parameters
  let xs = map (/50) [0 .. 200]
  bs <- Simulate.simulate 1 Example.sineModel
                    xs
                    (repeat $ mkRecordLinRegr ([], [5], [-2], [0.1]))
  let (xs, ys) = (map fst bs, map snd bs)
  -- Run mh over data representing a sine wave
  mhTrace <- MH.mh 100 Example.sineModel [] xs (map (mkRecordLinRegrY . (:[])) ys)
  let mhTrace'   = processMHTrace mhTrace
      postParams = extractPostParams (Proxy @Double)  [("m", 0), ("c", 0), ("σ", 0)] mhTrace'
  return postParams

-- | Use mh posterior to output predictive distribution
testSinMHPred :: Sampler [(Double, Double)]
testSinMHPred = do
  mhTrace <- testSinMHPost
  -- Get the most recent accepted model parameters from the posterior
  let mu         = drawPredParam "m" mhTrace
      c          = drawPredParam "c" mhTrace
      sigma      = drawPredParam "σ" mhTrace
      xs         = map (/50) [0 .. 200]
  liftS $ putStrLn $ "mu c sigma" ++ show (mu, c, sigma)
  -- Using these parameters, simulate data from the predictive.
  bs <- Simulate.simulate 1 Example.sineModel
                         xs
                         (repeat $ mkRecordLinRegr ([], mu, c, sigma))
  return $ bs

{- Hidden markov model -}

mkRecordHMM :: ([Int], Double, Double) -> LRec Example.HMMEnv
mkRecordHMM (ys, transp, obsp) = #y @= ys <: #trans_p @= [transp] <: #obs_p @= [obsp] <:  nil

mkRecordHMMy :: [Int] -> LRec Example.HMMEnv
mkRecordHMMy ys = #y @= ys <: #trans_p @= [] <: #obs_p @= [] <:  nil

testHMMBasic :: Sampler [([Int], [Int])]
testHMMBasic = do
  let hmm_n_steps   = 100
      hmm_n_samples = 10
  bs <- Simulate.simulate hmm_n_samples (Example.hmmNSteps hmm_n_steps)
                                   [0] [mkRecordHMM ([], 0.5, 0.5)]
  return $ bs

testHMMLWSim :: Sampler [(([Int], [Int]), [(Addr, OpenSum PrimVal)], Double)]
testHMMLWSim = do
  let hmm_n_steps    = 10
      hmm_n_samples  = 10
  lwTrace <- LW.lw 1 (Example.hmmNSteps hmm_n_steps)
                 (replicate hmm_n_samples 0)
                 (map ((\ys -> mkRecordHMM (ys, 0.5, 0.5)) . replicate hmm_n_steps)
                  [0 .. hmm_n_samples])
  let lwTrace' = processLWTrace lwTrace
  return lwTrace'

testHMMLWInf :: Sampler [(([Int], [Int]), [(Addr, OpenSum PrimVal)], Double)]
testHMMLWInf = do
  let hmm_n_steps   = 10
      hmm_n_samples = 10
  bs <- Simulate.simulate hmm_n_samples (Example.hmmNSteps hmm_n_steps)
                    [0] [mkRecordHMM ([], 0.9, 0.1)]
  let lw_n_iterations = 100
      (_, yss) = unzip bs
  lwTrace <- LW.lw lw_n_iterations  (Example.hmmNSteps hmm_n_steps)
                                (replicate hmm_n_samples 0)
                                (map mkRecordHMMy yss)
  let lwTrace' = processLWTrace lwTrace
  return lwTrace'

testHMMMHPost :: Sampler [(Addr, [Double])]
testHMMMHPost = do
  let hmm_n_steps   = 20
      hmm_n_samples = 30
  bs <- Simulate.simulate hmm_n_samples (Example.hmmNSteps hmm_n_steps)
                    [0] [mkRecordHMM ([], 0.8, 0.1)]
  let mh_n_iterations = 1000
      (_, yss)  = unzip bs
  mhTrace <- MH.mh mh_n_iterations (Example.hmmNSteps hmm_n_steps) ["trans_p", "obs_p"]
                                   (replicate hmm_n_samples 0)
                                   (map mkRecordHMMy yss)
  let mhTrace'   = processMHTrace mhTrace
      postParams = extractPostParams (Proxy @Double)  [("trans_p", 0), ("obs_p", 0)] mhTrace'
  liftS $ print $ "trace is " ++ show (map snd3 mhTrace')
  return postParams

testHMMMHPred :: Sampler [([Int], [Int])]
testHMMMHPred = do
  mhTrace <- testHMMMHPost
  let hmm_n_steps   = 10
      hmm_n_samples = 100
      trans_p    = drawPredParam "trans_p" mhTrace
      obs_p      = drawPredParam "obs_p" mhTrace
  liftS $ print $ "using parameters " ++ show (trans_p, obs_p)
  bs <- Simulate.simulate hmm_n_samples (Example.hmmNSteps hmm_n_steps)
                    [0] [mkRecordHMM ([], head trans_p, head obs_p)]
  return $ bs

testHMMStBasic :: Sampler [([Int], [Int])]
testHMMStBasic = do
  bs <- Simulate.simulate 2
            (runStateM . Example.hmmNStepsSt 0.5 0.5 10)
                         [0] (repeat $ mkRecordHMM ([], 0.5, 0.5))
  return $ bs

{- Hidden markov model : SIR -}

mkRecordSIR :: ([Double], [Double], [Double]) -> LRec Example.SIREnv
mkRecordSIR (ρv, βv, γv) = #infobs @= [] <: #ρ @= ρv <: #β @= βv <: #γ @= γv <: nil

mkRecordSIRy :: [Int] -> LRec Example.SIREnv
mkRecordSIRy ys = #infobs @= ys <: #ρ @= [] <: #β @= [] <: #γ @= [] <: nil

fixedParams :: Int -> Int -> Example.FixedParams
fixedParams = Example.FixedParams

latentState :: Int -> Int -> Int -> Example.LatentState
latentState = Example.LatentState

fromLatentState :: Example.LatentState -> (Int, Int, Int)
fromLatentState (Example.LatentState sus inf recov) = (sus, inf, recov)

testSIRBasic :: Sampler ([(Int, Int, Int)], [Int])
testSIRBasic = do
  bs <- Simulate.simulate 1
          (Example.hmmSIRNsteps (fixedParams 763 1) 100)
          [latentState 762 1 0] [mkRecordSIR ([0.3], [0.7], [0.009])]
          --[mkRecordSIR ([0.29], [0.25], [0.015])]
  let output = map (\(xs, ys) -> (map fromLatentState xs, ys)) bs
  return $ head output

testSIRLWInf :: Sampler [(([(Int, Int, Int)], [Int]), [(Addr, OpenSum PrimVal)], Double)]
testSIRLWInf = do
  let sir_n_steps    = length sirInfectedData
  lwTrace <- LW.lw 100 (Example.hmmSIRNsteps (fixedParams 763 1) sir_n_steps)
                 [latentState 762 1 0] [mkRecordSIRy sirInfectedData]
  let lwTrace' = processLWTrace lwTrace
  liftS $ print $ show lwTrace'
  let output' =
        map (\((xs, ys), sampleMap, p) -> ((map fromLatentState xs, ys), sampleMap, p)) lwTrace'
  return output'

testSIRMHPost :: Sampler [(Addr, [Double])]
testSIRMHPost = do
  let sir_n_samples = 10
  bs <- Simulate.simulate sir_n_samples
          (Example.hmmSIRNsteps (fixedParams 763 1) 30)
          [latentState 762 1 0] [mkRecordSIR ([0.3], [0.7], [0.009])]
  let infectedData    = map snd bs
      mh_n_iterations = 2000
  liftS $ print $ "infected data is " ++ show infectedData
  -- This demonstrates well the need for specifying the sample sites ["ρ", "β", "γ"].
  mhTrace <- MH.mh mh_n_iterations (Example.hmmSIRNsteps (fixedParams 763 1) 200) ["ρ", "β", "γ"]
                        (replicate sir_n_samples $ latentState 762 1 0)
                        (map mkRecordSIRy infectedData)
  let mhTrace'    = processMHTrace mhTrace
      postParams  = extractPostParams (Proxy @Double)  [("ρ", 0), ("β", 0), ("γ", 0)] mhTrace'
  return postParams

testSIRMHPred :: Sampler ([(Int, Int, Int)], [Int])
testSIRMHPred = do
  mhTrace <- testSIRMHPost
  let ρ    = drawPredParam "ρ" mhTrace
      β    = drawPredParam "β" mhTrace
      γ    = drawPredParam "γ" mhTrace
  liftS $ print $ show (ρ, β, γ)
  bs <- Simulate.simulate 1
                    (Example.hmmSIRNsteps (fixedParams 763 1) 200)
                    [latentState 762 1 0] [mkRecordSIR (ρ, β, γ)]
  let output = map (\(xs, ys) -> (map fromLatentState xs, ys)) bs
  liftS $ print $ show (bs)
  return $ head output

-- | Testing random distributions
mkRecordDir :: [[Double]] -> LRec Example.DirEnv
mkRecordDir ds = #xs @= ds <: nil

-- testHalfNormal :: Sampler [String]
testHalfNormal = do
--  map fst <$> Simulate.simulate 5 Example.halfNorm [1] [mkRecordDir [0.3889326877819943,0.6110673122180057]]
  LW.lw 1 Example.halfNorm [1] [mkRecordDir [[0.3889326877819943,0.6110673122180057]]]
  -- let p = prob (HalfNormalDist 1 Nothing Nothing) (0)
  -- let p' = prob (NormalDist 0 1 Nothing Nothing) 0
  -- return (p, p')

-- | Topic model over single document
mkRecordTopic :: ([[Double]], [[Double]], [String]) -> LRec Example.TopicEnv
mkRecordTopic (tps, wps, ys) =  #topic_ps @= tps <:  #word_ps @= wps <: #word @= ys <:nil

testTopicBasic :: Sampler [[String]]
testTopicBasic = do
  Simulate.simulate 1 (Example.documentDist vocabulary 2)
                        [10] [mkRecordTopic ([[0.5, 0.5]], [[0.12491280814569208,1.9941599739151505e-2,0.5385152817942926,0.3166303103208638],[1.72605174564027e-2,2.9475900240868515e-2,9.906011619752661e-2,0.8542034661052021]], [])]

testTopicMHPost :: Sampler [(Addr, [[Double]])]
testTopicMHPost = do
  mhTrace <- MH.mh 100 (Example.documentDist vocabulary 2) ["word_ps", "topic_ps"]
                       [10] [mkRecordTopic ([], [], document1)]
  let mhTrace' = processMHTrace mhTrace
      paramTrace = extractPostParams (Proxy @[Double])  [("topic_ps", 0), ("word_ps", 0), ("word_ps", 1)] mhTrace'
  return paramTrace

testTopicMHPred :: Sampler [[String]]
testTopicMHPred = do
  mhTrace <- testTopicMHPost
  let  topic_ps   = drawPredParam "topic_ps" mhTrace
       word_ps    = drawPredParam "word_ps" mhTrace
  liftS $ print $ "Using params " ++ show (topic_ps, word_ps)
  Simulate.simulate 1 (Example.documentDist vocabulary 2) [10]
        [mkRecordTopic (topic_ps,  word_ps, [])]

-- | Topic model over multiple (two) documents
testTopicsMHPost :: Sampler [(Addr, [[Double]])]
testTopicsMHPost = do
  mhTrace <- MH.mh 1000 (Example.topicModel vocabulary 2) ["word_ps", "topic_ps"]
                       [[10, 10]] [mkRecordTopic ([], [], concat corpus)]
  let mhTrace' = processMHTrace mhTrace
      paramTrace = extractPostParams (Proxy @[Double])  [("topic_ps", 0), ("topic_ps", 1), ("word_ps", 0), ("word_ps", 1), ("word_ps", 2), ("word_ps", 3)] mhTrace'
  return paramTrace

-- | Hierchical linear regression
mkRecordHLR :: ([Double], [Double], [Double], [Double], [Double], [Double], [Double]) -> LRec Example.HLREnv
mkRecordHLR (mua, mub, siga, sigb, a, b, lograds) = #mu_a @= mua <: #mu_b @= mub <: #sigma_a @= siga <: #sigma_b @= sigb <: #a @= a <: #b @= b <: #log_radon @= lograds <: nil

-- testHLRBasic :: Sampler [[Double]]
testHLRBasic :: Sampler ([Double], [Double])
testHLRBasic = do
  bs <- Simulate.simulate 1 (Example.hierarchicalLinRegr n_counties dataFloorValues countyIdx)
                    [()] [mkRecordHLR ([1.45], [-0.68], [0.3], [0.2], [], [], [])]
  let basementIdxs      = findIndexes dataFloorValues 0
      noBasementIdxs    = findIndexes dataFloorValues 1
      basementPoints    = map (head bs !!) basementIdxs
      nobasementPoints  = map (head bs !!) noBasementIdxs
  return (basementPoints, nobasementPoints)

-- testHLRMHPost :: Sampler  [([Double], [(Addr, OpenSum PrimVal)], [(Addr, Double)])]
testHLRMHPost :: Sampler  [(Addr, [Double])]
testHLRMHPost = do
  mhTrace <- MH.mh 3000 (Example.hierarchicalLinRegr n_counties dataFloorValues countyIdx) ["mu_a", "mu_b", "sigma_a", "sigma_b"]
                    [()] [mkRecordHLR ([], [], [], [], [], [], logRadon)]
  let mhTrace' = processMHTrace mhTrace
  -- liftS $ print $ show $ map snd3 mhTrace'
  let paramTrace = extractPostParams (Proxy @Double)  [("mu_a", 0), ("mu_b", 0), ("sigma_a", 0), ("sigma_b", 0) ] mhTrace'
  liftS $ print paramTrace
  return paramTrace

testHLRMHPredictive :: Sampler  ([Double], [(Addr, OpenSum PrimVal)], [(Addr, Double)])
testHLRMHPredictive = do
  mhTrace <- MH.mh 1500 (Example.hierarchicalLinRegr n_counties dataFloorValues countyIdx)
             ["mu_a", "mu_b", "sigma_a", "sigma_b"] [()] [mkRecordHLR ([], [], [], [], [], [], logRadon)]
  let mhTrace' = processMHTrace mhTrace
  liftS $ print $ show $ map snd3 mhTrace'
  -- Only returning the last of the mh trace here
  return (last mhTrace')

{- Gaussian Mixture Model -}
mkRecordGMM :: ([Double], [Double], [Double], [Double]) -> LRec Example.GMMEnv
mkRecordGMM (mus, mu_ks, xs, ys) = #mu @= mus <: #mu_k @= mu_ks <: #x @= xs <: #y @= ys <: nil

testGMMBasic :: Sampler [[((Double, Double), Int)]]
testGMMBasic = do
  bs <- Simulate.simulate 30 (Example.gmm 2) [20] [mkRecordGMM ([-2.0, 3.5], [], [], [])]
  return $  bs

testGMMMHPost :: Sampler [(Addr, [Double])]
testGMMMHPost = do
  bs <- testGMMBasic
  let xys = map unzip (map2 fst bs)
  mhTrace <- MH.mh 2000 (Example.gmm 2) [] (repeat 20)
                (map (\(xs, ys) ->  mkRecordGMM ([], [], xs, ys)) xys)
  let mhTrace'   = processMHTrace mhTrace
      paramTrace = extractPostParams (Proxy @Double)  [("mu", 0), ("mu", 1)] mhTrace'
  return paramTrace

{- School model -}
mkRecordSch :: ([Double], [[Double]], [Double]) -> LRec Example.SchEnv
mkRecordSch (mu, theta, ys) = #mu @= mu <: #theta @= theta <: #y @= ys <: nil

testSchBasic :: Sampler [[Double]]
testSchBasic = do
  let n_schools = 8
      ys        = [28, 8, -3,   7, -1,  1, 18, 12]
      sigmas    = [15, 10, 16, 11,  9, 11, 10, 18]
  bs <- Simulate.simulate 1 (Example.schoolModel n_schools)
          [sigmas] [mkRecordSch ([], [], ys)]
  return $ bs

testSchMHPost :: Sampler ([(Addr, [Double])], [(Addr, [[Double]])])
testSchMHPost = do
  let n_schools = 8
      ys        = [28, 8, -3,   7, -1,  1, 18, 12]
      sigmas    = [15, 10, 16, 11,  9, 11, 10, 18]
  mhTrace <- MH.mh 2000 (Example.schoolModel n_schools) []
              [sigmas] [mkRecordSch ([], [], ys)]
  let mhTrace'   = processMHTrace mhTrace
      thetas     = map fst3 mhTrace
      muTrace    = extractPostParams (Proxy @Double) [("mu", 0)] mhTrace'
      thetaTrace = extractPostParams (Proxy @[Double]) [("theta", 0)] mhTrace'
  return (muTrace, thetaTrace)