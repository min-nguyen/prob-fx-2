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

module Extensible.Test where

import Data.Maybe
import qualified Data.Map as Map
import qualified Extensible.Example as Example
import Extensible.DataSets
import Extensible.Dist
import qualified Extensible.Inference.Basic as Basic
import qualified Extensible.Inference.LW as LW
import qualified Extensible.Inference.MH as MH
import Extensible.OpenSum
import Extensible.Inference.Inf
import Extensible.State
import Extensible.Model
import Extensible.Sampler
import Extensible.AffineReader
-- import Data.Extensible
import Extensible.OpenProduct
import Util

getPostParams :: [Addr] -> [(a, [(Addr, OpenSum MH.Vals)], [(Addr, Double)])] -> [(Addr, Double)]
getPostParams addrs mhTrace =
  let sampleMap = snd3 (last mhTrace)
  in  [ (addr, x) | addr <- addrs, let x = fromJust (lookup addr sampleMap >>= prj @Double ) ]

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
      -- Run basic simulation over linearRegression
      {- This should generate a set of points on the y-axis for each given point on the x-axis -}
  bs <- Basic.basic n_samples Example.linearRegression
                    [0, 1, 2, 3, 4]
                    (repeat $ mkRecordLinRegr ([], [1.0], [0.0], [1.0]))
      {- This should output the provided fixed set of data points on the x and y axis. -}
  bs' <- Basic.basic n_samples Example.linearRegression
                    [0, 1, 2, 3, 4]
                    (map mkRecordLinRegrY [[-0.3], [0.75], [2.43], [3.5], [3.2]])
  return $ map fst bs

testLinRegrLWSim :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
testLinRegrLWSim = do
  -- Run linear model with fixed parameters, inputs, and outputs, to get likelihood of every data point over a uniform area
  let lw_n_iterations = 3
      xs  = concat [ replicate 11 x | x <- [0 .. 10]]
  lws <- LW.lw lw_n_iterations Example.linearRegression
                    xs
                    (concat $ repeat $ map (\y -> mkRecordLinRegr ([y], [1], [2], [2.0]))
                      [0 .. 10])
  let output = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob)) lws
  liftS $ print $ show output
  return output

-- | [(datapoints, samples, likelihood)]
testLinRegrLWInf :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
testLinRegrLWInf = do
  -- Run likelihood weighting inference over linearRegression
  {- This should output the provided fixed set of data points on the x and y axis, where each point has a different probability (due to us observing the probability of given y's). Also returns a trace of parameters and their likelihoods -}
  let  lw_n_iterations = 100
  lws <- LW.lw lw_n_iterations Example.linearRegression
                    [0 .. 100]
                    (map (mkRecordLinRegrY . (:[]) ) [0 .. 100])
  let output = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob)) lws
  return output

-- | Returns trace of model parameter samples
testLinRegrMHPost :: Sampler [((Double, Double), [(Addr, OpenSum MH.Vals)], [(Addr, Double)])]
testLinRegrMHPost = do
  -- Run mh inference over linearRegression for data representing a line with gradient 3 and intercept 0
  let mh_n_iterations = 200
  mhTrace <- MH.mh mh_n_iterations Example.linearRegression [] [0 .. 100]
                     (map (mkRecordLinRegrY . (:[]) . (*3)) [0 .. 100])
  -- Reformat MH trace
  let mhTrace' = map (\(xy, samples, logps) ->
       let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
           logps'   = Map.toList logps
       in  (xy, samples', logps') ) mhTrace
  return mhTrace'

-- | Use mh posterior to output predictive distribution
testLinRegrMHPred :: Sampler [(Double, Double)]
testLinRegrMHPred = do
  mhTrace <- testLinRegrMHPost
  -- Get the most recent accepted model parameters from the posterior
  let postParams = getPostParams [("m", 0), ("c", 0), ("σ", 0)] mhTrace
      mu         = fromJust $ lookup ("m", 0) postParams
      c          = fromJust $ lookup ("c", 0) postParams
      sigma      = fromJust $ lookup ("σ", 0) postParams
  liftS $ print $ "Using parameters " ++ show (mu, c, sigma)
  -- Using these parameters, simulate data from the predictive.
  bs <- Basic.basic 1 Example.linearRegression
                         (map (/1) [0 .. 100])
                         (repeat $ mkRecordLinRegr ([], [mu], [c], [sigma]))
  return $ map fst bs

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
  bs <- Basic.basic n_samples Example.logisticRegression
                         (map (/50) [(-100) .. 100])
                         (repeat $ mkRecordLogRegr ([], [2], [-0.15]))
  -- This should output the provided fixed set of data points on the x and y axis.
  bs' <- Basic.basic 3 Example.logisticRegression
                         [0, 1, 2, 3, 4]
                         (map mkRecordLogRegrL [[False], [False], [True], [False], [True]])
  return $ map fst bs

testLogRegrLWSim :: Sampler [((Double, Bool), [(Addr, OpenSum LW.Vals)], Double)]
testLogRegrLWSim = do
  -- Run logistic model with fixed parameters, inputs, and outputs, to get likelihood of every data point over a uniform area
  bs <- map fst <$> Basic.basic 1 Example.logisticRegression
                         (map (/50) [(-100) .. 100])
                         (repeat $ mkRecordLogRegr ([], [2], [-0.15]))
  let (xs, ys) = unzip bs
  let lw_n_iterations = 3
  lws <- LW.lw lw_n_iterations Example.logisticRegression
            xs
            (map (\y -> mkRecordLogRegr ([y], [2], [-0.15])) ys)
  let output = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob)) lws
  liftS $ print $ show (map snd3 output)
  return output

testLogRegrLWInf :: Sampler [((Double, Bool), [(Addr, OpenSum LW.Vals)], Double)]
testLogRegrLWInf = do
  -- Using fixed model parameters, generate some sample data points to learn
  let n_samples = 1
  bs <- map fst <$> Basic.basic n_samples Example.logisticRegression
                         (map (/50) [(-100) .. 100])
                         (repeat $ mkRecordLogRegr ([], [2], [-0.15]))
  let (xs, ys) = (map fst bs, map snd bs)
  -- Perform inference against these data points
  lws <- LW.lw 3 Example.logisticRegression xs (map (mkRecordLogRegrL . (:[])) ys)
  let output = map (\(xy, samples, prob) ->
        let samples' = Map.toList samples
        in (xy, samples', prob)) lws
  return output

testLogRegrMHPost :: Sampler [((Double, Bool), [(Addr, OpenSum MH.Vals)], [(Addr, Double)])]
testLogRegrMHPost = do
  let n_samples = 1
  bs <- map fst <$> Basic.basic n_samples Example.logisticRegression
                         (map (/50) [(-100) .. 100])
                         (repeat $ mkRecordLogRegr ([], [2], [-0.15]))
  let (xs, ys) = (map fst bs, map snd bs)
      mh_n_iterations = 70
  mhTrace <- MH.mh mh_n_iterations Example.logisticRegression []
                   xs (map (mkRecordLogRegrL . (:[])) ys)
  let mhTrace' = map (\(xy, samples, logps) ->
       let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
           logps'   = Map.toList logps
       in  (xy, samples', logps') ) mhTrace
  return mhTrace'

testLogRegrMHPred :: Sampler [(Double, Bool)]
testLogRegrMHPred = do
  mhTrace <- testLogRegrMHPost
  let postParams = map (fromJust . prj @Double . snd)
                      ((snd3 . head) (reverse mhTrace))
      (mu, postParams') = splitAt 1 postParams
      (b, _)            = splitAt 1 postParams'
  bs <- Basic.basic 1 Example.logisticRegression
                      (map (/50) [(-100) .. 100])
                      (repeat $ mkRecordLogRegr ([], mu, b))
  return $ map fst bs

-- -- {- Bayesian Neural Network for linear regression -}

-- mkRecordNN :: ([Double], [Double], [Double], [Double])
--            -> LRec Example.NNEnv
-- mkRecordNN (yobs_vals, weight_vals, bias_vals, sigm_vals) =
--   #yObs @= yobs_vals <: #weight @= weight_vals <: #bias @= bias_vals <: #sigma @= sigm_vals <: nil

-- mkRecordNNy :: Double
--            -> LRec Example.NNEnv
-- mkRecordNNy yobs_val =
--   #yObs @= [yobs_val] <: #weight @= [] <: #bias @= [] <: #sigma @= [] <: nil

-- testNNLinBasic :: Sampler  [(Double, Double)]
-- testNNLinBasic = do
--   -- Run basic simulation over neural network
--   bs <- Basic.basic 1 (Example.nnLinModel 3)
--                          (map (/1) [0 .. 300])
--                          (repeat $ mkRecordNN ([], [1, 5, 8],
--                                                    [2, -5, 1],
--                                                    [4.0]))
--   return $ map fst bs

-- testNNLinLWSim :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
-- testNNLinLWSim = do
--   -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a uniform area
--   let xs  = concat [ replicate 11 x | x <- [0 .. 10]]
--   lws <- LW.lw 1 (Example.nnLinModel 3)
--                     xs
--                     (concat $ repeat $ map (\y -> mkRecordNN ([y], [1, 5, 8],
--                                               [2, -5, 1],
--                                               [2.0])) [0 .. 10])
--   let output = map (\(xy, samples, prob) ->
--         let samples' = Map.toList samples
--         in (xy, samples', prob)) lws
--   return output

-- testNNLinLWInf :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
-- testNNLinLWInf = do
--   -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a sine curve
--   lws <- LW.lw 1  (Example.nnLinModel 3)
--                       (map (/50) [0 .. 300])
--                       (map (\y -> mkRecordNN ([y], [], [], []))
--                            [ x | x <- map (/50) [0 .. 300] ])
--   let output = map (\(xy, samples, prob) ->
--         let samples' = Map.toList samples
--         in (xy, samples', prob)) lws
--   return output

-- -- Run this with nn-basic, as it returns a predictive distribution rather than a posterior one.
-- testNNLinMHPost :: Sampler [((Double, Double), [(Addr, OpenSum MH.Vals)], [(Addr, Double)])]
-- testNNLinMHPost = do
--   -- Run mh over data representing a line with gradient 1 and intercept 0
--   let mh_n_iterations = 40
--   mhTrace <- MH.mh mh_n_iterations (Example.nnLinModel 3)
--                     (map (/50) [0 .. 300])
--                     (map mkRecordNNy [ x | x <- map (/50) [0 .. 300] ])
--   let mhTrace' = map (\(xy, samples, logps) ->
--        let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
--            logps'   = Map.toList logps
--        in  (xy, samples', logps') ) mhTrace
--   return mhTrace'

-- testNNLinMHPred :: Sampler [(Double, Double)]
-- testNNLinMHPred = do
--   mhTrace <- testNNLinMHPost
--   -- Get the most recent accepted model parameters from the posterior
--   let postParams = map (fromJust . prj @Double . snd)
--                        ((snd3 . head) (reverse mhTrace))
--       (bias, postParams') = splitAt 3 postParams
--       (weights, sigma)    = splitAt 3 postParams'
--   liftS $ print $ show (weights, bias, sigma)
--   -- Using these parameters, simulate data from the predictive. We can see that the predictive data becomes more accurate with more mh steps.
--   bs <- Basic.basic 1 (Example.nnLinModel 3)
--                          (map (/1) [0 .. 300])
--                          (repeat $ mkRecordNN ([], bias,
--                                                    weights,
--                                                    sigma))
--   return $ map fst bs

-- {- Bayesian neural network v2 -}

-- testNNStepBasic :: Sampler  [(Double, Double)]
-- testNNStepBasic = do
--   let n_samples = 1 -- Run basic simulation over neural network
--   bs <- Basic.basic n_samples (Example.nnStepModel 3)
--                               (map (/1) [-100 .. 100])
--                               (repeat $ mkRecordNN ([], [1, 5, 8],
--                                                         [2, -5, 1],
--                                                         [4.0]))
--   return $ map fst bs

-- testNNStepLWSim :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
-- testNNStepLWSim = do
--   let xs  = concat [ replicate 31 x | x <- [-10 .. 10]]
--       -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a uniform area
--   lws <- LW.lw 1 (Example.nnStepModel 3)
--                     xs
--                     (concat $ repeat $ map (\y -> mkRecordNN ([y], [1, 5, 8],
--                                               [2, -5, 1],
--                                               [2.0])) [-20 .. 10])
--   let output = map (\(xy, samples, prob) ->
--         let samples' = Map.toList samples
--         in (xy, samples', prob)) lws
--   return output

-- testNNStepLWSim2 :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
-- testNNStepLWSim2 = do
--   -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a sine curve
--   lws <- LW.lw 1  (Example.nnLinModel 3)
--                       (map (/50) [-200 .. 200])
--                       (map (\y -> mkRecordNN ([y], [1, 5, 8],
--                                                    [2, -5, 1],
--                                                    [2.0]))
--                            [ sin x | x <- map (/50) [-200 .. 200] ])
--   let output = map (\(xy, samples, prob) ->
--         let samples' = Map.toList samples
--         in (xy, samples', prob)) lws
--   return output

-- testNNStepLWInf :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
-- testNNStepLWInf = do
--   -- Run nn with fixed parameters, inputs, and outputs, to get likelihood of every data point over a sine curve
--   lws <- LW.lw 1  (Example.nnLinModel 3)
--                       (map (/50) [-200 .. 200])
--                       (map (\y -> mkRecordNN ([y], [],
--                                                    [],
--                                                    []))
--                            [ sin x | x <- map (/50) [-200 .. 200] ])
--   let output = map (\(xy, samples, prob) ->
--         let samples' = Map.toList samples
--         in (xy, samples', prob)) lws
--   return output

-- testNNStepMHPost :: Sampler [((Double, Double), [(Addr, OpenSum MH.Vals)],
--                    [(Addr, Double)])]
-- testNNStepMHPost = do
--   mhTrace <- MH.mh 20  (Example.nnStepModel 3)
--                        (map (/20) [-200 .. 200])
--                        (map mkRecordNNy
--                            [  x | x <- map (/20) [-200 .. 200] ])
--   let mhTrace' = map (\(xy, samples, logps) ->
--        let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
--            logps'   = Map.toList logps
--        in  (xy, samples', logps') ) mhTrace
--   return mhTrace'

-- testNNStepMHPred :: Sampler [(Double, Double)]
-- testNNStepMHPred = do
--   mhTrace <- testNNStepMHPost
--   -- Get the most recent accepted model parameters from the posterior
--   let postParams = map (fromJust . prj @Double . snd)
--                        ((snd3 . head) (reverse mhTrace))
--       (bias, postParams') = splitAt 3 postParams
--       (weights, sigma)    = splitAt 3 postParams'
--   -- Using these parameters, simulate data from the predictive.
--   bs <- Basic.basic 1 (Example.nnStepModel 3)
--                          ([-200 .. 200])
--                          (repeat $ mkRecordNN ([], bias,
--                                                    weights,
--                                                    sigma))
--   liftS $ print $ show (weights, bias, sigma)
--   return $ map fst bs

-- -- | Another neural network variation for logistic regression
-- mkRecordNNLog :: ([Bool], [Double])
--            -> LRec Example.NNLogEnv
-- mkRecordNNLog (yobs_vals, weight_vals) =
--   #yObs @= yobs_vals <: #weight @= weight_vals <: nil

-- mkRecordNNLogy :: Bool -> LRec Example.NNLogEnv
-- mkRecordNNLogy yobs_val =
--   #yObs @= [yobs_val] <: #weight @= [] <: nil

-- testNNLogBasic :: Sampler [((Double, Double), Bool)]
-- testNNLogBasic = do
--   let -- Run basic simulation over neural network
--       w1 = [0.18, 0.36, -1.29, 0.094, -1.64, 0.65]
--       w2 = [0.147, -0.417, -0.278, -1.275,0.568,-0.785,0.074,0.351,0.732]
--       w3 = [0.295, 0.414, -0.834]
--   bs <- Basic.basic 1 (Example.nnLogModel 3)
--                       nnLogDataX
--                       (repeat $ mkRecordNNLog ([], w1 ++ w2 ++ w3))
--   return $ map fst bs

-- testNNLogMHPost :: Sampler [(((Double, Double), Bool), [(Addr, OpenSum MH.Vals)], [(Addr, Double)])]
-- testNNLogMHPost = do
--   mhTrace <- MH.mh 50 (Example.nnLogModel 3)
--                       nnLogDataX
--                       (map mkRecordNNLogy nnLogDataY)
--   let mhTrace' = map (\(xy, samples, logps) ->
--         let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
--             logps'   = Map.toList logps
--         in  (xy, samples', logps') ) mhTrace
--   return mhTrace'

-- testNNLogMHPred :: Sampler [((Double, Double), Bool)]
-- testNNLogMHPred = do
--   mhTrace <- testNNLogMHPost
--   let postParams = map (fromJust . prj @Double . snd)
--                        ((snd3 . head) (reverse mhTrace))
--       (w1, postParams')   = splitAt 6 postParams
--       (w2, postParams'')  = splitAt 9 postParams'
--       (w3, postParams''') = splitAt 3 postParams''
--   bs <- Basic.basic 1 (Example.nnLogModel 3)
--                          nnLogDataX
--                          (repeat $ mkRecordNNLog ([], w1 ++ w2 ++ w3))
--   return $ map fst bs

-- {- Sine Model -}
-- testSinBasic :: Sampler [(Double, Double)]
-- testSinBasic = do
--   -- Simulate a sine curve
--   bs <- Basic.basic 1 Example.sineModel
--                        (map (/50) [0 .. 200])
--                        (repeat $ mkRecordLinRegr ([], [2], [0], [0.1]))
--   return $ map fst bs

-- testSinLWSim :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
-- testSinLWSim = do
--   -- Run sine model with fixed parameters, inputs, and outputs, to get likelihood of every data point over a uniform area
--   let xs  = concat [ replicate 101 x | x <- [0 .. 100]]
--   lws <- LW.lw 1 Example.sineModel
--                     xs
--                     (concat $ repeat $ map (\y -> mkRecordLinRegr ([y], [0.1], [0], [0.1]))
--                     (map (/100) [-100 .. 100]))
--   let output = map (\(xy, samples, prob) ->
--         let samples' = Map.toList samples
--         in (xy, samples', prob)) lws
--   return output

-- testSinLWInf :: Sampler [((Double, Double), [(Addr, OpenSum LW.Vals)], Double)]
-- testSinLWInf = do
--   -- Generate data points from sine model with fixed parameters
--   let xs = map (/50) [0 .. 200]
--   bs <- map fst <$> Basic.basic 1 Example.sineModel
--                     xs
--                     (repeat $ mkRecordLinRegr ([], [2], [0], [0.1]))
--   let (xs, ys) = (map fst bs, map snd bs)
--   -- Perform inference against these data points to try and learn sine model parameters
--   let lws = LW.lw 3 Example.sineModel xs (map (mkRecordLinRegrY . (:[])) ys)
--   output <- lws
--   let output' = map (\(xy, samples, prob) ->
--         let samples' = Map.toList samples
--         in (xy, samples', prob)) output
--   return output'

-- testSinMHPost :: Sampler [((Double, Double), [(Addr, OpenSum MH.Vals)], [(Addr, Double)])]
-- testSinMHPost = do
--   -- Generate data points from sine model with fixed parameters
--   let xs = map (/50) [0 .. 200]
--   bs <- map fst <$> Basic.basic 1 Example.sineModel
--                     xs
--                     (repeat $ mkRecordLinRegr ([], [2], [0], [0.1]))
--   let (xs, ys) = (map fst bs, map snd bs)
--       -- Run mh over data representing a sine wave
--   mhTrace <- MH.mh 200 Example.sineModel xs (map (mkRecordLinRegrY . (:[])) ys)
--   let mhTrace' = map (\(xy, samples, logps) ->
--        let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
--            logps'   = Map.toList logps
--        in  (xy, samples', logps') ) mhTrace
--   return mhTrace'

-- -- | Use mh posterior to output predictive distribution
-- testSinMHPred :: Sampler [(Double, Double)]
-- testSinMHPred = do
--   mhTrace <- testSinMHPost
--   liftS $ print $ show mhTrace
--   -- Get the most recent accepted model parameters from the posterior
--   let postParams = map (fromJust . prj @Double . snd)
--                        ((snd3 . head) (reverse mhTrace))
--       (mu, postParams') = splitAt 1 postParams
--       (c, sigma)        = splitAt 1 postParams'
--       xs                = map (/50) [0 .. 200]
--   liftS $ putStrLn $ "mu c sigma" ++ show (mu, c, sigma)
--   -- Using these parameters, simulate data from the predictive.
--   bs <- Basic.basic 1 Example.sineModel
--                          xs
--                          (repeat $ mkRecordLinRegr ([], mu, c, sigma))
--   return $ map fst bs

-- {- Hidden markov model -}

-- mkRecordHMM :: ([Int], Double, Double) -> LRec Example.HMMEnv
-- mkRecordHMM (ys, obsp, transp) = #y @= ys <: #obs_p @= [obsp] <: #trans_p @= [transp] <: nil

-- mkRecordHMMy :: [Int] -> LRec Example.HMMEnv
-- mkRecordHMMy ys = #y @= ys <: #obs_p @= [] <: #trans_p @= [] <: nil

-- testHMMBasic :: Sampler [([Int], [Int])]
-- testHMMBasic = do
--   let hmm_n_steps   = 10
--       hmm_n_samples = 10
--   bs <- Basic.basic hmm_n_samples (Example.hmmNSteps hmm_n_steps)
--                                    [0] [mkRecordHMM ([], 0.5, 0.5)]
--   return $ map fst bs

-- testHMMLWSim :: Sampler [(([Int], [Int]), [(Addr, OpenSum LW.Vals)], Double)]
-- testHMMLWSim = do
--   let hmm_n_steps    = 10
--       hmm_n_samples  = 10
--   lws <- LW.lw 1 (Example.hmmNSteps hmm_n_steps)
--                     (replicate hmm_n_samples 0)
--                     (concat $ repeat $ map (mkRecordHMMy . repeat) [0 .. hmm_n_samples])
--   let output = map (\(xy, samples, prob) ->
--         let samples' = Map.toList samples
--         in (xy, samples', prob)) lws
--   liftS $ print $ show output
--   return output

-- testHMMLWInf :: Sampler [(([Int], [Int]), [(Addr, OpenSum LW.Vals)], Double)]
-- testHMMLWInf = do
--   let hmm_n_steps   = 10
--       hmm_n_samples = 10
--   bs <- map fst <$> Basic.basic hmm_n_samples (Example.hmmNSteps hmm_n_steps)
--                     [0] [mkRecordHMM ([], 0.5, 0.5)]
--   let lw_n_iterations = 100
--       (_, yss) = unzip bs
--   lws <- LW.lw lw_n_iterations  (Example.hmmNSteps hmm_n_steps)
--                                 (replicate hmm_n_samples 0)
--                                 (map mkRecordHMMy yss)
--   let output = map (\(xy, samples, prob) ->
--         let samples' = Map.toList samples
--         in (xy, samples', prob)) lws
--   return output

-- testHMMMHPost :: Sampler [(([Int], [Int]), [(Addr, OpenSum MH.Vals)], [(Addr, Double)])]
-- testHMMMHPost = do
--   let hmm_n_steps   = 10
--       hmm_n_samples = 10
--   bs <- map fst <$> Basic.basic hmm_n_samples (Example.hmmNSteps hmm_n_steps)
--                     [0] [mkRecordHMM ([], 0.5, 0.5)]
--   let mh_n_iterations = 100
--       (_, yss)  = unzip bs

--   mhTrace <- MH.mh mh_n_iterations (Example.hmmNSteps hmm_n_steps)
--                                    (replicate hmm_n_samples 0)
--                                    (map mkRecordHMMy yss)
--   let mhTrace' = map (\(xy, samples, logps) ->
--        let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
--            logps'   = Map.toList logps
--        in  (xy, samples', logps') ) mhTrace
--   return mhTrace'

-- testHMMMHPred :: Sampler [([Int], [Int])]
-- testHMMMHPred = do
--   mhTrace <- testHMMMHPost
--   let (trans_p:obs_p:_) = map (fromJust . prj @Double . snd)
--                           ((snd3 . head) (reverse mhTrace))
--   liftS $ print $ "using parameters " ++ show (trans_p, obs_p)
--   let hmm_n_samples = 100
--       hmm_n_steps   = 10

--   bs <- Basic.basic hmm_n_samples (Example.hmmNSteps hmm_n_steps)
--                     [0] [mkRecordHMM ([], trans_p, obs_p)]
--   return $ map fst bs

-- testHMMStBasic :: Sampler [([Int], [Int])]
-- testHMMStBasic = do
--   bs <- Basic.basic 2
--             (runStateM . Example.hmmNStepsSt 0.5 0.5 10)
--                          [0] (repeat $ mkRecordHMM ([], 0.5, 0.5))
--   return $ map fst bs

-- {- Hidden markov model : SIR -}

-- mkRecordSIR :: ([Double], [Double], [Double]) -> LRec Example.SIREnv
-- mkRecordSIR (ρv, βv, γv) = #infobs @= [] <: #ρ @= ρv <: #β @= βv <: #γ @= γv <: nil

-- mkRecordSIRy :: [Int] -> LRec Example.SIREnv
-- mkRecordSIRy ys = #infobs @= ys <: #ρ @= [] <: #β @= [] <: #γ @= [] <: nil

-- fixedParams :: Int -> Int -> Example.FixedParams
-- fixedParams = Example.FixedParams

-- latentState :: Int -> Int -> Int -> Example.LatentState
-- latentState = Example.LatentState

-- fromLatentState :: Example.LatentState -> (Int, Int, Int)
-- fromLatentState (Example.LatentState sus inf recov) = (sus, inf, recov)

-- testSIRBasic :: Sampler ([(Int, Int, Int)], [Int])
-- testSIRBasic = do
--   bs <- Basic.basic 1
--           (Example.hmmSIRNsteps (fixedParams 763 1) 200)
--           [latentState 762 1 0] [mkRecordSIR ([0.29], [0.25], [0.015])]

--   let output = map ((\(xs, ys) -> (map fromLatentState xs, ys)) . fst) bs
--   return $ head output

-- testSIRLWInf :: Sampler [(([(Int, Int, Int)], [Int]), [(Addr, OpenSum LW.Vals)], Double)]
-- testSIRLWInf = do
--   let sir_n_steps    = length sirInfectedData

--   lws <- LW.lw 100 (Example.hmmSIRNsteps (fixedParams 763 1) sir_n_steps)
--                  [latentState 762 1 0] [mkRecordSIRy sirInfectedData]
--   let output = map (\(xy, samples, prob) ->
--         let samples' = Map.toList samples
--         in (xy, samples', prob)) lws
--   liftS $ print $ show output
--   let output' =
--         map (\((xs, ys), sampleMap, p) -> ((map fromLatentState xs, ys), sampleMap, p)) output
--   return output'

-- testSIRMHPost ::  Sampler [(([(Int, Int, Int)], [Int]), [(Addr, OpenSum MH.Vals)], [(Addr, Double)])]
-- testSIRMHPost = do
--   let sir_n_samples = 10
--   bs <- map fst <$> Basic.basic sir_n_samples
--           (Example.hmmSIRNsteps (fixedParams 763 1) 100)
--           [latentState 762 1 0] [mkRecordSIR ([0.29], [0.25], [0.015])]
--   let infectedData    = map snd bs
--       mh_n_iterations = 100
--   liftS $ print $ "infected data is " ++ show infectedData
--   mhTrace <- MH.mh mh_n_iterations (Example.hmmSIRNsteps (fixedParams 763 1) 200)
--                         (replicate sir_n_samples $ latentState 762 1 0)
--                         (map mkRecordSIRy infectedData)
--   let mhTrace' = map (\(xy, samples, logps) ->
--         let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
--             logps'   = Map.toList logps
--         in  (xy, samples', logps') ) mhTrace
--       mhTrace'' = map (\((xs, ys), sampleMap, p) -> ((map fromLatentState xs, ys), sampleMap, p)) mhTrace'
--   return mhTrace''