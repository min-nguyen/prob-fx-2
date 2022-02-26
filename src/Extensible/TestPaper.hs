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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Extensible.TestPaper where

import Data.Maybe
import qualified Data.Map as Map
import qualified Extensible.ExamplePaper as Example
import Extensible.DataSets
import Extensible.Dist
import qualified Extensible.Inference.SimulateTrace as Simulate
import qualified Extensible.Inference.LW as LW
import qualified Extensible.Inference.MH as MH
import Extensible.OpenSum
import qualified Extensible.OpenSum as OpenSum
import Extensible.Inference.Inf
import Extensible.State
import Extensible.Model
import Extensible.Sampler
import Extensible.ObsReader
import Extensible.ModelEnv
import Util
import Debug.Trace
import Unsafe.Coerce
import Extensible.Inference.SimulateTrace (extractSamples)

{- Linear Regression One -- slower than linRegr for LW and MH -}
testLinRegrOneBasic :: Int -> Int -> Sampler [(Double, Double)]
testLinRegrOneBasic n_datapoints n_samples = do
  bs <- Simulate.simulate n_samples Example.linearRegressionOne
                    [0 .. (fromIntegral n_datapoints)]
                    (repeat $ mkRecordLinRegr ([], [1.0], [0.0], [1.0]))
  return $ map fst bs

testLinRegrOneLWInf :: Int -> Int -> Sampler [((Double, Double), Double)]
testLinRegrOneLWInf n_datapoints n_samples = do
  lwTrace <- LW.lw n_samples Example.linearRegressionOne
                    [0 .. (fromIntegral n_datapoints)]
                    (map (mkRecordLinRegrY . (:[]) ) (map ((+2) . (*3)) [0 .. 100]))
  return $ map (\(ys, sampleMap, prob) -> (ys, prob)) lwTrace

testLinRegrOneMHPost :: Int -> Int -> Sampler [((Double, Double), MH.LPMap)]
testLinRegrOneMHPost n_datapoints n_samples = do
  mhTrace <- MH.mh n_samples Example.linearRegressionOne [] [0 .. (fromIntegral n_datapoints)]
                    (map (mkRecordLinRegrY . (:[]) ) (map ((+2) . (*3)) [0 .. 100]))
  return $ map (\(ys, sampleMap, prob) -> (ys, prob)) mhTrace

{- Linear Regression -}
mkRecordLinRegr :: ([Double],  [Double],  [Double],  [Double]) -> ModelEnv Example.LinRegrEnv
mkRecordLinRegr (y_vals, m_vals, c_vals, σ_vals) =
  (#y := y_vals) <:> (#m := m_vals) <:> (#c := c_vals) <:> (#σ := σ_vals) <:> nil

mkRecordLinRegrY :: [Double] -> ModelEnv Example.LinRegrEnv
mkRecordLinRegrY y_vals =
  (#y := y_vals) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:> nil

-- testLinRegrBasic :: Int -> Int -> Sampler [[Double]]
-- testLinRegrBasic n_datapoints n_samples = do
--   let n_datapoints' = fromIntegral n_datapoints
--   bs :: [([Double], Simulate.SampleMap)]
--       <- Simulate.simulate n_samples Example.linearRegression
--                     [[0 .. n_datapoints']]
--                     [mkRecordLinRegr ([], [1.0], [0.0], [1.0])]
--   return $ map fst bs

testLinRegrLWInf :: Int -> Int -> Sampler [([Double], Double)]
testLinRegrLWInf n_datapoints n_samples = do
  let n_datapoints' = fromIntegral n_datapoints
  lwTrace :: [([Double],       -- y data points
                LW.SampleMap,  -- sample trace
                Double)]       -- likelihood
          <- LW.lw n_samples Example.linearRegression
                   [[0 .. n_datapoints']]
                   [mkRecordLinRegrY (map ((+2) . (*3)) [0 .. n_datapoints'])]
  return $ map (\(ys, sampleMap, prob) -> (ys, prob)) lwTrace

testLinRegrMHPost :: Int -> Int -> Sampler [([Double], MH.LPMap)]
testLinRegrMHPost n_datapoints n_samples = do
  let n_datapoints' = fromIntegral n_datapoints
  mhTrace <- MH.mh n_samples Example.linearRegression [] [[0 .. n_datapoints']]
                   [mkRecordLinRegrY (map ((+2) . (*3)) [0 .. n_datapoints'])]

  return $ map (\(ys, sampleMap, prob) -> (ys, prob)) mhTrace


{- Log Regr -}
mkRecordLogRegr :: ([Bool], [Double], [Double]) -> ModelEnv Example.LogRegrEnv
mkRecordLogRegr (label_vals, m_vals, b_vals) =
  #label := label_vals <:> #m := m_vals <:> #b := b_vals <:> nil

mkRecordLogRegrL :: [Bool] -> ModelEnv Example.LogRegrEnv
mkRecordLogRegrL label_val =
 #label := label_val <:> #m := [] <:> #b := [] <:> nil

testLogRegrBasic :: Int -> Int -> Sampler [([Double], [Bool])]
testLogRegrBasic n_datapoints n_samples = do
  -- This should generate a set of points on the y-axis for each given point on the x-axis
  let incr = 200/fromIntegral n_datapoints
      xs = [ (-100 + (fromIntegral x)*incr)/50 | x <- [0 .. n_datapoints]]
  bs <- Simulate.simulate n_samples Example.logisticRegression
                         [xs]
                         [mkRecordLogRegr ([], [2], [-0.15])]
  return $ map fst bs

-- logRegrData :: [Bool]
-- logRegrData = [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False,True,True,True,True,True,False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False,True,True,True,True,True,True,True,True,False,True,True,True,True,True,True,True,True,False,True,False,True,True,True,False,True,True,True,True,False,True,False,False,True,False,True,True,False,False,True,True,False,False,True,False,True,True,False,True,True,False,True,False,True,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,False,True,False,False,True,False,False,False,False,True,False,False,False,False,False,True,False,False,False,False,True,False,False,True,False,False,False,True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False]

-- testLogRegrLWInf :: Int -> Int -> Sampler [((Double, Bool), [(Addr, OpenSum PrimVal)], Double)]
-- testLogRegrLWInf n_datapoints n_samples =  do
--   -- Using fixed model parameters, generate some sample data points to learn
--   let incr = 200/fromIntegral n_datapoints
--       xs = [ (-100 + (fromIntegral x)*incr)/50 | x <- [0 .. n_datapoints]]
--   -- Perform inference against these data points
--   lwTrace <- LW.lw 5 Example.logisticRegression xs [(mkRecordLogRegrL . (:[])) ys]
--   let lwTrace' = processLWTrace lwTrace
--   return lwTrace'

{- HMM -}
mkRecordHMM :: ([Int], Double, Double) -> ModelEnv Example.HMMEnv
mkRecordHMM (ys, transp, obsp) = #y := ys <:> #trans_p := [transp] <:> #obs_p := [obsp] <:>  nil

mkRecordHMMy :: [Int] -> ModelEnv Example.HMMEnv
mkRecordHMMy ys = #y := ys <:> #trans_p := [] <:> #obs_p := [] <:>  nil

testHMMBasic :: Int -> Int -> Sampler [Int]
testHMMBasic hmm_length n_samples = do
  bs <- Simulate.simulate n_samples (Example.hmmNSteps hmm_length)
                          [0] [mkRecordHMM ([], 0.5, 0.9)]
  return $ map fst bs

hmm_data :: [Int]
hmm_data = [0,1,1,3,4,5,5,5,6,5,6,8,8,9,7,8,9,8,10,10,7,8,10,9,10,10,14,14,14,15,14,15,14,17,17,17,16,17,14,15,16,18,17,19,20,20,20,22,23,22,23,25,21,21,23,25,24,26,28,23,25,23,27,28,28,25,28,29,28,24,27,28,28,32,32,32,33,31,33,34,32,31,33,36,37,39,36,36,32,38,38,38,38,37,40,38,38,39,40,42]

testHMMLWInf :: Int -> Int -> Sampler [(Int, Double)]
testHMMLWInf hmm_length n_samples = do
  lwTrace <- LW.lw n_samples (Example.hmmNSteps hmm_length)
                             [0] [mkRecordHMMy hmm_data]
  return $ map (\(ys, sampleMap, prob) -> (ys, prob)) lwTrace

testHMMMHPost :: Int -> Int -> Sampler [(Int, MH.LPMap)]
testHMMMHPost hmm_length n_samples = do
  mhTrace <- MH.mh n_samples (Example.hmmNSteps hmm_length) ["trans_p", "obs_p"]
                             [0] [mkRecordHMMy hmm_data]
  return $ map (\(ys, sampleMap, prob) -> (ys, prob)) mhTrace

{- Topic model -}
vocab :: [String]
vocab = ["DNA", "evolution", "parsing", "phonology"]

doc_words :: [String]
doc_words     = ["DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA"]

mkRecordTopic :: ([[Double]], [[Double]], [String]) -> ModelEnv Example.TopicEnv
mkRecordTopic (tps, wps, ys) =  #θ := tps <:>  #φ := wps <:> #w := ys <:>nil

testTopicBasic :: Int -> Int -> Sampler [[String]]
testTopicBasic n_words n_samples = do
  bs <- Simulate.simulate n_samples (Example.documentDist vocab 2)
                        [n_words] [mkRecordTopic ([[0.5, 0.5]], [[0.12491280814569208,1.9941599739151505e-2,0.5385152817942926,0.3166303103208638],[1.72605174564027e-2,2.9475900240868515e-2,9.906011619752661e-2,0.8542034661052021]], [])]
  return $ map fst bs

testTopicLW :: Int -> Int -> Sampler [([String], Double)]
testTopicLW n_words n_samples = do
  lwTrace <- LW.lw n_samples (Example.documentDist vocabulary 2)
                        [n_words] [mkRecordTopic ([], [], doc_words)]
  return $ map (\(ys, sampleMap, prob) -> (ys, prob)) lwTrace

testTopicMHPost :: Int -> Int -> Sampler [([String], MH.LPMap)]
testTopicMHPost n_words n_samples = do
  mhTrace <- MH.mh n_samples (Example.documentDist vocabulary 2) ["φ", "θ"]
                       [n_words] [mkRecordTopic ([], [], doc_words)]
  return $ map (\(ys, sampleMap, prob) -> (ys, prob)) mhTrace

{- SIR -}
mkRecordSIR :: ([Double], [Double], [Double], [Int]) -> ModelEnv Example.SIREnv
mkRecordSIR (βv, γv, ρv, infobs) = #β := βv <:> #γ := γv <:>  #ρ := ρv <:>  #infobs := infobs <:> nil

mkRecordSIRparams :: ([Double], [Double], [Double]) -> ModelEnv Example.SIREnv
mkRecordSIRparams (βv, γv, ρv) = #β := βv <:> #γ := γv <:>  #ρ := ρv <:>  #infobs := [] <:> nil

mkRecordSIRy :: [Int] -> ModelEnv Example.SIREnv
mkRecordSIRy ys = #β := [] <:> #γ := [] <:>  #ρ := [] <:> #infobs := ys <:> nil

latentState :: Int -> Int -> Int -> Example.LatState
latentState = Example.LatState

fromLatState :: Example.LatState -> (Int, Int, Int)
fromLatState (Example.LatState sus inf recov) = (sus, inf, recov)

testSIRBasic :: Sampler ([(Int, Int, Int)], -- sir values
                          [Int])            -- observed infections
testSIRBasic = do
  let latState0  = Example.LatState { Example.sus = 762, Example.inf = 1, Example.recov = 0 }
      params     = #β := [0.5] <:> #γ := [0.009] <:>  #ρ := [0.3] <:>  #infobs := [] <:> nil
  simOutputs :: [((Example.LatState,   -- model output
                  [Example.LatState]), -- writer effect log of sir latent states
                   Simulate.SampleMap)]   -- trace of samples
                <- Simulate.simulateWith 1 (Example.hmmSIRNsteps 100)
                   [latState0] [params] runWriterM
  let fstOutput = head simOutputs
      sirLog    :: [Example.LatState] = (snd . fst) fstOutput
      sampleMap :: Simulate.SampleMap = snd fstOutput
      infobs    :: [Int]              = Simulate.extractSamples (#infobs, Proxy @Int) sampleMap

      sirLog_tuples :: [(Int, Int, Int)] = map fromLatState sirLog

  return (sirLog_tuples, infobs)

infobs_data :: [Int]
infobs_data = [0,1,4,2,1,3,3,5,10,11,30,23,48,50,91,94,129,151,172,173,198,193,214,179,229,172,205,211,191,212,185,184,173,211,185,197,176,169,198,174,163,197,152,198,153,164,154,167,178,174,160,149,140,172,169,144,137,151,166,151,147,149,159,150,151,139,137,182,121,119,133,146,141,136,126,131,124,110,120,113,117,102,120,117,122,121,110,125,127,117,117,98,109,108,108,120,98,103,104,103]

testSIRMHPost :: Sampler ([Double], [Double], [Double])
testSIRMHPost = do
  let mh_n_iterations = 50000
  -- This demonstrates well the need for specifying the sample sites ["ρ", "β", "γ"].
  mhTrace :: [((Example.LatState, [Example.LatState]), MH.SMap, MH.LPMap)]
          <- MH.mh mh_n_iterations (runWriterM . Example.hmmSIRNsteps 20) ["β", "γ", "ρ"]
                        [latentState 762 1 0]
                        [mkRecordSIR ([], [0.009], [], infobs_data)]
  let mhSampleMaps = map snd3 mhTrace
      ρs = concatMap (MH.extractSamples (#ρ, Proxy @Double)) mhSampleMaps
      βs = concatMap (MH.extractSamples (#β, Proxy @Double)) mhSampleMaps
      γs = concatMap (MH.extractSamples (#γ, Proxy @Double)) mhSampleMaps
  -- printS $ show (ρs, βs, γs)
  return (ρs, βs, γs)
  -- let mhTrace'    = processMHTrace mhTrace
  --     postParams  = extractPostParams (Proxy @Double)  [("ρ", 0), ("β", 0), ("γ", 0)] mhTrace'
  -- return postParams

-- Version of SIR simulation which instead directly composes runWriterM with the model, instead of using Simulate.simulateWith
testSIRBasic' :: Sampler ([(Int, Int, Int)], [Int])
testSIRBasic' = do
  simOutputs <- Simulate.simulate 1 (runWriterM . Example.hmmSIRNsteps 100)
                   [latentState 762 1 0]
                   [mkRecordSIRparams ([0.7], [0.009], [0.3])]

  let fstOutput = head simOutputs
      sirLog    :: [Example.LatState]       = (snd . fst) fstOutput
      env_strace :: ModelEnv Example.SIREnv = snd fstOutput
      infobs    :: [Int]                    = getOP #infobs env_strace
      sirLog_tuples :: [(Int, Int, Int)]    = map fromLatState sirLog

  return (sirLog_tuples, infobs)


{- SIR Resusceptible -}
-- testSIRSBasic :: Sampler ([(Int, Int, Int)], -- sir values
--                           [Int])
-- testSIRSBasic = do
--   simOutputs <- Simulate.simulate 1 (runWriterM . Example.hmmSIRNsteps' 100)
--                    [latentState 762 1 0]
--                    [ #β := [0.5] <:> #γ := [0.009] <:>  #ρ := [0.3] <:> #η := [0.05] <:> #infobs := [] <:> nil]

--   let fstOutput = head simOutputs
--       sirLog    :: [Example.LatState] = (snd . fst) fstOutput
--       sampleMap :: Simulate.SampleMap = snd fstOutput
--       infobs    :: [Int]              = Simulate.extractSamples (#infobs, Proxy @Int) sampleMap

--       sirLog_tuples :: [(Int, Int, Int)] = map fromLatState sirLog

--   return (sirLog_tuples, infobs)

{- SIRV -}

fromLatSIRVState :: Example.LatStateSIRV -> (Int, Int, Int, Int)
fromLatSIRVState (Example.LatStateSIRV sus inf recov vacc) = (sus, inf, recov, vacc)

testSIRVBasic :: Sampler ([(Int, Int, Int, Int)], -- sir values
                          [Int])            -- observed infections
testSIRVBasic = do
  let latState0  = Example.LatStateSIRV { Example.s = 762, Example.i = 1, Example.r = 0,  Example.v = 0 }
      params     = #β := [0.5] <:> #γ := [0.009] <:>  #ρ := [0.3] <:> #ω := [0.04] <:>  #η := [0.05] <:> #infobs := [] <:> nil
  simOutputs :: [((Example.LatStateSIRV,   -- model output
                  [Example.LatStateSIRV]), -- writer effect log of sir latent states
                   Simulate.SampleMap)]   -- trace of samples
                <- Simulate.simulateWith 1 (Example.hmmSIRVNsteps 100)
                   [latState0] [params] runWriterM
  let fstOutput = head simOutputs
      sirLog    :: [Example.LatStateSIRV] = (snd . fst) fstOutput
      sampleMap :: Simulate.SampleMap = snd fstOutput
      infobs    :: [Int]              = Simulate.extractSamples (#infobs, Proxy @Int) sampleMap

      sirLog_tuples :: [(Int, Int, Int, Int)] = map fromLatSIRVState sirLog

  return (sirLog_tuples, infobs)
