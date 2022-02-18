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

{- Linear Regression -}
mkRecordLinRegr :: ([Double],  [Double],  [Double],  [Double]) -> ModelEnv Example.LinRegrEnv
mkRecordLinRegr (y_vals, m_vals, c_vals, σ_vals) =
  (#y := y_vals) <:> (#m := m_vals) <:> (#c := c_vals) <:> (#σ := σ_vals) <:> nil

mkRecordLinRegrY :: [Double] -> ModelEnv Example.LinRegrEnv
mkRecordLinRegrY y_vals =
  (#y := y_vals) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:> nil

testLinRegrBasic :: Int -> Int -> Sampler [[Double]]
testLinRegrBasic n_datapoints n_samples = do
  let n_datapoints' = fromIntegral n_datapoints
  bs :: [([Double], Simulate.SampleMap)]
      <- Simulate.simulate n_samples Example.linearRegression
                    [[0 .. n_datapoints']]
                    [mkRecordLinRegr ([], [1.0], [0.0], [1.0])]
  return $ map fst bs

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
      params     = #β := [0.7] <:> #γ := [0.009] <:>  #ρ := [0.3] <:>  #infobs := [] <:> nil
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
infobs_data = [0,1,3,3,3,4,13,16,24,35,67,89,133,149,180,209,195,196,188,234,205,179,206,192,187,197,207,185,181,199,179,164,195,181,161,149,156,170,166,170,171,163,162,175,151,201,152,183,160,166,148,184,137,157,159,159,144,168,159,133,141,133,126,132,128,128,139,111,144,135,114,129,111,134,143,129,122,107,107,110,110,128,122,115,116,116,115,111,120,113,100,79,103,110,140,112,106,103,100,108]

testSIRMHPost :: Sampler ([Double], [Double], [Double])
testSIRMHPost = do
  let mh_n_iterations = 100000
  -- This demonstrates well the need for specifying the sample sites ["ρ", "β", "γ"].
  mhTrace :: [((Example.LatState, [Example.LatState]), MH.SMap, MH.LPMap)]
          <- MH.mh mh_n_iterations (runWriterM . Example.hmmSIRNsteps 10) ["β", "γ", "ρ"]
                        [latentState 762 1 0]
                        [mkRecordSIR ([], [0.009], [0.3], infobs_data)]
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
testSIRBasic' :: Sampler [((Example.LatState, [Example.LatState]), Simulate.SampleMap)]
testSIRBasic' = do
  simOutputs <- Simulate.simulate 1 (runWriterM . Example.hmmSIRNsteps 100)
                   [latentState 762 1 0]
                   [mkRecordSIRparams ([0.7], [0.009], [0.3])]

  let fstOutput = head simOutputs
      sirLog    :: [Example.LatState] = (snd . fst) fstOutput
      sampleMap :: Simulate.SampleMap    = snd fstOutput
      infobs    :: [Int]                 = Simulate.extractSamples (#infobs, Proxy @Int) sampleMap

  return simOutputs


{- SIR Resusceptible -}
testSIRSBasic :: Sampler ([(Int, Int, Int)], -- sir values
                          [Int])
testSIRSBasic = do
  simOutputs <- Simulate.simulate 1 (runWriterM . Example.hmmSIRNsteps' 100)
                   [latentState 762 1 0]
                   [ #β := [0.2] <:> #γ := [0.009] <:>  #ρ := [0.3] <:> #η := [0.05] <:> #infobs := [] <:> nil]

  let fstOutput = head simOutputs
      sirLog    :: [Example.LatState] = (snd . fst) fstOutput
      sampleMap :: Simulate.SampleMap = snd fstOutput
      infobs    :: [Int]              = Simulate.extractSamples (#infobs, Proxy @Int) sampleMap

      sirLog_tuples :: [(Int, Int, Int)] = map fromLatState sirLog

  return (sirLog_tuples, infobs)

{- SIRV -}

fromLatSIRVState :: Example.LatStateSIRV -> (Int, Int, Int, Int)
fromLatSIRVState (Example.LatStateSIRV sus inf recov vacc) = (sus, inf, recov, vacc)

testSIRVBasic :: Sampler ([(Int, Int, Int, Int)], -- sir values
                          [Int])            -- observed infections
testSIRVBasic = do
  let latState0  = Example.LatStateSIRV { Example.s = 762, Example.i = 1, Example.r = 0,  Example.v = 0 }
      params     = #β := [0.7] <:> #γ := [0.019] <:>  #ρ := [0.3] <:> #ω := [] <:>  #η := [] <:> #infobs := [] <:> nil
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
