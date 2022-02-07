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
import Extensible.AffineReader
import Extensible.OpenProduct
import Util
import Debug.Trace
import Unsafe.Coerce
import Extensible.Inference.SimulateTrace (extractSamples)

{- Linear Regression -}

mkRecordLinRegr :: ([Double],  [Double],  [Double],  [Double]) -> LRec Example.LinRegrEnv
mkRecordLinRegr (y_vals, m_vals, c_vals, σ_vals) =
  (#y := y_vals) <:> (#m := m_vals) <:> (#c := c_vals) <:> (#σ := σ_vals) <:> nil

mkRecordLinRegrY :: [Double] -> LRec Example.LinRegrEnv
mkRecordLinRegrY y_vals =
  (#y := y_vals) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:> nil

testLinRegrBasic :: Int -> Sampler [[Double]]
testLinRegrBasic n_samples = do
  bs :: [([Double], Simulate.SampleMap)]
      <- Simulate.simulate n_samples Example.linearRegression
                    [[0 .. 100]]
                    [mkRecordLinRegr ([], [1.0], [0.0], [1.0])]
  return $ map fst bs

testLinRegrLWInf :: Int -> Sampler [([Double], Double)]
testLinRegrLWInf n_samples = do
  lwTrace :: [([Double],       -- y data points
                LW.SampleMap,  -- sample trace
                Double)]       -- likelihood
          <- LW.lw n_samples Example.linearRegression
                   [[0 .. 100]]
                   [mkRecordLinRegrY (map ((+2) . (*3)) [0 .. 100])]
  return $ map (\(ys, sampleMap, prob) -> (ys, prob)) lwTrace

testLinRegrMHPost :: Int -> Sampler [([Double], MH.LPMap)]
testLinRegrMHPost n_samples = do
  mhTrace <- MH.mh n_samples Example.linearRegression [] [[0 .. 100]]
                   [mkRecordLinRegrY (map ((+2) . (*3)) [0 .. 100])]
  return $ map (\(ys, sampleMap, prob) -> (ys, prob)) mhTrace

{- HMM -}

mkRecordHMM :: ([Int], Double, Double) -> LRec Example.HMMEnv
mkRecordHMM (ys, transp, obsp) = #y := ys <:> #trans_p := [transp] <:> #obs_p := [obsp] <:>  nil

mkRecordHMMy :: [Int] -> LRec Example.HMMEnv
mkRecordHMMy ys = #y := ys <:> #trans_p := [] <:> #obs_p := [] <:>  nil

testHMMBasic :: Int -> Sampler [Int]
testHMMBasic n_samples = do
  let hmm_length   = 100
  bs <- Simulate.simulate n_samples (Example.hmmNSteps hmm_length)
                          [0] [mkRecordHMM ([], 0.5, 0.9)]
  return $ map fst bs

hmm_data :: [Int]
hmm_data = [0,1,1,3,4,5,5,5,6,5,6,8,8,9,7,8,9,8,10,10,7,8,10,9,10,10,14,14,14,15,14,15,14,17,17,17,16,17,14,15,16,18,17,19,20,20,20,22,23,22,23,25,21,21,23,25,24,26,28,23,25,23,27,28,28,25,28,29,28,24,27,28,28,32,32,32,33,31,33,34,32,31,33,36,37,39,36,36,32,38,38,38,38,37,40,38,38,39,40,42]

testHMMLWInf :: Int -> Sampler [(Int, Double)]
testHMMLWInf n_samples = do
  let hmm_length   = 100
  lwTrace <- LW.lw n_samples (Example.hmmNSteps hmm_length)
                             [0] [mkRecordHMMy hmm_data]
  return $ map (\(ys, sampleMap, prob) -> (ys, prob)) lwTrace

testHMMMHPost :: Int -> Sampler [(Int, MH.LPMap)]
testHMMMHPost n_samples = do
  let hmm_n_steps   = 20
  mhTrace <- MH.mh n_samples (Example.hmmNSteps hmm_n_steps) ["trans_p", "obs_p"]
                             [0] [mkRecordHMMy hmm_data]
  return $ map (\(ys, sampleMap, prob) -> (ys, prob)) mhTrace

{- Topic model -}
vocab :: [String]
vocab = ["DNA", "evolution", "parsing", "phonology"]

doc_words :: [String]
doc_words     = ["DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA"]

mkRecordTopic :: ([[Double]], [[Double]], [String]) -> LRec Example.TopicEnv
mkRecordTopic (tps, wps, ys) =  #θ := tps <:>  #φ := wps <:> #w := ys <:>nil

testTopicBasic :: Int -> Sampler [[String]]
testTopicBasic n_samples = do
  bs <- Simulate.simulate n_samples (Example.documentDist vocab 2)
                        [100] [mkRecordTopic ([[0.5, 0.5]], [[0.12491280814569208,1.9941599739151505e-2,0.5385152817942926,0.3166303103208638],[1.72605174564027e-2,2.9475900240868515e-2,9.906011619752661e-2,0.8542034661052021]], [])]
  return $ map fst bs

testTopicLW :: Int -> Sampler [([String], Double)]
testTopicLW n_samples = do
  lwTrace <- LW.lw n_samples (Example.documentDist vocabulary 2)
                        [100] [mkRecordTopic ([], [], doc_words)]
  return $ map (\(ys, sampleMap, prob) -> (ys, prob)) lwTrace

testTopicMHPost :: Int -> Sampler [([String], MH.LPMap)]
testTopicMHPost n_samples = do
  mhTrace <- MH.mh n_samples (Example.documentDist vocabulary 2) ["φ", "θ"]
                       [100] [mkRecordTopic ([], [], doc_words)]
  return $ map (\(ys, sampleMap, prob) -> (ys, prob)) mhTrace

{- SIR -}
mkRecordSIR :: ([Double], [Double], [Double]) -> LRec Example.SIREnv
mkRecordSIR (βv, γv, ρv) = #β := βv <:> #γ := γv <:>  #ρ := ρv <:>  #infobs := [] <:> nil

mkRecordSIRy :: [Int] -> LRec Example.SIREnv
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
      infobs    :: [Int]              = Simulate.extractSamples (#infob, Proxy @Int) sampleMap

      sirLog_tuples :: [(Int, Int, Int)] = map fromLatState sirLog

  return (sirLog_tuples, infobs)

{- Version of SIR simulation which instead directly composes runWriterM with the model, instead of using Simulate.simulateWith -}
testSIRBasic' :: Sampler [((Example.LatState, [Example.LatState]), Simulate.SampleMap)]
testSIRBasic' = do
  simOutputs <- Simulate.simulate 1 (runWriterM . Example.hmmSIRNsteps 100)
                   [latentState 762 1 0]
                   [mkRecordSIR ([0.7], [0.009], [0.3])]

  let fstOutput = head simOutputs
      sirLog    :: [Example.LatState] = (snd . fst) fstOutput
      sampleMap :: Simulate.SampleMap    = snd fstOutput
      infobs    :: [Int]                 = Simulate.extractSamples (#infobs, Proxy @Int) sampleMap

  return simOutputs