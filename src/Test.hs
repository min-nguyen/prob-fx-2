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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Test where

import Data.Maybe
import qualified Data.Map as Map
import qualified Example as Example
import DataSets
import Dist
import qualified Inference.Simulate as Simulate
import qualified Inference.LW as LW
import qualified Inference.MH as MH
import OpenSum
import qualified OpenSum as OpenSum
import State
import Model
import Sampler
import ObsReader
import ModelEnv
import Util
import Debug.Trace
import Unsafe.Coerce
import STrace

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

processMHTrace :: [(a, Map.Map Addr (PrimDist, OpenSum PrimVal), Map.Map Addr Double)]
               -> [(a, [(Addr, OpenSum PrimVal)], [(Addr, Double)])]
processMHTrace = map (\(xy, samples, logps) ->
  let samples' = map (\(α, (dist, sample)) -> (α, sample)) (Map.toList samples)
      logps'   = Map.toList logps
  in  (xy, samples', logps') )

{- Linear Regression -}
mkRecordLinRegr :: ([Double],  [Double],  [Double],  [Double]) -> ModelEnv Example.LinRegrEnv
mkRecordLinRegr (y_vals, m_vals, c_vals, σ_vals) =
  (#y := y_vals) <:> (#m := m_vals) <:> (#c := c_vals) <:> (#σ := σ_vals) <:> nil

mkRecordLinRegrY :: [Double] -> ModelEnv Example.LinRegrEnv
mkRecordLinRegrY y_vals =
  (#y := y_vals) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:> nil

testLinRegrSim :: Int -> Int -> Sampler [(Double, Double)]
testLinRegrSim n_datapoints n_samples = do
  let n_datapoints' = fromIntegral n_datapoints
  bs :: [([(Double, Double)], ModelEnv Example.LinRegrEnv)]
      <- Simulate.simulate n_samples Example.linearRegression
                    [0 .. n_datapoints']
                    (mkRecordLinRegr ([], [1.0], [0.0], [1.0]))
  return $ concatMap fst bs

testLinRegrLWInf :: Int -> Int -> Sampler [((Double, Double, Double), Double)]
testLinRegrLWInf n_datapoints n_samples = do
  let n_datapoints' = fromIntegral n_datapoints
  lwTrace :: [([(Double, Double)],              -- y data points
                ModelEnv Example.LinRegrEnv,    -- sample trace
                Double)]                        -- likelihood
          <- LW.lw n_samples Example.linearRegression
                   [0 .. n_datapoints']
                   (mkRecordLinRegrY (map ((+2) . (*3)) [0 .. n_datapoints']))
  let lw_envs_out = map snd3 lwTrace
      mus        = concatMap (getOP #m) lw_envs_out
      cs         = concatMap (getOP #c) lw_envs_out
      σs         = concatMap (getOP #σ) lw_envs_out
      ps         = map thrd3 lwTrace
  return $ zip (zip3 mus cs σs) ps

testLinRegrMHPost :: Int -> Int -> Sampler ([Double], [Double], [Double])
testLinRegrMHPost n_datapoints n_samples = do
  let n_datapoints' = fromIntegral n_datapoints
  mhTrace <- MH.mh n_samples Example.linearRegression ["m", "c", "σ"] [0 .. n_datapoints']
                   (mkRecordLinRegrY (map ((+2) . (*3)) [0 .. n_datapoints']))
  let mh_envs_out = map snd3 mhTrace
      mus        = concatMap (getOP #m) mh_envs_out
      cs         = concatMap (getOP #c) mh_envs_out
      σs         = concatMap (getOP #σ) mh_envs_out
  return (mus, cs, σs)

{- Log Regr -}
mkRecordLogRegr :: ([Bool], [Double], [Double]) -> ModelEnv Example.LogRegrEnv
mkRecordLogRegr (label_vals, m_vals, b_vals) =
  #label := label_vals <:> #m := m_vals <:> #b := b_vals <:> nil

mkRecordLogRegrL :: [Bool] -> ModelEnv Example.LogRegrEnv
mkRecordLogRegrL label_val =
 #label := label_val <:> #m := [] <:> #b := [] <:> nil

testLogRegrSim :: Int -> Int -> Sampler [(Double, Bool)]
testLogRegrSim n_datapoints n_samples = do
  -- This should generate a set of points on the y-axis for each given point on the x-axis
  let incr = 200/fromIntegral n_datapoints
      xs = [ (-100 + (fromIntegral x)*incr)/50 | x <- [0 .. n_datapoints]]
  bs <- Simulate.simulate n_samples Example.logisticRegression
                         xs
                         (mkRecordLogRegr ([], [2], [-0.15]))
  return $ concatMap fst bs

{- HMM -}
mkRecordHMM :: ([Int], Double, Double) -> ModelEnv Example.HMMEnv
mkRecordHMM (ys, transp, obsp) = #y := ys <:> #trans_p := [transp] <:> #obs_p := [obsp] <:>  nil

mkRecordHMMy :: [Int] -> ModelEnv Example.HMMEnv
mkRecordHMMy ys = #y := ys <:> #trans_p := [] <:> #obs_p := [] <:>  nil

testHMMBasic :: Int -> Int -> Sampler [Int]
testHMMBasic hmm_length n_samples = do
  bs <- Simulate.simulate n_samples (Example.hmmNSteps hmm_length)
                          0 (mkRecordHMM ([], 0.5, 0.9))
  return $ map fst bs

hmm_data :: [Int]
hmm_data = [0,1,1,3,4,5,5,5,6,5,6,8,8,9,7,8,9,8,10,10,7,8,10,9,10,10,14,14,14,15,14,15,14,17,17,17,16,17,14,15,16,18,17,19,20,20,20,22,23,22,23,25,21,21,23,25,24,26,28,23,25,23,27,28,28,25,28,29,28,24,27,28,28,32,32,32,33,31,33,34,32,31,33,36,37,39,36,36,32,38,38,38,38,37,40,38,38,39,40,42]

testHMMLWInf :: Int -> Int -> Sampler [(Int, Double)]
testHMMLWInf hmm_length n_samples = do
  lwTrace <- LW.lw n_samples (Example.hmmNSteps hmm_length)
                             0 (mkRecordHMMy hmm_data)
  return $ map (\(ys, sampleMap, prob) -> (ys, prob)) lwTrace

testHMMMHPost :: Int -> Int -> Sampler [(Int, LPTrace)]
testHMMMHPost hmm_length n_samples = do
  mhTrace <- MH.mh n_samples (Example.hmmNSteps hmm_length) ["trans_p", "obs_p"]
                             0 (mkRecordHMMy hmm_data)
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
                        n_words (mkRecordTopic ([[0.5, 0.5]], [[0.12491280814569208,1.9941599739151505e-2,0.5385152817942926,0.3166303103208638],[1.72605174564027e-2,2.9475900240868515e-2,9.906011619752661e-2,0.8542034661052021]], []))
  return $ map fst bs

testTopicLW :: Int -> Int -> Sampler [([String], Double)]
testTopicLW n_words n_samples = do
  lwTrace <- LW.lw n_samples (Example.documentDist vocabulary 2)
                        n_words (mkRecordTopic ([], [], doc_words))
  return $ map (\(ys, sampleMap, prob) -> (ys, prob)) lwTrace

testTopicMHPost :: Int -> Int -> Sampler [([String], LPTrace)]
testTopicMHPost n_words n_samples = do
  mhTrace <- MH.mh n_samples (Example.documentDist vocabulary 2) ["φ", "θ"]
                        n_words (mkRecordTopic ([], [], doc_words))
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
                   ModelEnv Example.SIREnv)]   -- trace of samples
                <- Simulate.simulate 1 (runWriterM . Example.hmmSIRNsteps 100)
                   latState0 params
  let fstOutput = head simOutputs
      sirLog    :: [Example.LatState] = (snd . fst) fstOutput
      sampleMap :: ModelEnv Example.SIREnv = snd fstOutput
      infobs    :: [Int]                   = getOP #infobs sampleMap

      sirLog_tuples :: [(Int, Int, Int)] = map fromLatState sirLog

  return (sirLog_tuples, infobs)

infobs_data :: [Int]
infobs_data = [0,1,4,2,1,3,3,5,10,11,30,23,48,50,91,94,129,151,172,173,198,193,214,179,229,172,205,211,191,212,185,184,173,211,185,197,176,169,198,174,163,197,152,198,153,164,154,167,178,174,160,149,140,172,169,144,137,151,166,151,147,149,159,150,151,139,137,182,121,119,133,146,141,136,126,131,124,110,120,113,117,102,120,117,122,121,110,125,127,117,117,98,109,108,108,120,98,103,104,103]

-- testSIRMHPost :: Sampler ([Double], [Double], [Double])
-- testSIRMHPost = do
--   let mh_n_iterations = 5000
--   -- This demonstrates well the need for specifying the sample sites ["ρ", "β", "γ"].
--   mhTrace :: [((Example.LatState, [Example.LatState]), SDTrace, LPTrace)]
--           <- MH.mh mh_n_iterations (runWriterM . Example.hmmSIRNsteps 20) ["β", "γ", "ρ"]
--                         (latentState 762 1 0)
--                         (mkRecordSIR ([], [0.009], [], infobs_data))
--   let mhSampleMaps = map snd3 mhTrace
--       ρs = concatMap (MH.extractSamples (#ρ, Proxy @Double)) mhSampleMaps
--       βs = concatMap (MH.extractSamples (#β, Proxy @Double)) mhSampleMaps
--       γs = concatMap (MH.extractSamples (#γ, Proxy @Double)) mhSampleMaps
--   -- printS $ show (ρs, βs, γs)
--   return (ρs, βs, γs)

{- SIR Resusceptible -}
testSIRSBasic :: Sampler ([(Int, Int, Int)], -- sir values
                          [Int])
testSIRSBasic = do
  simOutputs <- Simulate.simulate 1 (runWriterM . Example.hmmSIRSNsteps 100)
                   (latentState 762 1 0)
                   (#β := [0.5] <:> #γ := [0.009] <:>  #ρ := [0.3] <:> #η := [0.05] <:> #infobs := [] <:> nil)

  let fstOutput = head simOutputs
      sirLog    :: [Example.LatState]         = (snd . fst) fstOutput
      sampleMap :: ModelEnv Example.SIRSEnv   = snd fstOutput
      infobs    :: [Int]                      = getOP #infobs sampleMap

      sirLog_tuples :: [(Int, Int, Int)] = map fromLatState sirLog

  return (sirLog_tuples, infobs)

{- SIRV -}
fromLatSIRVState :: Example.LatStateSIRV -> (Int, Int, Int, Int)
fromLatSIRVState (Example.LatStateSIRV sus inf recov vacc) = (sus, inf, recov, vacc)

testSIRVBasic :: Sampler ([(Int, Int, Int, Int)], -- sir values
                          [Int])                  -- observed infections
testSIRVBasic = do
  let latState0  = Example.LatStateSIRV { Example.s = 762, Example.i = 1, Example.r = 0,  Example.v = 0 }
      params     = #β := [0.5] <:> #γ := [0.009] <:>  #ρ := [0.3] <:> #ω := [0.04] <:>  #η := [0.05] <:> #infobs := [] <:> nil
  simOutputs :: [((Example.LatStateSIRV,        -- model output
                  [Example.LatStateSIRV]),      -- writer effect log of sir latent states
                   ModelEnv Example.SIRVEnv)]   -- trace of samples
                <- Simulate.simulate 1 (runWriterM . Example.hmmSIRVNsteps 100)
                   latState0 params
  let fstOutput = head simOutputs
      sirLog    :: [Example.LatStateSIRV]   = (snd . fst) fstOutput
      sampleMap :: ModelEnv Example.SIRVEnv = snd fstOutput
      infobs    :: [Int]                    = getOP #infobs sampleMap

      sirLog_tuples :: [(Int, Int, Int, Int)] = map fromLatSIRVState sirLog

  return (sirLog_tuples, infobs)
