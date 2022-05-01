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
import Effects.Dist
import qualified Inference.SIM as SIM
import qualified Inference.LW as LW
import qualified Inference.MH as MH
import OpenSum
import qualified OpenSum as OpenSum
import Effects.State
import Model
import Sampler
import Effects.ObsReader
import ModelEnv
import Util
import Debug.Trace
import Unsafe.Coerce
import Trace
import Old.Example (LatentState(LatentState))


{- Linear Regression -}
mkRecordLinRegr :: ([Double],  [Double],  [Double],  [Double]) -> ModelEnv Example.LinRegrEnv
mkRecordLinRegr (y_vals, m_vals, c_vals, σ_vals) =
  (#y := y_vals) <:> (#m := m_vals) <:> (#c := c_vals) <:> (#σ := σ_vals) <:> nil

mkRecordLinRegrY :: [Double] -> ModelEnv Example.LinRegrEnv
mkRecordLinRegrY y_vals =
  (#y := y_vals) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:> nil


testLinRegrSimOnce :: Int  -> Sampler [Double]
testLinRegrSimOnce n_datapoints  = do
  let xs = [0 .. fromIntegral n_datapoints]
      env_in = (#m := [3]) ∙ (#c := [5]) ∙ (#σ := [1]) ∙ (#y := []) ∙ nil
  (ys, env_out) <- SIM.simulateOnce Example.linearRegression env_in xs
  return ys

testLinRegrSim :: Int -> Int -> Sampler [(Double, Double)]
testLinRegrSim n_datapoints n_samples = do
  let xs = [0 .. fromIntegral n_datapoints]
  bs :: [([Double], ModelEnv Example.LinRegrEnv)]
      <- SIM.simulate n_samples Example.linearRegression
                    xs
                    (mkRecordLinRegr ([], [1.0], [0.0], [1.0]))
  return $ zip xs (concatMap fst bs)

testLinRegrLW :: Int -> Int -> Sampler [((Double, Double, Double), Double)]
testLinRegrLW n_datapoints n_samples = do
  let xs = [0 .. fromIntegral n_datapoints]
  lwTrace :: [([Double],              -- y data points
                ModelEnv Example.LinRegrEnv,    -- sample trace
                Double)]                        -- likelihood
          <- LW.lw n_samples (Example.linearRegression xs)
                   (mkRecordLinRegrY (map ((+2) . (*3)) xs))
  let lw_envs_out = map snd3 lwTrace
      mus        = concatMap (getOP #m) lw_envs_out
      cs         = concatMap (getOP #c) lw_envs_out
      σs         = concatMap (getOP #σ) lw_envs_out
      ps         = map thrd3 lwTrace
  return $ zip (zip3 mus cs σs) ps

-- Performs LW sampling *per* data point (x,y), rather than per set of input data (xs, ys)
testLinRegrLW' :: Int -> Int -> Sampler [((Double, Double, Double), Double)]
testLinRegrLW' n_datapoints n_samples = do
  let xs = [0 .. fromIntegral n_datapoints]
  lwTrace :: [([Double],              -- y data points
                ModelEnv Example.LinRegrEnv,    -- sample trace
                Double)]                        -- likelihood
          <- concat <$> mapM (\(x, y) -> LW.lw n_samples (Example.linearRegression [x]) (mkRecordLinRegrY [y]))
                                  (zip xs (map ((+2) . (*3)) xs))
  let lw_envs_out = map snd3 lwTrace
      mus        = concatMap (getOP #m) lw_envs_out
      cs         = concatMap (getOP #c) lw_envs_out
      σs         = concatMap (getOP #σ) lw_envs_out
      ps         = map thrd3 lwTrace
  printS ps
  return $ zip (zip3 mus cs σs) ps

testLinRegrMH :: Int -> Int -> Sampler [Double] --, [Double], [Double])
testLinRegrMH n_datapoints n_samples = do
  let n_datapoints' = fromIntegral n_datapoints
  mhTrace <- MH.mhTopLevel n_samples (Example.linearRegression [0 .. n_datapoints'])
                   (mkRecordLinRegrY (map ((+2) . (*3)) [0 .. n_datapoints'])) ["m", "c", "σ"]
  let mh_envs_out = map snd3 mhTrace
      mus        = concatMap (getOP #m) mh_envs_out
      cs         = concatMap (getOP #c) mh_envs_out
      σs         = concatMap (getOP #σ) mh_envs_out
  return mus

{- Log Regr -}
mkRecordLogRegr :: ([Bool], [Double], [Double]) -> ModelEnv Example.LogRegrEnv
mkRecordLogRegr (label_vals, m_vals, b_vals) =
  #label := label_vals <:> #m := m_vals <:> #b := b_vals <:> nil

mkRecordLogRegrL :: [Bool] -> ModelEnv Example.LogRegrEnv
mkRecordLogRegrL label_val =
 #label := label_val <:> #m := [] <:> #b := [] <:> nil

testLogRegrSim :: Int -> Int -> Sampler [(Double, Bool)]
testLogRegrSim n_datapoints n_samples = do
  let incr = 200/fromIntegral n_datapoints
      xs = [ (-100 + (fromIntegral x)*incr)/50 | x <- [0 .. n_datapoints]]
  bs <- SIM.simulate n_samples Example.logisticRegression
                         xs
                         (mkRecordLogRegr ([], [2], [-0.15]))
  return $ concatMap fst bs

testLogRegrLW :: Int -> Int -> Sampler [((Double, Double), Double)]
testLogRegrLW n_datapoints n_samples = do
  xys <- testLogRegrSim n_datapoints 1
  let xs = map fst xys
      ys = map snd xys
  lwTrace <- LW.lw n_samples (Example.logisticRegression xs) (mkRecordLogRegrL ys)
  let lw_envs_out = map snd3 lwTrace
      mus    = concatMap (getOP #m) lw_envs_out
      bs     = concatMap (getOP #b) lw_envs_out
      ps     = map thrd3 lwTrace
  return $ zip (zip mus bs) ps

testLogRegrMH :: Int -> Int -> Sampler ([Double], [Double])
testLogRegrMH n_datapoints n_samples = do
  xys <- testLogRegrSim n_datapoints 1
  let xs = map fst xys
      ys = map snd xys
  mhTrace <- MH.mhTopLevel n_samples (Example.logisticRegression xs) (mkRecordLogRegrL ys) ["m", "b"]
  let mh_envs_out = map snd3 mhTrace
      mus        = concatMap (getOP #m) mh_envs_out
      bs         = concatMap (getOP #b) mh_envs_out
      -- σs         = concatMap (getOP #σ) mh_envs_out
  return (mus, bs)

{- HMM -}
mkRecordHMM :: ([Int], Double, Double) -> ModelEnv Example.HMMEnv
mkRecordHMM (ys, transp, obsp) = #y := ys <:> #trans_p := [transp] <:> #obs_p := [obsp] <:>  nil

mkRecordHMMy :: [Int] -> ModelEnv Example.HMMEnv
mkRecordHMMy ys = #y := ys <:> #trans_p := [] <:> #obs_p := [] <:>  nil

testHMMSim :: Int -> Int -> Sampler [(Int, Int)]
testHMMSim hmm_length n_samples = do
  bs <- SIM.simulate n_samples (runWriterM . Example.hmmNSteps hmm_length)
                          0 (mkRecordHMM ([], 0.9, 0.2))
  let sim_envs_out  = map snd bs
      xs :: [Int]   = concatMap (snd . fst) bs
      ys :: [Int]   = concatMap (getOP #y) sim_envs_out
  return $ zip xs ys

testHMMLW :: Int -> Int -> Sampler [((Double, Double), Double)]
testHMMLW hmm_length n_samples = do
  ys <- map snd <$> testHMMSim hmm_length 1
  lwTrace <- LW.lw n_samples (runWriterM @[Int] (Example.hmmNSteps hmm_length 0))
                             (mkRecordHMMy ys)
  let lw_envs_out = map snd3 lwTrace

      trans_ps    = concatMap (getOP #trans_p) lw_envs_out
      obs_ps      = concatMap (getOP #obs_p) lw_envs_out
      ps          = map thrd3 lwTrace
  return $ zip (zip trans_ps obs_ps) ps

testHMMMH :: Int -> Int -> Sampler ([Double], [Double])
testHMMMH hmm_length n_samples = do
  ys <- map snd <$> testHMMSim hmm_length 1
  mhTrace <- MH.mhTopLevel n_samples (runWriterM @[Int] $ Example.hmmNSteps hmm_length 0) (mkRecordHMMy ys) ["trans_p", "obs_p"]
  let mh_envs_out = map snd3 mhTrace
      trans_ps    = concatMap (getOP #trans_p) mh_envs_out
      obs_ps      = concatMap (getOP #obs_p) mh_envs_out
  return $ (trans_ps, obs_ps)

{- Topic model -}
vocab :: [String]
vocab = ["DNA", "evolution", "parsing", "phonology"]

doc_words :: [String]
doc_words     = ["DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA"]

mkRecordTopic :: ([[Double]], [[Double]], [String]) -> ModelEnv Example.TopicEnv
mkRecordTopic (tps, wps, ys) =  #θ := tps <:>  #φ := wps <:> #w := ys <:>nil

testTopicSim :: Int -> Int -> Sampler [String]
testTopicSim n_words n_samples = do
  bs <- SIM.simulate n_samples (Example.documentDist vocab 2)
                        n_words (mkRecordTopic ([[0.5, 0.5]], [[0.12491280814569208,1.9941599739151505e-2,0.5385152817942926,0.3166303103208638],
                                                               [1.72605174564027e-2,2.9475900240868515e-2,9.906011619752661e-2,0.8542034661052021]], []))
  return $ concatMap fst bs

testTopicLW :: Int -> Int -> Sampler [ (([[Double]], [[Double]]) , Double) ] -- this test is just for benchmark purposes.
testTopicLW n_words n_samples = do
  lwTrace <- LW.lw n_samples (Example.documentDist vocabulary 2 n_words) (mkRecordTopic ([], [], doc_words))
  let lw_envs_out = map snd3 lwTrace
      θs          = map (getOP #θ) lw_envs_out
      φs          = map (getOP #φ) lw_envs_out
      ps          = map thrd3 lwTrace
  return $ zip (zip θs φs) ps

testTopicMH :: Int -> Int -> Sampler ([ [[Double]]  ], [[[Double]]])
testTopicMH n_words n_samples = do
  mhTrace <- MH.mhTopLevel n_samples (Example.documentDist vocabulary 2 n_words) (mkRecordTopic ([], [], doc_words)) ["φ", "θ"]
  let mh_envs_out = map snd3 mhTrace
      θs          = map (getOP #θ) mh_envs_out
      φs          = map (getOP #φ) mh_envs_out
  return (θs, φs)

testTopicMHPred :: Int -> Int -> Sampler [String]
testTopicMHPred n_words n_samples = do
  (θs, φs) <- testTopicMH n_words n_samples
  let  θ   = last θs
       φ   = last φs
  -- liftS $ print $ "Using params " ++ show (topic_ps, word_ps)
  bs <- SIM.simulate 1 (Example.documentDist vocabulary 2) 10
        (mkRecordTopic (θ,  φ, []))
  return $ concatMap fst bs

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

testSIRSim :: Sampler ([(Int, Int, Int)], -- sir values
                          [Int])            -- observed infections
testSIRSim = do
  let latState0  = Example.LatState { Example.sus = 762, Example.inf = 1, Example.recov = 0 }
      params     = #β := [0.5] <:> #γ := [0.009] <:>  #ρ := [0.3] <:>  #infobs := [] <:> nil
  simOutputs :: [((Example.LatState,   -- model output
                  [Example.LatState]), -- writer effect log of sir latent states
                   ModelEnv Example.SIREnv)]   -- trace of samples
                <- SIM.simulate 1 (runWriterM . Example.hmmSIRNsteps 100)
                   latState0 params
  let fstOutput = head simOutputs
      sirLog    :: [Example.LatState] = (snd . fst) fstOutput
      sampleMap :: ModelEnv Example.SIREnv = snd fstOutput
      infobs    :: [Int]                   = getOP #infobs sampleMap

      sirLog_tuples :: [(Int, Int, Int)] = map fromLatState sirLog

  return (sirLog_tuples, infobs)

infobs_data :: [Int]
infobs_data = [0,1,4,2,1,3,3,5,10,11,30,23,48,50,91,94,129,151,172,173,198,193,214,179,229,172,205,211,191,212,185,184,173,211,185,197,176,169,198,174,163,197,152,198,153,164,154,167,178,174,160,149,140,172,169,144,137,151,166,151,147,149,159,150,151,139,137,182,121,119,133,146,141,136,126,131,124,110,120,113,117,102,120,117,122,121,110,125,127,117,117,98,109,108,108,120,98,103,104,103]

testSIRMH :: Sampler ([Double], [Double], [Double])
testSIRMH = do
  let mh_n_iterations = 5000
  -- This demonstrates well the need for specifying the sample sites ["ρ", "β", "γ"].
  mhTrace  <- MH.mhTopLevel mh_n_iterations (runWriterM @[Example.LatState] $ Example.hmmSIRNsteps 20 (latentState 762 1 0))
                        (mkRecordSIR ([], [0.009], [], infobs_data)) ["β", "γ", "ρ"]
  let mhSampleMaps = map snd3 mhTrace
      ρs = concatMap (getOP #ρ) mhSampleMaps
      βs = concatMap (getOP #β) mhSampleMaps
      γs = concatMap (getOP #γ) mhSampleMaps
  -- printS $ show (ρs, βs, γs)
  return (ρs, βs, γs)

{- SIR Resusceptible -}
testSIRSSim :: Sampler ([(Int, Int, Int)], -- sir values
                          [Int])
testSIRSSim = do
  simOutputs <- SIM.simulate 1 (runWriterM . Example.hmmSIRSNsteps 100)
                   (latentState 762 1 0)
                   (#β := [0.5] <:> #γ := [0.009] <:>  #ρ := [0.3] <:> #η := [0.05] <:> #infobs := [] <:> nil)

  let fstOutput = head simOutputs
      sirLog      :: [Example.LatState]         = (snd . fst) fstOutput
      sim_env_out :: ModelEnv Example.SIRSEnv   = snd fstOutput
      infobs      :: [Int]                      = getOP #infobs sim_env_out

      sirLog_tuples :: [(Int, Int, Int)] = map fromLatState sirLog

  return (sirLog_tuples, infobs)

{- SIRV -}
fromLatSIRVState :: Example.LatStateSIRV -> (Int, Int, Int, Int)
fromLatSIRVState (Example.LatStateSIRV sus inf recov vacc) = (sus, inf, recov, vacc)

testSIRVSim :: Sampler ([(Int, Int, Int, Int)], -- sir values
                          [Int])                  -- observed infections
testSIRVSim = do
  let latState0  = Example.LatStateSIRV { Example.s = 762, Example.i = 1, Example.r = 0,  Example.v = 0 }
      params     = #β := [0.5] <:> #γ := [0.009] <:>  #ρ := [0.3] <:> #ω := [0.04] <:>  #η := [0.05] <:> #infobs := [] <:> nil
  simOutputs :: [((Example.LatStateSIRV,        -- model output
                  [Example.LatStateSIRV]),      -- writer effect log of sir latent states
                   ModelEnv Example.SIRVEnv)]   -- trace of samples
                <- SIM.simulate 1 (runWriterM . Example.hmmSIRVNsteps 100)
                   latState0 params
  let fstOutput = head simOutputs
      sirLog      :: [Example.LatStateSIRV]   = (snd . fst) fstOutput
      sim_env_out :: ModelEnv Example.SIRVEnv = snd fstOutput
      infobs      :: [Int]                    = getOP #infobs sim_env_out

      sirLog_tuples :: [(Int, Int, Int, Int)] = map fromLatSIRVState sirLog

  return (sirLog_tuples, infobs)
