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

module TestSMC where

import Data.Maybe
import qualified Data.Map as Map
import qualified Example as Example
import DataSets
import Dist
import qualified Inference.LW as LW
import qualified Inference.MH as MH
import qualified Inference.SMC as SMC
import qualified Inference.SMC_SIS as SMC_SIS
import qualified Inference.SIS as SIS
import OpenSum
import State
import Model
import Sampler
import ObsReader
-- import Data.Extensible
import ModelEnv
import Util
import Debug.Trace
import Unsafe.Coerce
import STrace

mkRecordLinRegr :: ([Double],  [Double],  [Double],  [Double]) -> ModelEnv Example.LinRegrEnv
mkRecordLinRegr (y_vals, m_vals, c_vals, σ_vals) =
  (#y := y_vals) <:> (#m := m_vals) <:> (#c := c_vals) <:> (#σ := σ_vals) <:> nil

mkRecordLinRegrY :: [Double] -> ModelEnv Example.LinRegrEnv
mkRecordLinRegrY y_vals =
  (#y := y_vals) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:> nil

-- testLinRegrSMC' :: Sampler [((Double, Double), Double)]
testLinRegrSMC :: Int -> Int -> Sampler [Double]
testLinRegrSMC n_datapoints n_particles = do
  let n_datapoints' = fromIntegral n_datapoints
  bs <- SMC.smc n_particles (Example.linearRegression [0 .. n_datapoints'])
                    (mkRecordLinRegrY (map ((+0) . (*3)) [0 .. n_datapoints']))
  let envs = map (\(a, env, prob) -> env) bs
      mus  = concatMap (getOP #m) envs
  return mus

testLinRegrSIS :: Int -> Int -> Sampler [Double]
testLinRegrSIS n_datapoints n_particles = do
  let n_datapoints' = fromIntegral n_datapoints
  bs <- SMC_SIS.smc n_particles (Example.linearRegression [0 .. n_datapoints'])
                    (mkRecordLinRegrY (map ((+0) . (*3)) [0 .. n_datapoints']))
  let envs = map (\(a, prob, env) -> env) bs
      mus  = concatMap (getOP #m) envs
  return mus
