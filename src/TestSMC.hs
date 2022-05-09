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
import Effects.Dist
import qualified Inference.LW as LW
import qualified Inference.MH as MH
import qualified Inference.SMC as SMC
import qualified Inference.SIS as SIS
import qualified Inference.RMSMC as RMSMC
import qualified Inference.PMMH as PMMH
import OpenSum
import Effects.State
import Model
import Sampler
import Effects.ObsReader
-- import Data.Extensible
import Env
import Util
import Debug.Trace
import Unsafe.Coerce
import Trace

mkRecordLinRegr :: ([Double],  [Double],  [Double],  [Double]) -> Env Example.LinRegrEnv
mkRecordLinRegr (y_vals, m_vals, c_vals, σ_vals) =
  (#y := y_vals) <:> (#m := m_vals) <:> (#c := c_vals) <:> (#σ := σ_vals) <:> ENil

mkRecordLinRegrY :: [Double] -> Env Example.LinRegrEnv
mkRecordLinRegrY y_vals =
  (#y := y_vals) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:> ENil

testLinRegrSMC :: Int -> Int -> Sampler [Double]
testLinRegrSMC n_datapoints n_particles = do
  let n_datapoints' = fromIntegral n_datapoints
  bs <- SMC.smcToplevel n_particles (Example.linearRegression [0 .. n_datapoints'])
                    (mkRecordLinRegrY (map ((+0) . (*3)) [0 .. n_datapoints']))
  let envs = map (\(a, prob, env) -> env) bs
      mus  = concatMap (getO #m) envs
  return mus

testLinRegrRMSMC :: Int -> Int -> Int -> Sampler [Double]
testLinRegrRMSMC n_datapoints n_particles n_mh_steps = do
  let n_datapoints' = fromIntegral n_datapoints
  bs <- RMSMC.rmsmcToplevel n_particles n_mh_steps (Example.linearRegression [0 .. n_datapoints'])
                    (mkRecordLinRegrY (map ((+0) . (*3)) [0 .. n_datapoints']))
  let envs = map (\(a, prob, env) -> env) bs
      mus  = concatMap (getO #m) envs
  return mus

testLinRegrPMMH :: Int -> Int -> Int -> Sampler ([Double], [Double])
testLinRegrPMMH n_datapoints n_particles n_mh_steps = do
  let n_datapoints' = fromIntegral n_datapoints
      spec  = #m ⋮ #c ⋮ ONil
  bs <- PMMH.pmmhTopLevel n_mh_steps n_particles (Example.linearRegression [0 .. n_datapoints'])
                    (mkRecordLinRegrY (map ((+0) . (*3)) [0 .. n_datapoints'])) spec
  let envs = map (\(a, env, logp) -> env) bs
      mus  = concatMap (getO #m) envs
      cs   = concatMap (getO #c) envs
  return (mus, cs)
