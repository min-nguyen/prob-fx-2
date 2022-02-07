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
  -- Run simulate simulation over linearRegression
      {- This should generate a set of points on the y-axis for each given point on the x-axis -}
  bs <- Simulate.simulate n_samples Example.linearRegression
                    [[0 .. 100]]
                    [mkRecordLinRegr ([], [1.0], [0.0], [1.0])]
      {- This should output the provided fixed set of data points on the x and y axis. -}
  -- bs' <- Simulate.simulate n_samples Example.linearRegression
  --                   [0, 1, 2, 3, 4]
  --                   (map mkRecordLinRegrY [[-0.3], [0.75], [2.43], [3.5], [3.2]])
  return $ map fst bs

testLinRegrLWInf :: Sampler [([Double], Double)]
testLinRegrLWInf = do
  -- Run likelihood weighting inference over linearRegression
  {- This should output the provided fixed set of data points on the x and y axis, where each point has a different probability (due to us observing the probability of given y's). Also returns a trace of parameters and their likelihoods -}
  let  lw_n_iterations = 100
  lwTrace :: [([Double],       -- y data points
                LW.SampleMap,  -- sample trace
                Double)]       -- likelihood
          <- LW.lw lw_n_iterations Example.linearRegression
                    [[0 .. 100]]
                    (map (mkRecordLinRegrY . (:[]) ) (map ((+2) . (*3)) [0 .. 100]))
  return $ map (\(ys,sampleMap,prob) -> (ys, prob)) lwTrace

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