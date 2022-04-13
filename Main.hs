{-# LANGUAGE AllowAmbiguousTypes, PolyKinds #-}

{-# LANGUAGE DataKinds #-}
module Main where

import Data.List.Split
import Data.Tuple
import qualified Data.Map as Map
import DataSets
import qualified Example as Example
import qualified Inference.Simulate as Simulate
import qualified Inference.LW as LW
import qualified Inference.MH as MH
import qualified Experiment as Experiment
import OpenSum as OpenSum
import Model
import Data.Extensible ()
import Sampler
import Test
import qualified TestSMC as TestSMC
import Util

main :: IO ()
main = do
  -- trace <- sampleIOFixed $ testLinRegrSim 10 1
  -- trace <- sampleIO $ testLinRegrLW 10 2000
  -- trace <- sampleIO $ testLinRegrLW' 10 2000
  -- trace <- sampleIO $ testLinRegrMH 10 2000
  -- trace <- sampleIOFixed $ testLogRegrSim 200 100
  -- trace <- sampleIOFixed $ testLogRegrLW 200 100
  -- trace <- sampleIOFixed $ testLogRegrMH 20 2000
  -- trace <- sampleIOFixed $ testHMMSim 20 10
  -- trace <- sampleIO $ testHMMLW 10 2000
  -- trace <- sampleIO $ testHMMMH 10 2000
  -- trace <- sampleIOFixed $ testTopicSim 10 10
  -- trace <- sampleIOFixed $ testTopicMHPost 10 1000
  -- trace <- sampleIOFixed $ testTopicMHPred 10 1000
  -- trace <- sampleIOFixed testSIRSim
  -- trace <- sampleIOFixed testSIRMH
  -- trace <- sampleIOFixed testSIRSSim
  -- trace <- sampleIOFixed testSIRVSim
  trace <- sampleIO (TestSMC.testLinRegrSMC 10 10)
  -- trace <- sampleIO (TestSMC.testLinRegrSIS 10 10)
  let traceStr = show trace
  putStrLn traceStr
  writeFile "model-output.txt" traceStr
  return ()

