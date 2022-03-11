{-# LANGUAGE AllowAmbiguousTypes, PolyKinds #-}

{-# LANGUAGE DataKinds #-}
module Main where

import Data.List.Split
import Data.Tuple
import qualified Data.Map as Map
import DataSets
import qualified Example as Example
import EffExample
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
  -- trace <- sampleIO $ testLinRegrMH 10 2000
  -- trace <- sampleIOFixed $ testLogRegrSim 200 100
  -- trace <- sampleIOFixed $ testLogRegrLW 200 100
  trace <- sampleIOFixed $ testLogRegrMH 20 2000
  -- trace <- sampleIOFixed $ testHMMSim 20 10
  -- trace <- sampleIO $ testHMMLW 10 2000
  -- trace <- sampleIO $ testHMMMH 10 2000
  -- trace <- sampleIOFixed testSIRBasic
  -- trace <- sampleIOFixed testSIRMHPost
  -- trace <- sampleIOFixed testSIRSBasic
  -- trace <- sampleIOFixed testSIRVBasic
  -- trace <- sampleIO (TestSMC.testLinRegrSMC 20 101)
  -- trace <- sampleIO (TestSMC.testLinRegrSIS 6 100)
  let traceStr = show trace
  putStrLn traceStr
  writeFile "model-output.txt" traceStr
  return ()

