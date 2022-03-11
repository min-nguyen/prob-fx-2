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
  trace <- sampleIO $ testLinRegrLWInf 10 2000
  -- trace <- sampleIO $ testLinRegrMHPost 10 2000
  -- trace <- sampleIOFixed $ testLogRegrBasic 200 100
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

