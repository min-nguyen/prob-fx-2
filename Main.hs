{-# LANGUAGE AllowAmbiguousTypes, PolyKinds #-}

{-# LANGUAGE DataKinds #-}
module Main where

import Data.List.Split
import Data.Tuple
import qualified Data.Map as Map
import Extensible.DataSets
import qualified Extensible.Example as Example
import Extensible.EffExample
import qualified Extensible.Inference.Simulate as Simulate
import qualified Extensible.Inference.LW as LW
import qualified Extensible.Inference.MH as MH
import qualified Extensible.Experiment as Experiment
import Extensible.OpenSum as OpenSum
import Extensible.Model
import Data.Extensible ()
import Extensible.Sampler
import Extensible.Test
import qualified Extensible.TestSMC as TestSMC
import Util

main :: IO ()
main = do
  trace <- sampleIOFixed $ testLinRegrBasic 200 100
  trace <- sampleIOFixed $ testLinRegrMHPost 200 100
  trace <- sampleIOFixed $ testLogRegrBasic 200 100
  trace <- sampleIOFixed testSIRBasic
  trace <- sampleIOFixed testSIRMHPost
  trace <- sampleIOFixed testSIRSBasic
  trace <- sampleIOFixed testSIRVBasic
  trace <- sampleIO (TestSMC.testLinRegrSMC 20 101)
  trace <- sampleIO (TestSMC.testLinRegrSIS 6 100)
  let traceStr = show trace
  putStrLn traceStr
  writeFile "model-output.txt" traceStr
  return ()

