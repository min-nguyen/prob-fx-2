{-# LANGUAGE AllowAmbiguousTypes, PolyKinds #-}

{-# LANGUAGE DataKinds #-}
module Main where

import Data.List.Split
import Data.Tuple
import qualified Data.Map as Map
import DataSets
import qualified Inference.SIM as SIM
import qualified Inference.LW as LW
import qualified Inference.MH as MH
import Effects.Dist
import OpenSum as OpenSum
import Model
import Data.Extensible ()
import Sampler
import qualified TestSMC as TestSMC
import Util
import System.Environment
import Examples.LinRegr
import Examples.LogRegr
import Examples.SIR
import Examples.LDA
import Examples.HierarchicalLinRegr
import Examples.HierarchicalSchool
import Examples.GMM
import Examples.HMM

printThenWrite :: Show a => a -> IO ()
printThenWrite a = print a >> writeFile "model-output.txt" (show a)

availableCommands = "[simLinRegr, lwLinRegr, mhLinRegr, simSIR, simSIRS, simSIRSV, mhSIR, simLogRegr, lwLogRegr, mhLogRegr, simLDA, mhLDA, simHierchicalLinRegr, mhHierarchicalLinRegr]"

parseArgs :: String -> IO ()
parseArgs cmd = case cmd of
  "simLinRegr"  -> sampleIO simLinRegr >>= printThenWrite
  "lwLinRegr"   -> sampleIO lwLinRegr >>= printThenWrite
  "mhLinRegr"   -> sampleIO mhLinRegr >>= printThenWrite

  "simLogRegr"  -> sampleIO simLogRegr >>= printThenWrite
  "lwLogRegr"  -> sampleIO lwLogRegr >>= printThenWrite
  "mhLogRegr"   -> sampleIO mhLogRegr >>= printThenWrite

  "simSIR"      -> sampleIO simSIR >>= printThenWrite
  "mhSIR"       -> sampleIO mhSIR >>= printThenWrite
  "simSIRS"     -> sampleIO simSIRS >>= printThenWrite
  "simSIRSV"    -> sampleIO simSIRSV >>= printThenWrite

  "simHMM"      -> sampleIO simHMMw >>= printThenWrite
  "mhHMM"      -> sampleIO mhHMMw >>= printThenWrite

  "simLDA"      -> sampleIO simLDA >>= printThenWrite
  "mhLDA"       -> sampleIO mhLDA >>= printThenWrite

  "simHierarchicalLinRegr" -> sampleIO simHierarchicalLinRegr >>= printThenWrite
  "mhHierarchicalLinRegr" -> sampleIO mhHierarchicalLinRegr >>= printThenWrite

  "mhSchool"    -> sampleIO mhSchool >>= printThenWrite

  "simGMM"      -> sampleIO simGMM >>= printThenWrite
  "mhGMM"      -> sampleIO mhGMM >>= printThenWrite
  _             -> putStrLn $ "unrecognised command: " ++ cmd ++ "\n"
                           ++ "available commands: " ++ availableCommands

main :: IO ()
main = do
  -- trace <- sampleIOFixed (TestSMC.testLinRegrSMC 50 200)
  -- trace <- sampleIOFixed (TestSMC.testLinRegrRMSMC 50 100 20)
  -- trace <- sampleIOFixed (TestSMC.testLinRegrPMMH 30 10 1000)
  -- printThenWrite trace
  args <- getArgs
  case args of []      -> print $ "no arguments provided to Wasabaye. Available arguments: " ++ availableCommands
               (a:as)  -> parseArgs a
