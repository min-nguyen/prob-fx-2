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
import TestSMC
import Util
import System.Environment
import Examples.LinRegr
import Examples.LogRegr
import Examples.SIR
import Examples.LDA
import Examples.HLinRegr
import Examples.School
import Examples.GMM
import Examples.HMM

printThenWrite :: Show a => a -> IO ()
printThenWrite a = print a >> writeFile "model-output.txt" (show a)

parseArgs :: String -> IO ()
parseArgs cmd = case cmd of
  "simLinRegrOnce"  -> sampleIO simLinRegrOnce >>= printThenWrite
  "lwLinRegrOnce"   -> sampleIO lwLinRegrOnce >>= printThenWrite
  "mhLinRegrOnce"   -> sampleIO mhLinRegrOnce >>= printThenWrite

  "simLinRegr"  -> sampleIO (simLinRegr 50 100) >>= printThenWrite
  "lwLinRegr"   -> sampleIO (lwLinRegr 20 3000)>>= printThenWrite
  "mhLinRegr"   -> sampleIO (mhLinRegr 50 5000) >>= printThenWrite

  "simLogRegr"  -> sampleIO simLogRegr >>= printThenWrite
  "lwLogRegr"   -> sampleIO lwLogRegr >>= printThenWrite
  "mhLogRegr"   -> sampleIO mhLogRegr >>= printThenWrite

  "simSIR"      -> sampleIO simSIR >>= printThenWrite
  "mhSIR"       -> sampleIO mhSIR >>= printThenWrite
  "simSIRS"     -> sampleIO simSIRS >>= printThenWrite
  "simSIRSV"    -> sampleIO simSIRSV >>= printThenWrite

  "simHMM"      -> sampleIO simHMMw >>= printThenWrite
  "mhHMM"       -> sampleIO mhHMMw >>= printThenWrite

  "simLDA"      -> sampleIO simLDA >>= printThenWrite
  "mhLDA"       -> sampleIO mhLDA >>= printThenWrite

  "simHLinRegr" -> sampleIO simHLinRegr >>= printThenWrite
  "mhHLinRegr"  -> sampleIO mhHLinRegr >>= printThenWrite

  "mhSchool"    -> sampleIO mhSchool >>= printThenWrite

  "simGMM"      -> sampleIO simGMM >>= printThenWrite
  "mhGMM"       -> sampleIO mhGMM >>= printThenWrite

  "smcLinRegr"   -> sampleIOFixed (smcLinRegr 50 200) >>= printThenWrite
  "rmsmcLinRegr" -> sampleIOFixed (rmsmcLinRegr 50 200 20) >>= printThenWrite
  "pmmhLinRegr"  -> sampleIOFixed (pmmhLinRegr 30 10 1000) >>= printThenWrite
  _              -> putStrLn $ "unrecognised command: " ++ cmd ++ "\n"

main :: IO ()
main = do
  -- trace <- sampleIOFixed (smcLinRegr 50 200)
  -- trace <- sampleIOFixed (rmsmcLinRegr 50 100 20)
  -- trace <- sampleIOFixed (pmmhLinRegr 30 10 1000)
  -- printThenWrite trace
  args <- getArgs
  case args of []      -> print "no arguments provided to Wasabaye"
               (a:as)  -> parseArgs a
