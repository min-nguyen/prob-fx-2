{-# LANGUAGE AllowAmbiguousTypes, PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Main where

import LinRegr
import LogRegr
import SIR
import LDA
import Radon
import School
import HMM
import GMM
import Sampler
import System.Environment (getArgs)

printThenWrite :: Show a => a -> IO ()
printThenWrite a = print a >> writeFile "model-output.txt" (show a)

parseArgs :: String -> IO ()
parseArgs cmd = case cmd of
  "simLinRegrOnce"  -> sampleIOFixed (simLinRegrOnce 50) >>= printThenWrite
  "lwLinRegrOnce"   -> sampleIOFixed (lwLinRegrOnce 50 30) >>= printThenWrite
  "mhLinRegrOnce"   -> sampleIOFixed (mhLinRegrOnce 50 10) >>= printThenWrite

  "simLinRegr"  -> sampleIOFixed (simLinRegr 100) >>= printThenWrite
  "lwLinRegr"   -> sampleIOFixed (lwLinRegr 1000 10)>>= printThenWrite
  "mhLinRegr"   -> sampleIOFixed (mhLinRegr 10000 50) >>= printThenWrite
  "smcLinRegr"  -> sampleIOFixed (smcLinRegr 120 50) >>= printThenWrite

  "simLogRegrOnce"  -> sampleIOFixed (simLogRegrOnce 50) >>= printThenWrite
  "lwLogRegrOnce"   -> sampleIOFixed (lwLogRegrOnce 10 10) >>= printThenWrite
  "mhLogRegrOnce"   -> sampleIOFixed (mhLogRegrOnce 50 10) >>= printThenWrite

  "simLogRegr"  -> sampleIOFixed (simLogRegr 50) >>= printThenWrite
  "lwLogRegr"   -> sampleIOFixed (lwLogRegr 100 10) >>= printThenWrite
  "mhLogRegr"   -> sampleIOFixed (mhLogRegr 1000 10) >>= printThenWrite

  "simHMM"      -> sampleIOFixed (simHMMw 20) >>= printThenWrite
  "mhHMM"       -> sampleIOFixed (mhHMMw 5000 20) >>= printThenWrite
  "smcHMM"      -> sampleIOFixed (smcHMMw 80 30) >>= printThenWrite

  "simSIR"      -> sampleIOFixed (simSIR 100) >>= printThenWrite
  "simSIRS"     -> sampleIOFixed (simSIRS 100) >>= printThenWrite
  "simSIRSV"    -> sampleIOFixed (simSIRSV 100) >>= printThenWrite
  "mhSIR"       -> sampleIOFixed (mhSIR 1000 100) >>= printThenWrite

  "simLDA"      -> sampleIOFixed (simLDA 100) >>= printThenWrite
  "mhPredLDA"   -> sampleIOFixed (mhPredLDA 500 100) >>= printThenWrite
  "smcPredLDA"  -> sampleIOFixed (smcPredLDA 100 100) >>= printThenWrite

  "simRadon"    -> sampleIOFixed simRadon >>= printThenWrite
  "mhRadon"     -> sampleIOFixed (mhRadon 1000) >>= printThenWrite
  "mhPredRadon" -> sampleIOFixed (mhPredRadon 1500) >>= printThenWrite

  "mhSchool"    -> sampleIOFixed (mhSchool 1000) >>= printThenWrite

  "simGMM"      -> sampleIOFixed (simGMM 20) >>= printThenWrite
  "mhGMM"       -> sampleIOFixed (mhGMM 4000 20) >>= printThenWrite


  _             -> putStrLn $ "unrecognised argument: " ++ cmd ++ "\n"

main :: IO ()
main = do
  args <- getArgs
  case args of []      -> print $ "no arguments provided to ProbFX."
               (a:as)  -> parseArgs a
