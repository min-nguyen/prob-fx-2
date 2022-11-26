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
  "simLinRegrOnce"      -> sampleIOFixed (simLinRegrOnce 50) >>= printThenWrite
  "lwLinRegrOnce"       -> sampleIOFixed (lwLinRegrOnce 50 30) >>= printThenWrite
  "mhLinRegrOnce"       -> sampleIOFixed (mhLinRegrOnce 50 10) >>= printThenWrite

  "simLinRegr"          -> sampleIOFixed (simLinRegr 100) >>= printThenWrite
  "lwLinRegr"           -> sampleIOFixed (lwLinRegr 1000 10)>>= printThenWrite
  "mhLinRegr"           -> sampleIOFixed (mhLinRegr 10000 50) >>= printThenWrite
  "smcLinRegr"          -> sampleIOFixed (smcLinRegr 120 50) >>= printThenWrite
  "rmsmcLinRegr"        -> sampleIOFixed (rmsmcLinRegr 20 200 20) >>= printThenWrite
  "pmmhLinRegr"         -> sampleIOFixed (pmmhLinRegr 1000 20 30) >>= printThenWrite
  "smc2LinRegr"         -> sampleIOFixed (smc2LinRegr 20 20 20 30) >>= printThenWrite
  "bbviLinRegr"         -> sampleIOFixed (bbviLinRegr 200 40 8) >>= printThenWrite
  "bbviDefaultLinRegr"  -> sampleIOFixed (bbviDefaultLinRegr 200 40 8) >>= printThenWrite
  "inviLinRegr"         -> sampleIOFixed (inviLinRegr 400 40 8) >>= printThenWrite
  "mleMcmcLinRegr"      -> sampleIOFixed (mleMcmcLinRegr 3000 50 8) >>= printThenWrite
  "mleLinRegr"          -> sampleIOFixed (mleLinRegr 276 40 8) >>= printThenWrite
  "mapLinRegr"          -> sampleIOFixed (mapLinRegr 276 40 8) >>= printThenWrite
  "invimapLinRegr"      -> sampleIOFixed (invimapLinRegr 300 40 8) >>= printThenWrite

  "simLogRegrOnce"      -> sampleIOFixed (simLogRegrOnce 50) >>= printThenWrite
  "lwLogRegrOnce"       -> sampleIOFixed (lwLogRegrOnce 10 10) >>= printThenWrite
  "mhLogRegrOnce"       -> sampleIOFixed (mhLogRegrOnce 50 10) >>= printThenWrite

  "simLogRegr"          -> sampleIOFixed (simLogRegr 50) >>= printThenWrite
  "lwLogRegr"           -> sampleIOFixed (lwLogRegr 100 10) >>= printThenWrite
  "mhLogRegr"           -> sampleIOFixed (mhLogRegr 1000 10) >>= printThenWrite

  "simHMM"              -> sampleIOFixed (simHMM_WR 20) >>= printThenWrite
  "lwHMM"               -> sampleIOFixed (lwHMM 200 20) >>= printThenWrite
  "mhHMM"               -> sampleIOFixed (mhHMM 1000 20) >>= printThenWrite
  "smcHMM"              -> sampleIOFixed (smcHMM 200 30) >>= printThenWrite
  "rmsmcHMM"            -> sampleIOFixed (rmsmcHMM 50 50 30) >>= printThenWrite
  "pmmhHMM"             -> sampleIOFixed (pmmhHMM 1000 5 20) >>= printThenWrite
  "smc2HMM"             -> sampleIOFixed (smc2HMM 100 50 4 20) >>= printThenWrite
  "bbviHMM"             -> sampleIOFixed (bbviHMM 1000 50 20) >>= printThenWrite
  "bbviDefaultHMM"      -> sampleIOFixed (bbviDefaultHMM 1000 50 20) >>= printThenWrite
  "inviHMM"             -> sampleIOFixed (inviHMM 1000 100 20) >>= printThenWrite

  "simSIR"              -> sampleIOFixed (simSIR 100) >>= printThenWrite
  "simSIRS"             -> sampleIOFixed (simSIRS 100) >>= printThenWrite
  "simSIRSV"            -> sampleIOFixed (simSIRSV 100) >>= printThenWrite
  "mhSIR"               -> sampleIOFixed (mhSIR 1000 100) >>= printThenWrite

  "simLDA"              -> sampleIOFixed (simLDA 100) >>= printThenWrite
  "mhLDA"               -> sampleIOFixed (mhLDA 500 100) >>= printThenWrite
  "smcLDA"              -> sampleIOFixed (smcLDA 100 100) >>= printThenWrite
  "rmsmcLDA"            -> sampleIOFixed (rmsmcLDA 10 30 100) >>= printThenWrite
  "pmmhLDA"             -> sampleIOFixed (pmmhLDA 100 20 100) >>= printThenWrite
  "smc2LDA"             -> sampleIOFixed (smc2LDA 20 20 20 50) >>= printThenWrite
  "bbviLDA"             -> sampleIOFixed (bbviLDA 200 20 50) >>= printThenWrite
  "bbviDefaultLDA"      -> sampleIOFixed (bbviDefaultLDA 200 20 50) >>= printThenWrite
  "inviLDA"             -> sampleIOFixed (inviLDA 200 20 50) >>= printThenWrite

  "simRadon"            -> sampleIOFixed simRadon >>= printThenWrite
  "mhRadon"             -> sampleIOFixed (mhRadon 1000) >>= printThenWrite
  "mhPredRadon"         -> sampleIOFixed (mhPredRadon 1500) >>= printThenWrite

  "mhSchool"            -> sampleIOFixed (mhSchool 1000) >>= printThenWrite

  "simGMM"              -> sampleIOFixed (simGMM 20) >>= printThenWrite
  "mhGMM"               -> sampleIOFixed (mhGMM 2000 20) >>= printThenWrite


  _             -> putStrLn $ "unrecognised argument: " ++ cmd ++ "\n"

main :: IO ()
main = do
  args <- getArgs
  case args of []      -> print $ "no arguments provided to ProbFX."
               (a:as)  -> parseArgs a
