{-# LANGUAGE AllowAmbiguousTypes, PolyKinds #-}


module Main where

import LinRegr
import LogRegr
import SIR
import LDA
import Radon
import HMM
import GMM
import School
import qualified GuidedExamples
import Sampler
import System.Environment (getArgs)

printThenWrite :: Show a => a -> IO ()
printThenWrite a = print a >> writeFile "model-output.txt" (show a)

parseArgs :: String -> IO ()
parseArgs cmd = case cmd of
  "gbbviLinRegr"   -> sampleIOFixed (GuidedExamples.bbviLinRegr 200 40 8) >>= printThenWrite
  "gmleLinRegr"    -> sampleIOFixed (GuidedExamples.mleLinRegr 1400 40 8) >>= printThenWrite -- need to set 'safeAddGrad' to only add mu

  "simLinRegr"          -> sampleIOFixed (simLinRegr 100) >>= printThenWrite
  "lwLinRegr"           -> sampleIOFixed (lwLinRegr 1000 10)>>= printThenWrite
  "imLinRegr"           -> sampleIOFixed (imLinRegr 5000 50) >>= printThenWrite
  "ssmhLinRegr"         -> sampleIOFixed (ssmhLinRegr 5000 50) >>= printThenWrite
  "smcLinRegr"          -> sampleIOFixed (smcLinRegr 100 50) >>= printThenWrite
  "rmpfLinRegr"         -> sampleIOFixed (rmpfLinRegr 100 200 20) >>= printThenWrite
  "pmhLinRegr"          -> sampleIOFixed (pmhLinRegr 1000 20 30) >>= printThenWrite

  "simLogRegr"          -> sampleIOFixed (simLogRegr 50) >>= printThenWrite
  "lwLogRegr"           -> sampleIOFixed (lwLogRegr 100 10) >>= printThenWrite
  "ssmhLogRegr"         -> sampleIOFixed (ssmhLogRegr 500 10) >>= printThenWrite

  "simHMM"              -> sampleIOFixed (simHMM_WR 20) >>= printThenWrite
  "lwHMM"               -> sampleIOFixed (lwHMM 200 20) >>= printThenWrite
  "ssmhHMM"             -> sampleIOFixed (ssmhHMM 1000 20) >>= printThenWrite
  "smcHMM"              -> sampleIOFixed (smcHMM 200 30) >>= printThenWrite
  "rmpfHMM"             -> sampleIOFixed (rmpfHMM 50 50 30) >>= printThenWrite
  "pmhHMM"              -> sampleIOFixed (pmhHMM 1000 5 20) >>= printThenWrite

  "simSIR"              -> sampleIOFixed (simSIR 100) >>= printThenWrite
  "simSIRS"             -> sampleIOFixed (simSIRS 100) >>= printThenWrite
  "simSIRSV"            -> sampleIOFixed (simSIRSV 100) >>= printThenWrite
  "mhSIR"               -> sampleIOFixed (ssmhSIR 1000 100) >>= printThenWrite

  "simLDA"              -> sampleIOFixed (simLDA 100) >>= printThenWrite
  "ssmhLDA"             -> sampleIOFixed (ssmhLDA 500 100) >>= printThenWrite
  "smcLDA"              -> sampleIOFixed (smcLDA 100 100) >>= printThenWrite
  "rmpfLDA"             -> sampleIOFixed (rmpfLDA 10 30 100) >>= printThenWrite
  "pmhLDA"              -> sampleIOFixed (pmhLDA 100 20 100) >>= printThenWrite

  "simRadon"            -> sampleIOFixed simRadon >>= printThenWrite
  "ssmhRadon"           -> sampleIOFixed (ssmhRadon 1000) >>= printThenWrite
  "ssmhPredRadon"       -> sampleIOFixed (ssmhPredRadon 1500) >>= printThenWrite

  "ssmhSchool"          -> sampleIOFixed ssmhSchool >>= printThenWrite

  "simGMM"              -> sampleIOFixed (simGMM 20) >>= printThenWrite
  "ssmhGMM"             -> sampleIOFixed (ssmhGMM 2000 20) >>= printThenWrite


  _             -> putStrLn $ "unrecognised argument: " ++ cmd ++ "\n"

main :: IO ()
main = do
  args <- getArgs
  case args of []      -> print $ "no arguments provided to ProbFX."
               (a:as)  -> parseArgs a
