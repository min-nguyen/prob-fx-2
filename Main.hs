{-# LANGUAGE AllowAmbiguousTypes, PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Examples.LinRegr
import Examples.LogRegr
import Examples.SIR
import Examples.LDA
import Examples.Radon
import Examples.School
import Examples.HMM
import Sampler
import System.Environment (getArgs)

printThenWrite :: Show a => a -> IO ()
printThenWrite a = print a >> writeFile "model-output.txt" (show a)

parseArgs :: String -> IO ()
parseArgs cmd = case cmd of
  "simLinRegr"  -> sampleIOFixed (simLinRegr 100) >>= printThenWrite
  "lwLinRegr"   -> sampleIOFixed (lwLinRegr 200 100) >>= printThenWrite
  "mhLinRegr"   -> sampleIOFixed (mhLinRegr 100 100) >>= printThenWrite

  "simLinRegrs"  -> sampleIOFixed (simLinRegrs 100) >>= printThenWrite
  "lwLinRegrs"   -> sampleIOFixed (lwLinRegrs 3000 10)>>= printThenWrite
  "mhLinRegrs"   -> sampleIOFixed (mhLinRegrs 10000 50) >>= printThenWrite

  "simLogRegr"  -> sampleIOFixed (simLogRegrs 50) >>= printThenWrite
  "lwLogRegr"   -> sampleIOFixed (lwLogRegrs 20000 50) >>= printThenWrite
  "mhLogRegr"   -> sampleIOFixed (mhLogRegrs 50000 50) >>= printThenWrite

  "simHMM"      -> sampleIOFixed (simHMMw 20) >>= printThenWrite
  "mhHMM"       -> sampleIOFixed (mhHMMw 5000 20) >>= printThenWrite

  "simSIR"      -> sampleIOFixed (simSIR 100) >>= printThenWrite
  "simSIRS"     -> sampleIOFixed (simSIRS 100) >>= printThenWrite
  "simSIRSV"    -> sampleIOFixed (simSIRSV 100) >>= printThenWrite
  "mhSIR"       -> sampleIOFixed (mhSIR 10000 100) >>= printThenWrite

  "simLDA"      -> sampleIOFixed (simLDA 100) >>= printThenWrite
  "mhLDA"       -> sampleIOFixed (mhLDA 500 100) >>= printThenWrite

  "simRadon"    -> sampleIOFixed simRadon >>= printThenWrite
  "mhRadon"     -> sampleIOFixed (mhRadon 1500) >>= printThenWrite

  "mhSchool"    -> sampleIOFixed (mhSchool 10000) >>= printThenWrite

  _             -> putStrLn $ "unrecognised argument: " ++ cmd ++ "\n"

main :: IO ()
main = do
  args <- getArgs
  case args of []      -> print $ "no arguments provided to ProbFX."
               (a:as)  -> parseArgs a
