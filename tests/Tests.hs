module Main (main) where

import System.Exit

import Test.HUnit
import Examples.LinRegr
import Examples.LogRegr
import Examples.SIR
import Examples.LDA
import Examples.Radon
import Examples.School
import Sampler
import System.Environment (getArgs)

printThenWrite :: Show a => a -> IO ()
printThenWrite a = print a >> writeFile "model-output.txt" (show a)

availableCommands = "[simLinRegr, lwLinRegr, mhLinRegr, simSIR, simSIRS, simSIRSV, mhSIR, simLogRegr, lwLogRegr, mhLogRegr, simLDA, mhLDA, simRadon, mhRadon, mhSchool]"

parseArgs :: String -> IO ()
parseArgs cmd = case cmd of
  "simLinRegr"  -> sampleIOFixed (simLinRegr 100) >>= printThenWrite
  "lwLinRegr"   -> sampleIOFixed (lwLinRegr 100 50) >>= printThenWrite
  "mhLinRegr"   -> sampleIOFixed (mhLinRegr 100 100) >>= printThenWrite

  "simLinRegrs"  -> sampleIOFixed (simLinRegrs 100) >>= printThenWrite
  "lwLinRegrs"   -> sampleIOFixed (lwLinRegrs 1000 10) >>= printThenWrite
  "mhLinRegrs"   -> sampleIOFixed (mhLinRegrs 10000 50) >>= printThenWrite

  "simSIR"      -> sampleIOFixed (simSIR 100) >>= printThenWrite
  "simSIRS"     -> sampleIOFixed (simSIRS 100) >>= printThenWrite
  "simSIRSV"    -> sampleIOFixed (simSIRSV 100) >>= printThenWrite
  "mhSIR"       -> sampleIOFixed (mhSIR 10000 100) >>= printThenWrite

  "simLogRegr"  -> sampleIOFixed (simLogRegr 50) >>= printThenWrite
  "lwLogRegr"   -> sampleIOFixed (lwLogRegr 10 10) >>= printThenWrite
  "mhLogRegr"   -> sampleIOFixed (mhLogRegr 1000 100) >>= printThenWrite

  "simLogRegrs"  -> sampleIOFixed (simLogRegrs 50) >>= printThenWrite
  "lwLogRegrs"   -> sampleIOFixed (lwLogRegrs 100 10) >>= printThenWrite
  "mhLogRegrs"   -> sampleIOFixed (mhLogRegrs 50000 50) >>= printThenWrite

  "simLDA"      -> sampleIOFixed (simLDA 100) >>= printThenWrite
  "mhLDA"       -> sampleIOFixed (mhLDA 500 100) >>= printThenWrite

  "simRadon"    -> sampleIOFixed simRadon >>= printThenWrite
  "mhRadon"     -> sampleIOFixed (mhRadon 1500) >>= printThenWrite

  "mhSchool"    -> sampleIOFixed (mhSchool 10000) >>= printThenWrite

  _             -> putStrLn $ "unrecognised command: " ++ cmd ++ "\n"
                           ++ "available commands: " ++ availableCommands

main :: IO ()
main = do
  args <- getArgs
  case args of []      -> print $ "no arguments provided to ProbFX. Available arguments: " ++ availableCommands
               (a:as)  -> parseArgs a
