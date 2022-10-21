module Main (main) where

import Test.HUnit
import System.Exit
import Test.Inference ( testInference )
import Test.Numerics ( testLogPdfs )


main :: IO ()
main = do
  Counts cases_inf tried_inf errors_inf failures_inf     <- runTestTT testInference
  Counts cases_pdfs tried_pdfs errors_pdfs failures_pdfs <- runTestTT testLogPdfs
  if errors_inf + failures_inf +
      errors_pdfs + failures_pdfs == 0
    then
      exitSuccess
    else do
      exitWith (ExitFailure 1)