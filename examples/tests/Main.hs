module Main (main) where

import Test.HUnit
import System.Exit
import Test.Inference ( testInference )


main :: IO ()
main = do
  Counts cases tried errors failures <- runTestTT testInference
  if errors + failures == 0
    then
      exitSuccess
    else do
      exitWith (ExitFailure 1)