{-# LANGUAGE AllowAmbiguousTypes, PolyKinds #-}

{-# LANGUAGE DataKinds #-}
module Main where

import Data.List.Split
import Data.Tuple
import qualified Data.Map as Map
import Extensible.DataSets
import qualified Extensible.Example as Example
import qualified Extensible.ExamplePaper as ExamplePaper
import Extensible.EffExample
import qualified Extensible.Inference.Simulate as Simulate
import qualified Extensible.Inference.LW as LW
import qualified Extensible.Inference.MH as MH
import Extensible.OpenSum as OpenSum
import Extensible.Model
import Data.Extensible ()
import Extensible.Sampler
import Extensible.Test
import qualified Extensible.TestPaper as TestPaper
import Util
-- testAndWrite :: Show a => Sampler a -> IO ()
-- testAndWrite prog = do
--   a <- sampleIOFixed prog
--   writeFile "model-output.txt" (show a)

-- replaceWord :: String -> String -> String -> String
-- replaceWord word replace s =
--   let repl ',' = " , "
--       repl '(' = " ( "
--       repl ')' = " ) "
--       repl x   = [x]
--       s'  = concat $ map repl s
--       s'' = concat $ map (\w -> if w == word then replace else w) $ splitOn " " s'
--   in  s''

main :: IO ()
main = do
  -- trace <- sampleIOFixed testLinRegrBasic
  -- trace <- sampleIOFixed testLinRegrLWSim
  -- trace <- sampleIOFixed testLinRegrLWInf
  -- trace <- sampleIOFixed testLinRegrMHPost
  -- trace <- sampleIOFixed testLinRegrMHPred
  -- trace <- sampleIOFixed testLogRegrBasic
  -- trace <- sampleIOFixed testLogRegrLWSim
  -- trace <- sampleIOFixed testLogRegrLWInf
  -- trace <- sampleIOFixed testLogRegrMHPost
  -- trace <- sampleIOFixed testLogRegrMHPred
  -- trace <- sampleIOFixed testNNLinBasic
  -- trace <- sampleIOFixed testNNLinLWSim
  -- trace <- sampleIOFixed testNNLinLWInf
  -- trace <- sampleIOFixed testNNLinMHPost
  -- trace <- sampleIOFixed testNNLinMHPred
  -- trace <- sampleIOFixed testNNStepBasic
  -- trace <- sampleIOFixed testNNStepLWSim
  -- trace <- sampleIOFixed testNNStepLWSim2
  -- trace <- sampleIOFixed testNNStepLWInf
  -- trace <- sampleIOFixed testNNStepMHPost
  -- trace <- sampleIOFixed testNNStepMHPred
  -- trace <- sampleIOFixed testNNLogBasic
  -- trace <- sampleIOFixed testNNLogBasic'
  -- trace <- sampleIOFixed testNNLogMHPost
  -- trace <- sampleIO testNNLogMHPred
  -- trace <- sampleIOFixed testSinBasic
  -- trace <- sampleIOFixed testSinLWSim
  -- trace <- sampleIOFixed testSinLWInf
  -- trace <- sampleIOFixed testSinMHPost
  -- trace <- sampleIOFixed testSinMHPred
  -- trace <- sampleIOFixed testHMMBasic
  -- trace <- sampleIOFixed testHMMLWSim
  -- trace <- sampleIOFixed testHMMLWInf
  -- trace <- sampleIO testHMMMHPost
  -- trace <- sampleIOFixed testHMMMHPred
  -- trace <- sampleIOFixed testHMMStBasic
  -- trace <- sampleIOFixed testSIRBasic
  -- trace <- sampleIOFixed testSIRLWInf
  -- trace <- sampleIOFixed testSIRMHPost
  -- trace <- sampleIOFixed testSIRMHPred
  -- trace <- sampleIOFixed testHalfNormal
  -- trace <- sampleIOFixed testTopicBasic
  -- trace <- sampleIO testTopicMHPost
  -- trace <- sampleIOFixed testTopicMHPred
  -- trace <- sampleIOFixed testTopicsMHPost
  -- trace <- sampleIOFixed testHLRBasic
  -- trace <- sampleIOFixed testHLRMHPost
  -- trace <- sampleIO testHLRMHPredictive
  -- trace <- sampleIOFixed testGMMBasic
  -- trace <- sampleIOFixed testGMMMHPost
  -- trace <- sampleIOFixed testSchBasic
  -- trace <- sampleIOFixed testSchMHPost
  -- trace <- sampleIOFixed TestPaper.testSIRBasic
  -- trace <- sampleIOFixed TestPaper.testSIRMHPost
  -- trace <- sampleIOFixed TestPaper.testSIRSBasic
  trace <- sampleIOFixed TestPaper.testSIRVBasic
  -- trace <- sampleIOFixed (TestPaper.testLinRegrBasic 200)
  let traceStr = show trace
  putStrLn traceStr
  writeFile "model-output.txt" traceStr
  return ()

