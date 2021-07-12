{-# LANGUAGE AllowAmbiguousTypes, PolyKinds #-}

{-# LANGUAGE DataKinds #-}
module Main where

import Data.List.Split
import Data.Tuple
import qualified Data.Map as Map
import qualified Extensible.Example as Example
import qualified Extensible.Inference.Basic as Basic
import qualified Extensible.Inference.LW as LW
import qualified Extensible.Inference.MH as MH
import Extensible.OpenSum as OpenSum
import Extensible.Model
import Data.Extensible ()
import Extensible.Sampler
import Extensible.Test
import Util
-- testAndWrite :: Show a => Sampler a -> IO ()
-- testAndWrite prog = do
--   a <- sampleIOFixed prog
--   writeFile "model-output.txt" (show a)

-- removeWord :: String -> String -> String
-- removeWord word s =
--   let repl ',' = " , "
--       repl x   = [x]
--       s'  = concat $ map repl s
--       s'' = concat $ filter (not . (== word)) $ splitOn " " s'
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
  -- trace <- sampleIOFixed testNNLogMHPost
  -- trace <- sampleIOFixed testNNLogMHPred
  -- trace <- sampleIOFixed testSinBasic
  -- trace <- sampleIOFixed testSinLWSim
  -- trace <- sampleIOFixed testSinLWInf
  -- trace <- sampleIOFixed testSinMHPost
  -- trace <- sampleIOFixed testSinMHPred
  -- trace <- sampleIOFixed testHMMBasic
  -- trace <- sampleIOFixed testHMMLWSim
  -- trace <- sampleIOFixed testHMMLWInf
  trace <- sampleIOFixed testHMMMHPost
  -- trace <- sampleIOFixed testHMMStBasic
  let traceStr = show trace
  putStrLn traceStr
  writeFile "model-output.txt" traceStr
  return ()

