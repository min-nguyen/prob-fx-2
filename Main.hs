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
  trace <- sampleIOFixed testLinRegrMHPred
  -- trace <- sampleIOFixed testLogRegrBasic
  -- trace <- sampleIOFixed testLogRegrLW
  -- trace <- sampleIOFixed testLogRegrMH
  -- trace <- sampleIOFixed testNNBasic
  -- trace <- sampleIOFixed testNNLW
  -- trace <- sampleIOFixed testNNMH
  -- trace <- sampleIOFixed testNNMHSin
  -- trace <- sampleIOFixed testSin
  let traceStr = show trace
  putStrLn traceStr
  writeFile "model-output.txt" traceStr
  return ()

