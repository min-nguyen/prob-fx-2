{-# LANGUAGE AllowAmbiguousTypes, PolyKinds #-}

{-# LANGUAGE DataKinds #-}
module Main where


import Data.Tuple
import qualified Data.Map as Map
import qualified Inference as Infer
import qualified Extensible.Example as Example
import qualified Extensible.Inference.Basic as Basic
import qualified Extensible.Inference.LW as LW
import qualified Extensible.Inference.MH as MH
import Extensible.OpenSum as OpenSum
import Extensible.Model
import Data.Extensible ()
import Extensible.Sampler
import Extensible.Test

testAndWrite :: Show a => Sampler a -> IO ()
testAndWrite prog = do
  a <- sampleIOFixed prog
  writeFile "model-output.txt" (show a)

main :: IO ()
main = do
  -- testAndWrite testLinRegrBasic
  -- trace <- sampleIOFixed testLinRegrBasic
  -- trace <- sampleIOFixed testLinRegrLW
  -- trace <- sampleIOFixed testLinRegrMH
  -- writeFile "model-output.txt" (show trace)
  -- putStrLn $ show trace
  trace <- sampleIOFixed testLogRegrBasic
  return ()

