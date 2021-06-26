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
import Data.Extensible
import Extensible.Sampler
import Extensible.Test

testAndWrite' :: Show a => IO a -> IO ()
testAndWrite' prog = do
  -- a <- prog
  -- putStrLn (show a)
  writeFile "model-output.txt" "hi"

main :: IO ()
main = do
  -- writeFile "test.txt" "hi"
  a <- testLinRegr
  -- testAndWrite' testLinRegr
  return ()

