{-# LANGUAGE AllowAmbiguousTypes, PolyKinds #-}

module Main where

import Example
import qualified Inference as Infer
import qualified Extensible.Example
import Model
import Data.Extensible

main :: IO ()
main = do
  -- let k = runModel (linearRegression 0 0 0) (y @= Nothing <: nil)
  x <- Infer.runModel (linearRegression 0 1 0) (y @= Just 5 <: nil)
  -- y <- Extensible.Example.runLR'
  print x
  return ()

  