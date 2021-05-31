module Main where

import Example
-- import Inference.Basic
import Model 
import Data.Extensible

main :: IO ()
main = do 
  let k = runModel (linearRegression 0 0 0) (y @= Nothing <: nil)
  return ()
-- main :: IO ()
-- main = do 
--   let y = (allNothing :: MRec LinRegrEnv) 
--   runFull y (linearRegression 0 0 0)
--   return () 