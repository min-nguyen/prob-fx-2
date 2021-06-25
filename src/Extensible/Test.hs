module Extensible.Test where

import qualified Data.Map as Map
import qualified Extensible.Example as Example
import qualified Extensible.Inference.Basic as Basic
import qualified Extensible.Inference.LW as LW
import qualified Extensible.Inference.MH as MH
import Extensible.Model
import Extensible.Sampler
import Data.Extensible

testLinRegr :: IO ()
testLinRegr = do
  let ms  = map (Example.linearRegression 0 1) [0, 1, 2, 3, 4]
      xs  = map (Basic.runBasic nil) ms
      ms' = map (Basic.runBasic (y @= Just (0.4 :: Double) <: nil))
            (map (Example.linearRegression' 0 1) [0, 1, 2, 3, 4])
  --map Basic.runBasic (Example.linearRegression 0 1) [0, 1, 2, 3, 4]
  -- r <- Basic.runBasic (y @= Just (0.4 :: Double) <: nil) (Example.linearRegression' 0 1 0)
  -- (r, p) <- LW.runLW (y @= Just (0.4 :: Double) <: nil) (Example.linearRegression 0 1 0)
  -- putStrLn $ show r ++ "\n" ++ show p
  return ()

testLogRegr :: IO ()
testLogRegr = do
  (x, samples, logps)
    <- sampleIO $ MH.runMH (label @= Just True <: nil) Map.empty 0
       (Example.logisticRegression (-1))
  (x, samples, logps)
    <- sampleIOFixed $ MH.mhNsteps 5 (label @= Just True <: nil)
       (Example.logisticRegression 10)
  -- putStrLn $ show x ++ "\n" ++ show samples ++ "\n" ++ show logps
  return ()