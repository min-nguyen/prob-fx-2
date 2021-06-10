{-# LANGUAGE AllowAmbiguousTypes, PolyKinds #-}

{-# LANGUAGE DataKinds #-}
module Main where

import Example
import Data.Tuple
import qualified Data.Map as Map
import qualified Inference as Infer
import qualified Extensible.Example
import qualified Extensible.Inference.LW as LW
import qualified Extensible.Inference.MH as MH
import Extensible.OpenSum as OpenSum
import Model
import Data.Extensible
import Extensible.Sampler

main :: IO ()
main = do
  -- let k = runModel (linearRegression 0 0 0) (y @= Nothing <: nil)
  -- x <- Infer.runModel (linearRegression 0 1 0) (y @= Just 5 <: nil)
  y <- LW.runLW (y @= Just 0.4 <: nil) (Extensible.Example.linearRegression' 0 1 0)
  (x, samples, logps)
    <- sampleIO $ MH.runMH (label @= Just True <: nil) Map.empty 0 (Extensible.Example.logisticRegression (-1))
  (x, samples, logps)
    <- sampleIOFixed $ MH.mhNsteps 5 (label @= Just True <: nil) (Extensible.Example.logisticRegression (10))
  putStrLn $ show x ++ "\n" ++ show samples ++ "\n" ++ show logps

  return ()

  