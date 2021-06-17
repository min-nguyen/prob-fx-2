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

main :: IO ()
main = do
  y <- Basic.runBasic (y @= Just (0.4 :: Double) <: nil) (Example.linearRegression' 0 0 0)
    -- 
    -- (x, samples, logps)
    -- <- sampleIO $ MH.runMH (label @= Just True <: nil) Map.empty 0 (Extensible.Example.logisticRegression (-1))
  -- (x, samples, logps)
  --   <- sampleIOFixed $ MH.mhNsteps 5 (label @= Just True <: nil) (runModel $ Extensible.Example.logisticRegression (10))
  -- putStrLn $ show x ++ "\n" ++ show samples ++ "\n" ++ show logps
  return ()

  