{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications, UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Extensible.Test where

import qualified Data.Map as Map
import qualified Extensible.Example as Example
import qualified Extensible.Inference.Basic as Basic
import qualified Extensible.Inference.LW as LW
import qualified Extensible.Inference.MH as MH
import Extensible.Inference.Inf
import Extensible.Model
import Extensible.Sampler
import Data.Extensible



testLinRegr :: IO [(Double, Double)]
testLinRegr = do
  let -- Run basic over linearRegression
      rs   = runInf (Example.linearRegression 0 1)
                      [0, 1, 2, 3, 4]
                      (repeat nil)
                      Basic.runBasic
      -- Run basic over linearRegression'
      rs'  = runInf (Example.linearRegression' 0 1)
                      [0, 1, 2, 3, 4]
                      [(y @= Just (0.4 :: Double) <: nil)]
                      Basic.runBasic
  r   <- rs
  -- (r, p) <- LW.runLW (y @= Just (0.4 :: Double) <: nil) (Example.linearRegression 0 1 0)
  -- putStrLn $ show r ++ "\n" ++ show p
  print $ show r
  return r

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

testAndWrite :: Show a => IO a -> IO ()
testAndWrite prog = do
  -- a <- prog
  writeFile "../../test.txt" "hi"
  -- writeFile "/home/minh/Documents/model-output.txt" (show a)