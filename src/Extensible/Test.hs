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
import Extensible.Model
import Extensible.Sampler
import Data.Extensible

runModels :: (b -> Model s es a) -> [b] -> [MRec s] ->
             (MRec s -> Model s es a -> IO c) ->
             IO [c]
runModels model xs ys inf = do
  let models = map model xs
      infs   = map inf ys
      ys'    = zipWith ($) infs models
  sequence ys'

testLinRegr :: IO ()
testLinRegr = do
  let -- Run basic over linearRegression
      infs  = map Basic.runBasic [nil]
      ys    = zipWith ($) infs (map (Example.linearRegression 0 1) [0, 1, 2, 3, 4])
      -- Run basic over linearRegression'
      infs' = map Basic.runBasic [(y @= Just (0.4 :: Double) <: nil)]
      ys'    = zipWith ($) infs' (map (Example.linearRegression' 0 1) [0, 1, 2, 3, 4])

      rs   = runModels (Example.linearRegression 0 1)
                      [0, 1, 2, 3, 4]
                      (repeat nil)
                      Basic.runBasic
      rs'  = runModels (Example.linearRegression' 0 1)
                      [0, 1, 2, 3, 4]
                      [(y @= Just (0.4 :: Double) <: nil)]
                      Basic.runBasic
  ys' <- sequence ys'
  r   <- rs
  -- (r, p) <- LW.runLW (y @= Just (0.4 :: Double) <: nil) (Example.linearRegression 0 1 0)
  -- putStrLn $ show r ++ "\n" ++ show p
  print $ show r
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