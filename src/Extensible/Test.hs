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

mkRecordLinRegr :: (Maybe Double, Maybe Double, Maybe Double, Maybe Double) -> MRec Example.LinRegrEnv
mkRecordLinRegr (y_val, m_val, c_val, σ_val) =
  y @= y_val <: m @= m_val <: c @= c_val <: σ @= σ_val <: nil

mkRecordLinRegrY :: Double -> MRec Example.LinRegrEnv
mkRecordLinRegrY y_val =
 y @= Just y_val <: m @= Nothing <: c @= Nothing <: σ @= Nothing <: nil

testLinRegrBasic :: Sampler [(Double, Double)]
testLinRegrBasic = do
  let -- Run basic simulation over linearRegression
      bs   = runInf Example.linearRegression
                    [0, 1, 2, 3, 4]
                    (repeat $ mkRecordLinRegr (Nothing, Just 1, Just 0, Just 1))
                    Basic.runBasic
      -- Run basic inference over linearRegression
      bs'  = runInf Example.linearRegression
                    [0, 1, 2, 3, 4]
                    (map mkRecordLinRegrY [-0.3, 0.75, 2.43, 3.5, 3.2])
                    Basic.runBasic
  output <- bs'
  liftS $ print $ show output
  return output

testLinRegrLW :: Sampler [[((Double, Double), Double)]]
testLinRegrLW = do
  let lws = LW.lw 3 Example.linearRegression
                    [0, 1, 2, 3, 4]
                    (repeat $ mkRecordLinRegr (Nothing, Just 1, Just 0, Just 1))
      lws' = LW.lw 3 Example.linearRegression
                    [0, 1, 2, 3, 4]
                    (map mkRecordLinRegrY [-0.3, 0.75, 2.43, 3.5, 3.2])
  output <- lws'
  liftS $ print $ show output
  return output

testLinRegrMH :: Sampler [((Double, Double), MH.Ⲭ, MH.LogP)]
testLinRegrMH = do
  let
      mhs  = MH.mh 3 Example.linearRegression [1,2,3]
                     (repeat $ mkRecordLinRegr (Nothing, Just 1, Just 0, Just 1))
      mhs' = MH.mh 3 Example.linearRegression [1,2,3]
                     (map mkRecordLinRegrY [-0.3, 1.6, 3.5])
  output <- mhs'
  liftS $ print $ show output
  return output

-- testLogRegr :: IO ()
-- testLogRegr = do
--   let -- Run basic simulation over logisticRegression
--       bs =
--   -- (x, samples, logps)
--   --   <- sampleIO $ MH.runMH (label @= Just True <: nil) Map.empty 0
--   --      (Example.logisticRegression (-1))
--   -- (x, samples, logps)
--   --   <- sampleIOFixed $ MH.mhNsteps 5 (label @= Just True <: nil)
--   --      (Example.logisticRegression 10)
--   -- putStrLn $ show x ++ "\n" ++ show samples ++ "\n" ++ show logps
--   return ()
