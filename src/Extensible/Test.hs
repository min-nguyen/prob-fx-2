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


mkRecordLinRegr :: [Double] -> [MRec Example.LinRegrEnv]
mkRecordLinRegr = map (\y_val -> (y @= Just y_val) <: nil)

testLinRegrBasic :: Sampler [(Double, Double)]
testLinRegrBasic = do
  let -- Run basic simulation over linearRegression
      bs   = runInf (Example.linearRegression 0 1)
                      [0, 1, 2, 3, 4]
                      (repeat (y @= Nothing <: nil))
                      Basic.runBasic
      -- Run basic inference over linearRegression'
      bs'  = runInf (Example.linearRegression 0 1)
                      [0, 1, 2, 3, 4]
                      (mkRecordLinRegr [-0.3, 0.75, 2.43, 3.5, 3.2])
                      Basic.runBasic
  output <- bs'
  liftS $ print $ show output
  return output

testLinRegrLW :: Sampler [((Double, Double), Double)]
testLinRegrLW = do
  let lws   = runInf (Example.linearRegression 0 1)
                      [0, 1, 2, 3, 4]
                      (repeat (y @= Nothing <: nil))
                      LW.runLW
      lws'  = runInf (Example.linearRegression 0 1)
                      [0, 1, 2, 3, 4]
                      (mkRecordLinRegr [-0.3, 0.75, 2.43, 3.5, 3.2])
                      LW.runLW
  output <- lws'
  -- (r, p) <- LW.runLW (y @= Just (0.4 :: Double) <: nil) (Example.linearRegression 0 1 0)
  -- putStrLn $ show r ++ "\n" ++ show p
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
