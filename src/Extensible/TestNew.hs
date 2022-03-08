{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications, UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}

module Extensible.TestNew where

import Data.Maybe
import qualified Data.Map as Map
import qualified Extensible.ExamplePaper as Example
import Extensible.DataSets
import Extensible.Dist
import qualified Extensible.Inference.Simulate as Simulate
import qualified Extensible.Inference.LW as LW
import qualified Extensible.Inference.MH as MH
import qualified Extensible.Inference.SMC as SMC
import Extensible.OpenSum
import Extensible.State
import Extensible.Model
import Extensible.Sampler
import Extensible.ObsReader
-- import Data.Extensible
import Extensible.ModelEnv
import Util
import Debug.Trace
import Unsafe.Coerce
import Extensible.STrace


mkRecordLinRegr :: ([Double],  [Double],  [Double],  [Double]) -> ModelEnv Example.LinRegrEnv
mkRecordLinRegr (y_vals, m_vals, c_vals, σ_vals) =
  (#y := y_vals) <:> (#m := m_vals) <:> (#c := c_vals) <:> (#σ := σ_vals) <:> nil

mkRecordLinRegrY :: [Double] -> ModelEnv Example.LinRegrEnv
mkRecordLinRegrY y_vals =
  (#y := y_vals) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:> nil

-- testLinRegrSMC' :: Sampler [((Double, Double), Double)]
testLinRegrSMC' :: Sampler [((Double, Double), STrace, Double)]
testLinRegrSMC' = do
  let n_samples = 1
      -- Run simulate simulation over linearRegression
      {- This should generate a set of points on the y-axis for each given point on the x-axis -}
  bs <- SMC.smc 8 (Example.linearRegressionOne 0.1)
                    (mkRecordLinRegr ([0.3], [], [1.0], []))
      {- This should output the provided fixed set of data points on the x and y axis. -}
  -- bs' <- Simulate.simulate n_samples Example.linearRegression
  --                   [0, 1, 2, 3, 4]
  --                   (map mkRecordLinRegrY [[-0.3], [0.75], [2.43], [3.5], [3.2]])
  return $ bs
