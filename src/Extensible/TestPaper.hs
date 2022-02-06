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

module Extensible.TestPaper where

import Data.Maybe
import qualified Data.Map as Map
import qualified Extensible.ExamplePaper as Example
import Extensible.DataSets
import Extensible.Dist
import qualified Extensible.Inference.Simulate as Simulate
import qualified Extensible.Inference.LW as LW
import qualified Extensible.Inference.MH as MH
import Extensible.OpenSum
import qualified Extensible.OpenSum as OpenSum
import Extensible.Inference.Inf
import Extensible.State
import Extensible.Model
import Extensible.Sampler
import Extensible.AffineReader
import Extensible.OpenProduct
import Util
import Debug.Trace
import Unsafe.Coerce
{- Util -}


mkRecordSIR :: ([Double], [Double], [Double]) -> LRec Example.SIREnv
mkRecordSIR (ρv, βv, γv) = #infobs := [] <:> #ρ := ρv <:> #β := βv <:> #γ := γv <:> nil

mkRecordSIRy :: [Int] -> LRec Example.SIREnv
mkRecordSIRy ys = #infobs := ys <:> #ρ := [] <:> #β := [] <:> #γ := [] <:> nil

latentState :: Int -> Int -> Int -> Example.LatentState
latentState = Example.LatentState

fromLatentState :: Example.LatentState -> (Int, Int, Int)
fromLatentState (Example.LatentState sus inf recov) = (sus, inf, recov)

testSIRBasic :: Sampler ([(Int, Int, Int)], [Int])
testSIRBasic = do
  bs <- Simulate.simulate 1 (Example.hmmSIRNsteps 100) [latentState 762 1 0] [mkRecordSIR ([0.3], [0.7], [0.009])]
          --[mkRecordSIR ([0.29], [0.25], [0.015])]
  undefined
