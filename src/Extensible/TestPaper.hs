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
import qualified Extensible.Inference.SimulateTrace as Simulate
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
import Extensible.Inference.SimulateTrace (extractSamples)

{- Util -}
mkRecordSIR :: ([Double], [Double], [Double]) -> LRec Example.SIREnv
mkRecordSIR (βv, γv, ρv) = #β := βv <:> #γ := γv <:>  #ρ := ρv <:>  #infobs := [] <:> nil

mkRecordSIRy :: [Int] -> LRec Example.SIREnv
mkRecordSIRy ys = #β := [] <:> #γ := [] <:>  #ρ := [] <:> #infobs := ys <:> nil

latentState :: Int -> Int -> Int -> Example.LatentState
latentState = Example.LatentState

fromLatentState :: Example.LatentState -> (Int, Int, Int)
fromLatentState (Example.LatentState sus inf recov) = (sus, inf, recov)

testSIRBasic :: Sampler ([(Int, Int, Int)], -- sir values
                          [Int])            -- observed infections
testSIRBasic = do
  simOutputs :: [((Example.LatentState,   -- model output
                  [Example.LatentState]), -- writer effect log of sir latent states
                   Simulate.SampleMap)]   -- trace of samples
                <- Simulate.simulateWith 1 (Example.hmmSIRNsteps 100)
                   [latentState 762 1 0]
                   [mkRecordSIR ([0.7], [0.009], [0.3])]
                   runWriterM
  let fstOutput = head simOutputs
      sirLog    :: [Example.LatentState] = (snd . fst) fstOutput
      sampleMap :: Simulate.SampleMap    = snd fstOutput
      infobs    :: [Int]                 = Simulate.extractSamples ("infobs", Proxy @Int) sampleMap

      sirLog_tuples :: [(Int, Int, Int)] = map fromLatentState sirLog

  return (sirLog_tuples, infobs)

{- Version of SIR simulation which instead directly composes runWriterM with the model, instead of using Simulate.simulateWith -}
testSIRBasic' :: Sampler [((Example.LatentState, [Example.LatentState]), Simulate.SampleMap)]
testSIRBasic' = do
  simOutputs :: [((Example.LatentState, [Example.LatentState]), Simulate.SampleMap)]
                <- Simulate.simulate 1 (runWriterM . Example.hmmSIRNsteps 100)
                   [latentState 762 1 0]
                   [mkRecordSIR ([0.7], [0.009], [0.3])]

  let fstOutput = head simOutputs
      sirLog    :: [Example.LatentState] = (snd . fst) fstOutput
      sampleMap :: Simulate.SampleMap    = snd fstOutput
      infobs    :: [Int]                 = Simulate.extractSamples ("infobs", Proxy @Int) sampleMap

  return simOutputs