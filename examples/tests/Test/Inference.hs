{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Test.Inference where

import Examples
import Test.Expected
import Test.HUnit
import Sampler
import System.Exit
import Util

testSimLinRegr :: Test
testSimLinRegr = TestCase $ do
  output <- sampleIOFixed (simLinRegr 100)
  assertEqual "Testing (simLinRegr 100)"  simLinRegrExpected output

testLwLinRegr :: Test
testLwLinRegr = TestCase $ do
  output <- sampleIOFixed (lwLinRegr 1000 10)
  assertEqual "Testing (lwLinRegr 1000 10)"  lwLinRegrExpected output

testMhLinRegr :: Test
testMhLinRegr = TestCase $ do
  output <- sampleIOFixed (mhLinRegr 10000 50)
  assertEqual "Testing (mhLinRegr 10000 50)"  mhLinRegrExpected output

testSmcLinRegr :: Test
testSmcLinRegr = TestCase $ do
  output <- sampleIOFixed (smcLinRegr 120 50)
  assertEqual "Testing (smcLinRegr 120 50)"  smcLinRegrExpected output

testRmpfLinRegr :: Test
testRmpfLinRegr = TestCase $ do
  output <- sampleIOFixed (rmpfLinRegr 20 200 20)
  assertEqual "Testing (rmpfLinRegr 20 200 20)"  rmpfLinRegrExpected output

testPmhLinRegr :: Test
testPmhLinRegr = TestCase $ do
  output <- sampleIOFixed (pmhLinRegr 1000 20 30)
  assertEqual "Testing (pmhLinRegr 1000 20 30)"  pmhLinRegrExpected output

testSimLogRegr :: Test
testSimLogRegr = TestCase $ do
  output <- sampleIOFixed (simLogRegr 50)
  assertEqual "Testing (simLogRegr 50)"  simLogRegrExpected output

testLwLogRegr :: Test
testLwLogRegr = TestCase $ do
  output <- sampleIOFixed (lwLogRegr 100 10)
  assertEqual "Testing (lwLogRegr 100 10)"  lwLogRegrExpected output

testMhLogRegr :: Test
testMhLogRegr = TestCase $ do
  output <- sampleIOFixed (mhLogRegr 500 10)
  assertEqual "Testing (mhLogRegr 500 10)"  mhLogRegrExpected output

testSimHMM_WR :: Test
testSimHMM_WR = TestCase $ do
  output <- sampleIOFixed (simHMM_WR 20)
  assertEqual "Testing (simHMM_WR 20)"  simHMM_WRExpected output

testLwHMM :: Test
testLwHMM = TestCase $ do
  output <- sampleIOFixed (lwHMM 200 20)
  assertEqual "Testing (lwHMM 200 20)" lwHMMExpected output

testMhHMM :: Test
testMhHMM = TestCase $ do
  output <- sampleIOFixed (mhHMM 1000 20)
  assertEqual "Testing (mhHMM 1000 20)"  mhHMMExpected output

testSmcHMM :: Test
testSmcHMM = TestCase $ do
  output <- sampleIOFixed (smcHMM 200 30)
  assertEqual "Testing (smcHMM 200 30)"  smcHMMExpected output

testRmpfHMM :: Test
testRmpfHMM = TestCase $ do
  output <- sampleIOFixed (rmpfHMM 50 50 30)
  assertEqual "Testing (rmpfHMM 50 50 30)"  rmpfHMMExpected output

testPmhHMM :: Test
testPmhHMM = TestCase $ do
  output <- sampleIOFixed (pmhHMM 1000 5 20)
  assertEqual "Testing (pmhHMM 1000 5 20)"  pmhHMMExpected output

testSimSIR :: Test
testSimSIR = TestCase $ do
  output <- sampleIOFixed (simSIR 100)
  assertEqual "Testing (simSIR 100)"  simSIRExpected output

testMhSIR :: Test
testMhSIR = TestCase $ do
  output <- sampleIOFixed (mhSIR 1000 100)
  assertEqual "Testing (mhSIR 1000 100)"  mhSIRExpected output

testSimLDA :: Test
testSimLDA = TestCase $ do
  output <- sampleIOFixed (simLDA 100)
  assertEqual "Testing (simLDA 100)"  simLDAExpected output

testMhLDA :: Test
testMhLDA = TestCase $ do
  output <- sampleIOFixed (mhLDA 500 100)
  let mhLDAExpected' = mapT2 (map (map (roundPrecision 8))) mhLDAExpected
      output'        = mapT2 (map (map (roundPrecision 8))) output
  assertEqual "Testing (mhLDA 500 100)"  mhLDAExpected' output'

testSmcLDA :: Test
testSmcLDA = TestCase $ do
  output <- sampleIOFixed (smcLDA 100 100)
  assertEqual "Testing (smcLDA 100 100)"  smcLDAExpected output

testRmpfLDA :: Test
testRmpfLDA = TestCase $ do
  output <- sampleIOFixed (rmpfLDA 10 30 100)
  let rmpfLDAExpected' = mapT2 (map (map (roundPrecision 8))) rmpfLDAExpected
      output'           = mapT2 (map (map (roundPrecision 8))) output
  assertEqual "Testing (rmpfLDA 10 30 100)"  rmpfLDAExpected' output'

testPmhLDA :: Test
testPmhLDA = TestCase $ do
  output <- sampleIOFixed (pmhLDA 100 20 100)
  let pmhLDAExpected' = mapT2 (map (map (roundPrecision 8))) pmhLDAExpected
      output'           = mapT2 (map (map (roundPrecision 8))) output
  assertEqual "Testing (pmhLDA 100 20 100)" pmhLDAExpected' output'

testMhRadon :: Test
testMhRadon = TestCase $ do
  output <- sampleIOFixed (mhRadon 1000)
  assertEqual "Testing (mhRadon 1000)"  mhRadonExpected output

testMhPredRadon :: Test
testMhPredRadon = TestCase $ do
  output <- sampleIOFixed (mhPredRadon 1500)
  assertEqual "Testing (mhPredRadon 1500)"  mhPredRadonExpected output

testInference :: Test
testInference = TestList
 [
   testSimLinRegr
 , testLwLinRegr
 , testMhLinRegr
 , testSmcLinRegr
 , testRmpfLinRegr
 , testPmhLinRegr
 , testSimLogRegr
 , testLwLogRegr
 , testMhLogRegr
 , testSimHMM_WR
 , testLwHMM
 , testMhHMM
 , testSmcHMM
 , testRmpfHMM
 , testPmhHMM
 , testSimSIR
 , testMhSIR
 , testSimLDA
 , testMhLDA
 , testSmcLDA
 , testRmpfLDA
 , testPmhLDA
 , testMhRadon
 , testMhPredRadon
 ]

