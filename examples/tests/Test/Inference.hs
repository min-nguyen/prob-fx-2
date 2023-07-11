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

testSsmhLinRegr :: Test
testSsmhLinRegr = TestCase $ do
  output <- sampleIOFixed (ssmhLinRegr 10000 50)
  assertEqual "Testing (ssmhLinRegr 10000 50)"  ssmhLinRegrExpected output

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

testSsmhLogRegr :: Test
testSsmhLogRegr = TestCase $ do
  output <- sampleIOFixed (ssmhLogRegr 500 10)
  assertEqual "Testing (ssmhLogRegr 500 10)"  ssmhLogRegrExpected output

testSimHMM_WR :: Test
testSimHMM_WR = TestCase $ do
  output <- sampleIOFixed (simHMM_WR 20)
  assertEqual "Testing (simHMM_WR 20)"  simHMM_WRExpected output

testLwHMM :: Test
testLwHMM = TestCase $ do
  output <- sampleIOFixed (lwHMM 200 20)
  assertEqual "Testing (lwHMM 200 20)" lwHMMExpected output

testSsmhHMM :: Test
testSsmhHMM = TestCase $ do
  output <- sampleIOFixed (ssmhHMM 1000 20)
  assertEqual "Testing (ssmhHMM 1000 20)"  ssmhHMMExpected output

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

testSsmhSIR :: Test
testSsmhSIR = TestCase $ do
  output <- sampleIOFixed (ssmhSIR 1000 100)
  assertEqual "Testing (ssmhSIR 1000 100)"  ssmhSIRExpected output

testSimLDA :: Test
testSimLDA = TestCase $ do
  output <- sampleIOFixed (simLDA 100)
  assertEqual "Testing (simLDA 100)"  simLDAExpected output

testSsmhLDA :: Test
testSsmhLDA = TestCase $ do
  output <- sampleIOFixed (ssmhLDA 500 100)
  let ssmhLDAExpected' = mapT2 (map (map (roundPrecision 8))) ssmhLDAExpected
      output'        = mapT2 (map (map (roundPrecision 8))) output
  assertEqual "Testing (ssmhLDA 500 100)"  ssmhLDAExpected' output'

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

testSsmhRadon :: Test
testSsmhRadon = TestCase $ do
  output <- sampleIOFixed (ssmhRadon 1000)
  assertEqual "Testing (ssmhRadon 1000)"  ssmhRadonExpected output

testSsmhPredRadon :: Test
testSsmhPredRadon = TestCase $ do
  output <- sampleIOFixed (ssmhPredRadon 1500)
  assertEqual "Testing (ssmhPredRadon 1500)"  ssmhPredRadonExpected output

testInference :: Test
testInference = TestList
 [
   testSimLinRegr
 , testLwLinRegr
 , testSsmhLinRegr
 , testSmcLinRegr
 , testRmpfLinRegr
 , testPmhLinRegr
 , testSimLogRegr
 , testLwLogRegr
 , testSsmhLogRegr
 , testSimHMM_WR
 , testLwHMM
 , testSsmhHMM
 , testSmcHMM
 , testRmpfHMM
 , testPmhHMM
 , testSimSIR
 , testSsmhSIR
 , testSimLDA
 , testSsmhLDA
 , testSmcLDA
 , testRmpfLDA
 , testPmhLDA
 , testSsmhRadon
 , testSsmhPredRadon
 ]

