{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Test.Inference where

import Examples
import VIExamples
import Test.InferenceExpected
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

testRmsmcLinRegr :: Test
testRmsmcLinRegr = TestCase $ do
  output <- sampleIOFixed (rmsmcLinRegr 20 200 20)
  assertEqual "Testing (rmsmcLinRegr 20 200 20)"  rmsmcLinRegrExpected output

testPmmhLinRegr :: Test
testPmmhLinRegr = TestCase $ do
  output <- sampleIOFixed (pmmhLinRegr 1000 20 30)
  assertEqual "Testing (pmmhLinRegr 1000 20 30)"  pmmhLinRegrExpected output

testBbviLinRegr :: Test
testBbviLinRegr = TestCase $ do
  output <- sampleIOFixed (bbviLinRegr 200 40 8)
  assertEqual "Testing (bbviLinRegr 200 40 8)"  bbviLinRegrExpected output

testMleLinRegr :: Test
testMleLinRegr = TestCase $ do
  output <- sampleIOFixed (mleLinRegr 276 40 8)
  assertEqual "Testing (mleLinRegr 276 40 8)"  mleLinRegrExpected output

testMapLinRegr :: Test
testMapLinRegr = TestCase $ do
  output <- sampleIOFixed (mapLinRegr 276 40 8)
  assertEqual "Testing (mapLinRegr 276 40 8)"  mapLinRegrExpected output

testSimLogRegr :: Test
testSimLogRegr = TestCase $ do
  output <- sampleIOFixed (simLogRegr 50)
  assertEqual "Testing (simLogRegr 50)"  simLogRegrExpected output

testLwLogRegr :: Test
testLwLogRegr = TestCase $ do
  output <- sampleIOFixed (lwLogRegr 100 10)
  assertEqual "Testing (lwLogRegr 100 10)"  lwLogRegrExpected output

testSmc2LinRegr :: Test
testSmc2LinRegr = TestCase $ do
  output <- sampleIOFixed (smc2LinRegr 20 20 20 30)
  assertEqual "Testing (smc2LinRegr 20 20 20 30)"  smc2LinRegrExpected output

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

testRmsmcHMM :: Test
testRmsmcHMM = TestCase $ do
  output <- sampleIOFixed (rmsmcHMM 50 50 30)
  assertEqual "Testing (rmsmcHMM 50 50 30)"  rmsmcHMMExpected output

testPmmhHMM :: Test
testPmmhHMM = TestCase $ do
  output <- sampleIOFixed (pmmhHMM 1000 5 20)
  assertEqual "Testing (pmmhHMM 1000 5 20)"  pmmhHMMExpected output

testSmc2HMM :: Test
testSmc2HMM = TestCase $ do
  output <- sampleIOFixed (smc2HMM 20 20 20 20)
  assertEqual "Testing (smc2HMM 20 20 20 20)" smc2HMMExpected output

testBbviHMM :: Test
testBbviHMM = TestCase $ do
  output <- sampleIOFixed (bbviHMM 1000 50 20)
  assertEqual "Testing (bbviHMM 1000 50 20)"  bbviHMMExpected output

testMleHMM :: Test
testMleHMM = TestCase $ do
  output <- sampleIOFixed (mleHMM 1000 50 20)
  assertEqual "Testing (mleHMM 1000 50 20)"  mleHMMExpected output

testMapHMM :: Test
testMapHMM = TestCase $ do
  output <- sampleIOFixed (mapHMM 1000 50 20)
  assertEqual "Testing (mapHMM 1000 50 20)"  mapHMMExpected output

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

testRmsmcLDA :: Test
testRmsmcLDA = TestCase $ do
  output <- sampleIOFixed (rmsmcLDA 10 30 100)
  let rmsmcLDAExpected' = mapT2 (map (map (roundPrecision 8))) rmsmcLDAExpected
      output'           = mapT2 (map (map (roundPrecision 8))) output
  assertEqual "Testing (rmsmcLDA 10 30 100)"  rmsmcLDAExpected' output'

testPmmhLDA :: Test
testPmmhLDA = TestCase $ do
  output <- sampleIOFixed (pmmhLDA 100 20 100)
  let pmmhLDAExpected' = mapT2 (map (map (roundPrecision 8))) pmmhLDAExpected
      output'           = mapT2 (map (map (roundPrecision 8))) output
  assertEqual "Testing (pmmhLDA 100 20 100)" pmmhLDAExpected' output'

testBbviLDA :: Test
testBbviLDA = TestCase $ do
  output <- sampleIOFixed (bbviLDA 200 20 50)
  let bbviLDAExpected' = mapT3 (map (roundPrecision 8)) bbviLDAExpected
      output'          = mapT3 (map (roundPrecision 8)) output
  assertEqual "Testing (bbviLDA 200 20 50)"  bbviLDAExpected' output'

testMleLDA :: Test
testMleLDA = TestCase $ do
  output <- sampleIOFixed (mleLDA 200 20 50)
  let mleLDAExpected' = mapT3 (map (roundPrecision 8)) mleLDAExpected
      output'          = mapT3 (map (roundPrecision 8)) output
  assertEqual "Testing (mleLDA 200 20 50)"  mleLDAExpected' output'

testMapLDA :: Test
testMapLDA = TestCase $ do
  output <- sampleIOFixed (mapLDA 200 20 50)
  let mapLDAExpected' = mapT3 (map (roundPrecision 8)) mapLDAExpected
      output'          = mapT3 (map (roundPrecision 8)) output
  assertEqual "Testing (mapLDA 200 20 50)"  mapLDAExpected' output'

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
 , testRmsmcLinRegr
 , testPmmhLinRegr
 , testSmc2LinRegr
 , testBbviLinRegr
 , testMleLinRegr
 , testMapLinRegr
 , testSimLogRegr
 , testLwLogRegr
 , testMhLogRegr
 , testSimHMM_WR
 , testLwHMM
 , testMhHMM
 , testSmcHMM
 , testRmsmcHMM
 , testPmmhHMM
 , testSmc2HMM
 , testBbviHMM
 , testMleHMM
 , testMapHMM
 , testSimSIR
 , testMhSIR
 , testSimLDA
 , testMhLDA
 , testSmcLDA
 , testRmsmcLDA
 , testPmmhLDA
 , testBbviLDA
 , testMleLDA
 , testMapLDA
 , testMhRadon
 , testMhPredRadon
 ]

