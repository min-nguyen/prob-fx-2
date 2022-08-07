module Main (main) where

import Examples
import Expected
import Test.HUnit
import Sampler
import System.Exit

testSimLinRegrOnce :: Test
testSimLinRegrOnce = TestCase $ do
  output <- sampleIOFixed (simLinRegrOnce 50)
  assertEqual "Testing (simLinRegrOnce 50)"  simLinRegrOnceExpected output

testLwLinRegrOnce :: Test
testLwLinRegrOnce = TestCase $ do
  output <- sampleIOFixed (lwLinRegrOnce 50 30)
  assertEqual "Testing (lwLinRegrOnce 50 30)"  lwLinRegrOnceExpected output

testMhLinRegrOnce :: Test
testMhLinRegrOnce = TestCase $ do
  output <- sampleIOFixed (mhLinRegrOnce 50 10)
  assertEqual "Testing (mhLinRegrOnce 50 10)"  mhLinRegrOnceExpected output

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

testSimLogRegrOnce :: Test
testSimLogRegrOnce = TestCase $ do
  output <- sampleIOFixed (simLogRegrOnce 50)
  assertEqual "Testing (simLogRegrOnce 50)"  simLogRegrOnceExpected output

testLwLogRegrOnce :: Test
testLwLogRegrOnce = TestCase $ do
  output <- sampleIOFixed (lwLogRegrOnce 10 10)
  assertEqual "Testing (lwLogRegrOnce 10 10)" lwLogRegrOnceExpected  output

testMhLogRegrOnce :: Test
testMhLogRegrOnce = TestCase $ do
  output <- sampleIOFixed (mhLogRegrOnce 50 10)
  assertEqual "Testing (mhLogRegrOnce 50 10)" mhLogRegrOnceExpected output

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
  output <- sampleIOFixed (mhLogRegr 1000 10)
  assertEqual "Testing (mhLogRegr 1000 10)"  mhLogRegrExpected output

testSimHMMw :: Test
testSimHMMw = TestCase $ do
  output <- sampleIOFixed (simHMMw 20)
  assertEqual "Testing (simHMMw 20)"  simHMMwExpected output

testLwHMMw :: Test
testLwHMMw = TestCase $ do
  output <- sampleIOFixed (lwHMMw 200 20)
  assertEqual "Testing (lwHMMw 200 20)" lwHMMwExpected output

testMhHMMw :: Test
testMhHMMw = TestCase $ do
  output <- sampleIOFixed (mhHMMw 5000 20)
  assertEqual "Testing (mhHMMw 5000 20)"  mhHMMwExpected output

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

testMhPredLDA :: Test
testMhPredLDA = TestCase $ do
  output <- sampleIOFixed (mhPredLDA 500 100)
  assertEqual "Testing (mhPredLDA 500 100)"  mhPredLDAExpected output

testSimRadon :: Test
testSimRadon = TestCase $ do
  output <- sampleIOFixed simRadon
  assertEqual "Testing (simRadon)"  simRadonExpected output

testMhRadon :: Test
testMhRadon = TestCase $ do
  output <- sampleIOFixed (mhRadon 1000)
  assertEqual "Testing (mhRadon 1000)"  mhRadonExpected output

testMhPredRadon :: Test
testMhPredRadon = TestCase $ do
  output <- sampleIOFixed (mhPredRadon 1500)
  assertEqual "Testing (mhPredRadon 1500)"  mhPredRadonExpected output

testMhSchool :: Test
testMhSchool = TestCase $ do
  output <- sampleIOFixed (mhSchool 1000)
  assertEqual "Testing (mhSchool 1000)"  mhSchoolExpected output

testSimGMM :: Test
testSimGMM = TestCase $ do
  output <- sampleIOFixed (simGMM 20)
  assertEqual "Testing (simGMM 20)"  simGMMExpected output

testMhGMM :: Test
testMhGMM = TestCase $ do
  output <- sampleIOFixed (mhGMM 4000 20)
  assertEqual "Testing (mhGMM 4000 20)"  mhGMMExpected output

tests :: Test
tests = TestList
 [
--    testSimLinRegrOnce
--  , testLwLinRegrOnce
--  , testMhLinRegrOnce
--  , testSimLinRegr
--  , testLwLinRegr
--  , testMhLinRegr
--  , testSimLogRegrOnce
--  , testLwLogRegrOnce
--  , testMhLogRegrOnce
--  , testSimLogRegr
--  , testLwLogRegr
--  , testMhLogRegr
--  , testSimHMMw
   testLwHMMw
 , testMhHMMw
--  , testSimSIR
--  , testMhSIR
--  , testSimLDA
--  , testMhPredLDA
--  , testSimRadon
--  , testMhRadon
--  , testMhPredRadon
--  , testMhSchool
--  , testSimGMM
--  , testMhGMM
 ]


main :: IO ()
main = do
  Counts cases tried errors failures <- runTestTT tests
  if errors + failures == 0
    then
      exitSuccess
    else do
      exitWith (ExitFailure 1)