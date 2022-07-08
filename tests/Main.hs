module Main (main) where

import System.Exit


import Tests.Examples
import Tests.Expected
import Test.HUnit
import Sampler
import System.Environment (getArgs)
import Test.HUnit (Test(TestCase))
import Tests.Expected 




testSimLinRegrOnce :: Test
testSimLinRegrOnce = TestCase $ do
  output <- sampleIOFixed (simLinRegrOnce 100)
  assertEqual "Testing (simLinRegrOnce 100)" output simLinRegrOnceExpected

testLwLinRegrOnce :: Test
testLwLinRegrOnce = TestCase $ do
  output <- sampleIOFixed (lwLinRegrOnce 100 50) 
  assertEqual "Testing (lwLinRegrOnce 100 50)" output lwLinRegrOnceExpected

testMhLinRegrOnce :: Test
testMhLinRegrOnce = TestCase $ do
  output <- sampleIOFixed (mhLinRegrOnce 100 100) 
  assertEqual "Testing (mhLinRegrOnce 100 100)" output mhLinRegrOnceExpected

testSimLinRegr :: Test
testSimLinRegr = TestCase $ do
  output <- sampleIOFixed (simLinRegr 100)
  assertEqual "Testing (simLinRegr 100)" output simLinRegrExpected

testLwLinRegr :: Test
testLwLinRegr = TestCase $ do
  output <- sampleIOFixed (lwLinRegr 1000 10) 
  assertEqual "Testing (lwLinRegr 1000 10)" output lwLinRegrExpected

testMhLinRegr :: Test
testMhLinRegr = TestCase $ do
  output <- sampleIOFixed (mhLinRegr 10000 50) 
  assertEqual "Testing (mhLinRegr 10000 50)" output mhLinRegrExpected

testSimLogRegrOnce :: Test
testSimLogRegrOnce = TestCase $ do
  output <- sampleIOFixed (simLogRegrOnce 50)
  assertEqual "Testing (simLogRegrOnce 50)" output simLogRegrOnceExpected

testLwLogRegrOnce :: Test
testLwLogRegrOnce = TestCase $ do
  output <- sampleIOFixed (lwLogRegrOnce 10 10) 
  assertEqual "Testing (lwLogRegrOnce 10 10)" output lwLogRegrOnceExpected

testMhLogRegrOnce :: Test
testMhLogRegrOnce = TestCase $ do
  output <- sampleIOFixed (mhLogRegrOnce 100 10) 
  assertEqual "Testing (mhLogRegrOnce 100 10)" output mhLogRegrOnceExpected

testSimLogRegr :: Test
testSimLogRegr = TestCase $ do
  output <- sampleIOFixed (simLogRegr 50)
  assertEqual "Testing (simLogRegr 50)" output simLogRegrExpected

testLwLogRegr :: Test
testLwLogRegr = TestCase $ do
  output <- sampleIOFixed (lwLogRegr 100 10) 
  assertEqual "Testing (lwLogRegr 100 10)" output lwLogRegrExpected

testMhLogRegr :: Test
testMhLogRegr = TestCase $ do
  output <- sampleIOFixed (mhLogRegr 1000 10) 
  assertEqual "Testing (mhLogRegr 1000 10)" output mhLogRegrExpected

testSimHMM :: Test
testSimHMM = TestCase $ do
  output <- sampleIOFixed (simHMMw 20)
  assertEqual "Testing (simHMM 20)" output simHMMExpected

testMhHMM :: Test
testMhHMM = TestCase $ do
  output <- sampleIOFixed (mhHMMw 5000 20)
  assertEqual "Testing (mhHMMw 5000 20)" output mhHMMExpected

testSimSIR :: Test
testSimSIR = TestCase $ do
  output <- sampleIOFixed (simSIR 100)
  assertEqual "Testing (simSIR 100)" output simSIRExpected

testMhSIR :: Test
testMhSIR = TestCase $ do
  output <- sampleIOFixed (mhSIR 10000 100)
  assertEqual "Testing (mhSIR 10000 100)" output mhSIRExpected

testSimLDA :: Test
testSimLDA = TestCase $ do
  output <- sampleIOFixed (simLDA 100)
  assertEqual "Testing (simLDA 100)" output simLDAExpected

testMhLDA :: Test
testMhLDA = TestCase $ do
  output <- sampleIOFixed (mhLDA 500 100)
  assertEqual "Testing (mhLDA 500 100)" output mhLDAExpected

testSimRadon :: Test
testSimRadon = TestCase $ do
  output <- sampleIOFixed simRadon
  assertEqual "Testing (simRadon)" output simRadonExpected

testMhRadon :: Test
testMhRadon = TestCase $ do
  output <- sampleIOFixed (mhRadon 1500)
  assertEqual "Testing (mhRadon 1500)" output mhRadonExpected

testMhSchool :: Test
testMhSchool = TestCase $ do
  output <- sampleIOFixed (mhSchool 1000)
  assertEqual "Testing (mhSchool 1000)" output mhSchoolExpected

tests :: Test
tests = TestList [testSimLinRegrOnce, testLwLinRegrOnce, testMhLinRegrOnce, testSimLinRegr, testLwLinRegr, testMhLinRegr, testSimLogRegrOnce, testLwLogRegrOnce, testMhLogRegrOnce, testSimLogRegr, testLwLogRegr, testMhLogRegr, testSimHMM, testMhHMM, testSimSIR, testMhSIR, testSimLDA, testMhLDA, testSimRadon, testMhRadon, testMhSchool
 ]

main :: IO ()
main = do
  runTestTT tests
  pure ()