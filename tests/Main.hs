module Main (main) where

import System.Exit


import Tests.Examples
import Tests.Expected
import Test.HUnit
import Sampler
import System.Environment (getArgs)
import Test.HUnit (Test(TestCase))
import Tests.Expected (simLinRegrExpected)




testSimLinRegr :: Test
testSimLinRegr = TestCase $ do
  output <- sampleIOFixed (simLinRegr 100)
  assertEqual "Testing (simLinRegr 100)" output simLinRegrExpected

testLwLinRegr :: Test
testLwLinRegr = TestCase $ do
  output <- sampleIOFixed (lwLinRegr 100 50) 
  assertEqual "Testing (lwLinRegr 100 50)" output lwLinRegrExpected

testMhLinRegr :: Test
testMhLinRegr = TestCase $ do
  output <- sampleIOFixed (mhLinRegr 100 100) 
  assertEqual "Testing (mhLinRegr 100 100)" output mhLinRegrExpected

testSimLinRegrs :: Test
testSimLinRegrs = TestCase $ do
  output <- sampleIOFixed (simLinRegrs 100)
  assertEqual "Testing (simLinRegrs 100)" output simLinRegrsExpected

testLwLinRegrs :: Test
testLwLinRegrs = TestCase $ do
  output <- sampleIOFixed (lwLinRegrs 1000 10) 
  assertEqual "Testing (lwLinRegrs 1000 10)" output lwLinRegrsExpected

testMhLinRegrs :: Test
testMhLinRegrs = TestCase $ do
  output <- sampleIOFixed (mhLinRegrs 10000 50) 
  assertEqual "Testing (mhLinRegrs 10000 50)" output mhLinRegrsExpected

testSimLogRegr :: Test
testSimLogRegr = TestCase $ do
  output <- sampleIOFixed (simLogRegr 50)
  assertEqual "Testing (simLogRegr 50)" output simLogRegrExpected

testLwLogRegr :: Test
testLwLogRegr = TestCase $ do
  output <- sampleIOFixed (lwLogRegr 10 10) 
  assertEqual "Testing (lwLogRegr 10 10)" output lwLogRegrExpected

testMhLogRegr :: Test
testMhLogRegr = TestCase $ do
  output <- sampleIOFixed (mhLogRegr 100 10) 
  assertEqual "Testing (mhLogRegr 100 10)" output mhLogRegrExpected

testSimLogRegrs :: Test
testSimLogRegrs = TestCase $ do
  output <- sampleIOFixed (simLogRegrs 50)
  assertEqual "Testing (simLogRegrs 50)" output simLogRegrsExpected

testLwLogRegrs :: Test
testLwLogRegrs = TestCase $ do
  output <- sampleIOFixed (lwLogRegrs 100 10) 
  assertEqual "Testing (lwLogRegrs 100 10)" output lwLogRegrsExpected

testMhLogRegrs :: Test
testMhLogRegrs = TestCase $ do
  output <- sampleIOFixed (mhLogRegrs 1000 10) 
  assertEqual "Testing (mhLogRegrs 1000 10)" output mhLogRegrsExpected

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
tests = TestList [testSimLinRegr, testLwLinRegr, testMhLinRegr, testSimLinRegrs, testLwLinRegrs, testMhLinRegrs, testSimLogRegr,testLwLogRegr, testMhLogRegr, testSimLogRegrs, testLwLogRegrs, testMhLogRegrs, testSimHMM, testMhHMM, testSimSIR, testMhSIR, testSimLDA, testMhLDA, testSimRadon, testMhRadon, testMhSchool]

main :: IO ()
main = do
  runTestTT tests
  pure ()