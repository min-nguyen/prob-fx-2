
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE TypeApplications #-}
module Test.Numerics where

import qualified Data.Vector.Unboxed as UV
import Numeric.Log ( Log(ln) )
import qualified Vec
import Data.Maybe
import GHC.TypeNats
import LogP
import Test.QuickCheck
import Test.HUnit
import Statistics.Distribution ( ContDistr(density, logDensity), DiscreteDistr(probability), logProbability )
import Statistics.Distribution.Beta ( betaDistr )
import Statistics.Distribution.Binomial ( binomial )
import Statistics.Distribution.CauchyLorentz ( cauchyDistribution )
import Statistics.Distribution.Dirichlet ( dirichletDensity, dirichletDistribution )
import Statistics.Distribution.DiscreteUniform ( discreteUniformAB )
import Statistics.Distribution.Gamma ( gammaDistr )
import Statistics.Distribution.Normal ( normalDistr )
import Statistics.Distribution.Poisson ( poisson )
import Statistics.Distribution.Uniform ( uniformDistr )
import PrimDist
import Debug.Trace
import Control.Monad (replicateM)
import Model (bernoulli)

c_error_margin :: Double
c_error_margin = 0.0000001

{- Continuous distributions
-}

testNormalLogPdf :: Gen Bool
testNormalLogPdf = do
  mu   <- choose (-10, 10)
  std  <- choose (0.1, 5.0)
  x    <- choose (-10, 10)
  let logp  = logDensity (normalDistr mu std) x
      logp' = logProb (mkNormal mu std) x
  pure (abs (logp - logp') < c_error_margin)

testHalfNormalLogPdf :: Gen Bool
testHalfNormalLogPdf = do
  std  <- choose (0.1, 5.0)
  x    <- choose (0.001, 10)
  let logp  = log 2 + logDensity (normalDistr 0 std) x
      logp' = logProb (mkHalfNormal std) x

  pure (abs (logp - logp') < c_error_margin)

testCauchyLogPdf :: Gen Bool
testCauchyLogPdf = do
  loc    <- choose (-10, 10)
  scale  <- choose (0.1, 5.0)
  x      <- choose (-10, 10)
  let logp  = logDensity (cauchyDistribution loc scale) x
      logp' = logProb (mkCauchy loc scale) x

  pure (abs (logp - logp') < c_error_margin)

testHalfCauchyLogPdf :: Gen Bool
testHalfCauchyLogPdf = do
  scale  <- choose (0.1, 5.0)
  x      <- choose (0.001, 10)
  let logp  = log 2 + logDensity (cauchyDistribution 0 scale) x
      logp' = logProb (mkHalfCauchy scale) x

  pure (abs (logp - logp') < c_error_margin)

testGammaLogPdf :: Gen Bool
testGammaLogPdf = do
  k  <- choose (0.001, 10)
  t  <- choose (0.001, 10)
  x  <- choose (0.001, 10)
  let logp  = logDensity (gammaDistr k t) x
      logp' = logProb (mkGamma k t) x

  pure (abs (logp - logp') < c_error_margin)

testBetaLogPdf :: Gen Bool
testBetaLogPdf = do
  a  <- choose (0.001, 10)
  b  <- choose (0.001, 10)
  x  <- choose (0.001, 0.999)
  let logp  = logDensity (betaDistr a b) x
      logp' = logProb (mkBeta a b) x

  pure (abs (logp - logp') < c_error_margin)

-- testDirichletLogPdf :: Gen Bool
-- testDirichletLogPdf = do
--   size   <- chooseInt (2, 20)
--   alphas <- Vec.replicateM size (choose (0.001, 10))
--   xs     <- Vec.replicateM size (choose (0.001, 10))
--   let alphas' = Vec.map (/Prelude.sum alphas) alphas
--   let xs'     = Vec.map (/Prelude.sum xs) xs
--   case dirichletDistribution (UV.fromList (Vec.toList alphas'))
--       of Left e  -> error $ "Dirichlet error: " ++ e
--          Right d -> let logp  = ln $ dirichletDensity d (UV.fromList (Vec.toList xs'))
--                         dir   = (Dirichlet $ alphas')
--                         logp' = logProb  dir xs'
--                     in  pure (abs (logp - logp') < c_error_margin)

testUniformLogPdf :: Gen Bool
testUniformLogPdf = do
  min  <- choose (-10, 10)
  max  <- choose (-10, 10) `suchThat` (> min)
  x    <- choose (-10, 10)
  let logp  = logDensity (uniformDistr min max) x
      logp' = logProb (mkUniform min max) x

  pure (abs (logp - logp') < c_error_margin || (logp == logp'))

{- Discrete distributions
-}
testBernoulliLogPdf :: Gen Bool
testBernoulliLogPdf = do
  p  <- choose (0, 1)
  b  <- chooseInt (0, 1) >>= (\i -> pure $ [False, True] !! i)
  let logp  = logProbability (binomial 1 p) (boolToInt b)
      logp' = logProb (mkBernoulli p) b

  pure (abs (logp - logp') < c_error_margin || (logp == logp'))
  where boolToInt True  = 1
        boolToInt False = 0

testBinomialLogPdf :: Gen Bool
testBinomialLogPdf = do
  n  <- chooseInt (1, 30)
  p  <- choose    (0, 1)
  y  <- chooseInt (1, 30)
  let logp  = logProbability (binomial n p) y
      logp' = logProb (mkBinomial n p) y

  pure (abs (logp - logp') < c_error_margin || (logp == logp'))

testPoissonLogPdf :: Gen Bool
testPoissonLogPdf = do
  lambda  <- choose    (0, 100)
  y       <- chooseInt (0, 100)
  let logp  = logProbability (poisson lambda) y
      logp' = logProb (mkPoisson lambda) y

  pure (abs (logp - logp') < c_error_margin || (logp == logp'))

testUniformDLogPdf :: Gen Bool
testUniformDLogPdf = do
  min  <- chooseInt (-10, 10)
  max  <- chooseInt (-10, 10) `suchThat` (>= min)
  x    <- chooseInt (-10, 10) `suchThat` (\x -> x >= min && x  <= max)
  let logp  = logProbability (discreteUniformAB min max) x
      logp' = logProb (mkUniformD min max) x

  pure (abs (logp - logp') < c_error_margin || (logp == logp'))

{-
unnecessary tests:
- Categorical
- Discrete
- Deterministic
-}



{-
-}
hspecQuickTest :: Gen Bool -> Test
hspecQuickTest prog = TestCase $ quickCheckResult prog >>= assert . isSuccess

testLogPdfs :: Test
testLogPdfs = TestList $ map hspecQuickTest
  [testNormalLogPdf, testHalfNormalLogPdf, testCauchyLogPdf, testHalfCauchyLogPdf, testGammaLogPdf, testBetaLogPdf,
  -- testDirichletLogPdf,
  testUniformLogPdf, testBernoulliLogPdf, testBinomialLogPdf, testPoissonLogPdf, testUniformDLogPdf]