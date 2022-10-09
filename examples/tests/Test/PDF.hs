module Test.PDF where

import Test.QuickCheck
import Statistics.Distribution ( ContDistr(density), DiscreteDistr(probability) )
import Statistics.Distribution.Beta ( betaDistr )
import Statistics.Distribution.Binomial ( binomial )
import Statistics.Distribution.CauchyLorentz ( cauchyDistribution )
import Statistics.Distribution.Dirichlet ( dirichletDensity, dirichletDistribution )
import Statistics.Distribution.DiscreteUniform ( discreteUniformAB )
import Statistics.Distribution.Gamma ( gammaDistr )
import Statistics.Distribution.Normal ( normalDistr )
import Statistics.Distribution.Poisson ( poisson )
import Statistics.Distribution.Uniform ( uniformDistr )
import Grad



genBounded :: Double -> Double -> Gen Double
genBounded min max = arbitrarySizedFractional `suchThat` (\x -> x > min && x < max)

testNormalLogPdf :: Double -> Positive Double -> Double -> Bool
testNormalLogPdf mu (Positive std) x =
  abs (log (density (normalDistr mu std') x) - normalLogPdfRaw [mu, std', x]) < 0.00001
  where std' = abs std

testNormalLogPdf' :: IO ()
testNormalLogPdf' = quickCheck $ forAll (genBounded 1 5) testNormalLogPdf