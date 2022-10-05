{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Grad where


-- import Numeric.AD
import Data.Reflection
import Numeric.AD.Mode.Reverse
import Numeric.AD.Internal.Reverse
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


-- normalDensity :: [Double] -> Double
-- normalDensity (x:y:xs) = density (normalDistr 0 1) x

-- gradNormalDensity = grad normalDensity

-- g :: (Numeric.AD.Internal.Reverse.Reverse s Double) ->  (Numeric.AD.Internal.Reverse.Reverse s Double)
g :: Reifies s Tape => Reverse s Double -> Reverse s Double
g = log

f :: Double
f = diff g 0 -- (\[x,y,z] -> x * y * z)