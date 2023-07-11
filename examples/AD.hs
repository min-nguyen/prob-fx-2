{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module AD where

{- | This file defines a simple forward-mode version of AD.
-}

import Control.Monad
import Data.Map
import Data.Number.Erf
import Comp
import Sampler
import Numeric.MathFunctions.Constants (m_sqrt_2_pi)

-- | Nagata number (aka, generalized dual number) whose second
--   component is the gradient vector represented as a sparse
--   map data structure from variable names to their partial
--   derivative. Absent entries are zero.
data Nagata v d = N { primal :: d, tangent :: Map v d }
  deriving Show

instance (Ord v, Ord d, Num d) => Num (Nagata v d) where
  fromInteger n   = N (fromInteger n) empty
  N x dx + N y dy = N (x + y) (unionWith (+) dx dy)
  N x dx - N y dy = N (x - y) (unionWith (+) dx (fmap negate dy))
  N x dx * N y dy = N (x * y) (unionWith (+) (fmap (y *) dx) (fmap (x *) dy))
  negate (N x dx) = N (negate x) (fmap negate dx)
  abs (N x dx) = N (abs x) (if x >= 0 then dx else fmap negate dx)

instance (Ord v, Ord d, Fractional d) => Fractional (Nagata v d) where
  fromRational r  = N (fromRational r) empty
  recip (N x dx)  = N (recip x) (fmap (recip (-x * x)*) dx)
  N x dx / N y dy = N (x / y) (let z = y * y in unionWith (+) (fmap ((y / z) *) dx) (fmap ((-x / z) *) dy))

instance (Ord v, Ord d, Floating d) => Floating (Nagata v d) where
   pi = N pi empty
   exp (N x dx) = N (exp x) (fmap ((exp x) *) dx)
   log (N x dx) = N (log x) (fmap ((recip x) *) dx)
   sqrt (N x dx) = N (sqrt x) (fmap ((recip (2 * sqrt x)) *) dx)
   sin (N x dx) = N (sin x) (fmap ((cos x) *) dx)
   cos (N x dx) = N (cos x) (fmap ((negate $ sin x) *) dx)
   tan (N x dx) = N (tan x) (fmap ((recip $ cos x ** 2) *) dx)

-- Standard Normal PDF
normpdf :: Floating d => d -> d
normpdf x = exp (negate (x * x) / 2) / (sqrt (2 * pi))

-- Probit function (inv cdf of normal)
instance (Ord v, Ord d, Floating d, InvErf d) => InvErf (Nagata v d) where
  invnormcdf (N x dx) = N (invnormcdf x) (fmap (/ (normpdf (invnormcdf x))) dx)

---------
data Normal d = Normal d d

data Sample d a where
  Sample  :: Normal d -> Sample d d

data Observe d a where
  Observe :: Normal d -> d -> Observe d d

type DiffModel d a = Comp '[Observe d] a

gradAsc :: Int -> (Nagata String Double -> DiffModel (Nagata String Double) a) -> Nagata String Double -> Nagata String Double
gradAsc steps model arg = go arg 0 where
  go p@(N x dx) i
      | i > steps = p
      | otherwise = let N x' dx' = runPure $ likelihood (model p)
                    in  go (N (x + 0.001 * (dx' ! "m")) dx) (i + 1)

example :: Nagata String Double
example = gradAsc 100 linRegr m_0 where
  linRegr m = forM xys (\(x, y) -> call $ Observe (Normal (m * x) 1) y)
  xys       = [(1,1.2),(2,1.8),(3,3.1),(4,4.2),(5, 5.0)]
  m_0       = N 0.5 (singleton "m" 1)

likelihood :: LogProb d => Handler (Observe d) es a d
likelihood  = handleWith 0 (\lρ x -> Val lρ) hop where
  hop :: LogProb d => d -> Observe d x -> (d -> x -> Comp es d) -> Comp es d
  hop lρ (Observe d y) k = k (lρ + logProb d y) y

defaultSample ::  Member Sampler es => Handler (Sample Double) es a a
defaultSample = handleWith () (const Val) (const hop) where
  hop :: Member Sampler es => Sample Double x -> (() -> x -> Comp es b) -> Comp es b
  hop (Sample (Normal mu s :: Normal Double ) ) k = do x <- call $ normal mu s
                                                       k () x

class (Num d) => LogProb d where
  logProb :: Normal d -> d -> d

instance LogProb Double where
  logProb (Normal μ σ) x = -(xμ * xμ / (2 * (σ ** 2))) - log m_sqrt_2_pi - log σ
    where xμ = x - μ

instance LogProb (Nagata String Double) where
  logProb (Normal (N μ dμ) (N σ dσ)) (N x dx)
    = N (logProb (Normal μ σ) x)
        (unionWith (+) (fmap (dpμ *) dμ) (fmap (dpσ *) dσ))
    where xμ = x - μ
          dpμ = xμ/(σ ** 2)
          dpσ = -1/σ + (xμ**2)/(σ ** 3)
