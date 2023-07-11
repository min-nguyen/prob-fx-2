{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Dual where

import Control.Monad
import Data.Map
import Comp
import Sampler
import Numeric.MathFunctions.Constants (m_sqrt_2_pi)

-- Forward mode
data Dual v d = N { primal :: d, tangent :: Map v d }
  deriving Show

instance (Ord v, Ord d, Num d) => Num (Dual v d) where
  fromInteger n   = N (fromInteger n) empty
  N x dx + N y dy = N (x + y) (unionWith (+) dx dy)
  N x dx - N y dy = N (x - y) (unionWith (+) dx (fmap negate dy))
  N x dx * N y dy = N (x * y) (unionWith (+) (fmap (y *) dx) (fmap (x *) dy))
  negate (N x dx) = N (negate x) (fmap negate dx)
  abs (N x dx) = N (abs x) (if x >= 0 then dx else fmap negate dx)

instance (Ord v, Ord d, Fractional d) => Fractional (Dual v d) where
  fromRational r  = N (fromRational r) empty
  recip (N x dx)  = N (recip x) (fmap (recip (-x * x)*) dx)
  N x dx / N y dy = N (x / y) (let z = y * y in unionWith (+) (fmap ((y / z) *) dx) (fmap ((-x / z) *) dy))

instance (Ord v, Ord d, Floating d) => Floating (Dual v d) where
   pi = N pi empty
   exp (N x dx) = N (exp x) (fmap ((exp x) *) dx)
   log (N x dx) = N (log x) (fmap ((recip x) *) dx)
   sqrt (N x dx) = N (sqrt x) (fmap ((recip (2 * sqrt x)) *) dx)
   sin (N x dx) = N (sin x) (fmap ((cos x) *) dx)
   cos (N x dx) = N (cos x) (fmap ((negate $ sin x) *) dx)
   tan (N x dx) = N (tan x) (fmap ((recip $ cos x ** 2) *) dx)
