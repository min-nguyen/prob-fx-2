{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Grad where


-- import Numeric.AD
import Data.Reflection
import Numeric.AD.Mode
import Numeric.AD.Mode.Reverse
import Numeric.AD.Internal.Reverse
import Numeric.MathFunctions.Constants

-- Log pdf using just Doubles
normalLogPdfRaw :: [Double] -> Double
normalLogPdfRaw [mean, variance, y] = (-ym * ym / (2 * variance)) - (log (m_sqrt_2_pi * sqrt variance))
  where ym = y - mean

-- Log pdf explicitly using Reverse s Double
normalLogPdfRev :: Reifies s Tape => [Reverse s Double] -> Reverse s Double
normalLogPdfRev [mean, variance, y] = (-ym * ym / (2 * variance)) - (log (auto m_sqrt_2_pi * sqrt variance))
  where ym = y - mean

-- Log pdf where "a ~ Reverse s Double" or "a ~ Double"
normalLogPdf :: (Floating a, Mode a, Scalar a ~ Double) => [a] -> a
normalLogPdf [mean, variance, y] = (-ym * ym / (2 * variance)) - (log (auto m_sqrt_2_pi * sqrt variance))
  where ym = y - mean


gradNormalLogPdf_example :: [Double]
gradNormalLogPdf_example = grad normalLogPdf [0, 1, 0]

r :: Double
r = normalLogPdf []