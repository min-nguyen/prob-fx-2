{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Grad where


-- import Numeric.AD
import Data.Maybe
import Data.Reflection
import Numeric.AD.Mode
import Numeric.AD.Mode.Reverse
import Numeric.AD.Internal.Reverse
import Numeric.MathFunctions.Constants
import Numeric.SpecFunctions (
  incompleteBeta, invIncompleteBeta, logBeta, digamma, log1p)

{- Normal -}
-- Log pdf using just Doubles
normalLogPdfDouble :: [Double] -> Double
normalLogPdfDouble [mean, variance, y] = (-ym * ym / (2 * variance)) - (log (m_sqrt_2_pi * sqrt variance))
  where ym = y - mean

-- Log pdf explicitly using Reverse s Double
normalLogPdfRev :: Reifies s Tape => [Reverse s Double] -> Reverse s Double
normalLogPdfRev [mean, variance, y] = (-ym * ym / (2 * variance)) - (log (auto m_sqrt_2_pi * sqrt variance))
  where ym = y - mean

-- Log pdf where "a ~ Reverse s Double" or "a ~ Double"
normalLogPdf :: (Floating a, Mode a, Scalar a ~ Double) => [a] -> a
normalLogPdf [mean, variance, y] = (-ym * ym / (2 * variance)) - (log (auto m_sqrt_2_pi * sqrt variance))
  where ym = y - mean

normalLogPdf_example :: Double
normalLogPdf_example = normalLogPdf [0, 1, 0]

-- Gradient of log pdf directly
gradNormalLogPdfRaw :: [Double] -> [Double]
gradNormalLogPdfRaw [mean, variance, y] = [ym/variance, -1/(2 * variance) + ((ym/sqrt variance)**2)/(2*variance), -ym/variance ]
  where ym = y - mean

-- Gradient of log pdf using AD
gradNormalLogPdf :: [Double] -> [Double]
gradNormalLogPdf = grad normalLogPdf

gradNormalLogPdf_example :: [Double]
gradNormalLogPdf_example = grad normalLogPdf [0, 1, 0]

{- Beta -}
betaLogPdf :: (Ord a, Floating a, Mode a, Scalar a ~ Double) => [a] -> a
betaLogPdf [a, b, y]
    | a <= 0 || b <= 0 = auto m_NaN
    | y <= 0 = auto m_neg_inf
    | y >= 1 = auto m_neg_inf
    | otherwise = (a-1)*log y + (b-1)*log1p (-y) - auto (logBeta (fromJust $ asKnownConstant a) (fromJust $ asKnownConstant a))
