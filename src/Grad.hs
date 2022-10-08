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
  incompleteBeta, invIncompleteBeta, logBeta, logGamma, digamma, log1p)

{- Normal -}
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

normalLogPdf_example :: Double
normalLogPdf_example = normalLogPdf [0, 1, 0]

-- Gradient of log pdf directly
normalGradLogPdfRaw :: [Double] -> [Double]
normalGradLogPdfRaw [mean, variance, y] = [dm, dv, dx]
  where ym = y - mean
        dm = ym/variance
        dv = -1/(2 * variance) + ((ym/sqrt variance)**2)/(2*variance)
        dx = -dm

-- Gradient of log pdf using AD
normalGradLogPdf :: [Double] -> [Double]
normalGradLogPdf = grad normalLogPdf

normalGradLogPdf_example :: [Double]
normalGradLogPdf_example = grad normalLogPdf [0, 1, 0]

{- Gamma -}

-- Log pdf using just Doubles
gammaLogPdfRaw :: [Double] -> Double
gammaLogPdfRaw [k, t, x] = (k - 1) * log x - (x/t) - logGamma k - (k * log t)

-- Gradient of log pdf directly
gammaGradLogPdfRaw :: [Double] -> [Double]
gammaGradLogPdfRaw [k, t, x] = [dk, dt, dx]
  where dk = log x - digamma k - log t
        dt = x/(t**2) - k/t
        dx = (k - 1)/x - 1/t

{- Beta -}

betaLogPdfRaw :: [Double] -> Double
betaLogPdfRaw [a, b, y]
    | a <= 0 || b <= 0 = m_NaN
    | y <= 0 = m_neg_inf
    | y >= 1 = m_neg_inf
    | otherwise = (a-1)*log y + (b-1)*log1p (-y) - logBeta a a
