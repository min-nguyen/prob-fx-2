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
-- Log pdf using Double
normalLogPdfRaw :: [Double] -> Double
normalLogPdfRaw [mean, variance, x] = (-xm * xm / (2 * variance)) - log (m_sqrt_2_pi * sqrt variance)
  where xm = x - mean

-- Gradient of log pdf directly
normalGradLogPdfRaw :: [Double] -> [Double]
normalGradLogPdfRaw [mean, variance, x] = [dm, dv, dx]
  where xm = x - mean
        dm = xm/variance
        dv = -1/(2 * variance) + ((xm/sqrt variance)**2)/(2*variance)
        dx = -dm

-- Log pdf explicitly using Reverse s Double
normalLogPdfRev :: Reifies s Tape => [Reverse s Double] -> Reverse s Double
normalLogPdfRev [mean, variance, x] = (-xm * xm / (2 * variance)) - log (auto m_sqrt_2_pi * sqrt variance)
  where xm = x - mean

-- Log pdf where "a ~ Reverse s Double" or "a ~ Double"
normalLogPdf :: (Floating a, Mode a, Scalar a ~ Double) => [a] -> a
normalLogPdf [mean, variance, x] = (-xm * xm / (2 * variance)) - log (auto m_sqrt_2_pi * sqrt variance)
  where xm = x - mean

normalLogPdf_example :: Double
normalLogPdf_example = normalLogPdf [0, 1, 0]

normalGradLogPdf_example :: [Double]
normalGradLogPdf_example = grad normalLogPdf [0, 1, 0]

{- Half Normal -}
-- Log pdf using Double
halfNormalLogPdfRaw :: [Double] -> Double
halfNormalLogPdfRaw [variance, x] = (-x * x / (2 * variance)) - log (m_sqrt_2_pi * sqrt variance)

-- Gradient of log pdf directly
halfNormalGradLogPdfRaw :: [Double] -> [Double]
halfNormalGradLogPdfRaw [variance, x]
  | x < 0     = error "No gradient at x < 0"
  | otherwise = [dv, dx]
  where dv = -1/(2 * variance) + ((x/sqrt variance)**2)/(2*variance)
        dx = -x/variance

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
-- Log pdf using just Doubles
betaLogPdfRaw :: [Double] -> Double
betaLogPdfRaw [a, b, x]
    | a <= 0 || b <= 0 = m_NaN
    | x <= 0 = m_neg_inf
    | x >= 1 = m_neg_inf
    | otherwise = (a-1)*log x + (b-1)*log1p (-x) - logBeta a b

-- Gradient of log pdf directly
betaGradLogPdfRaw :: [Double] -> [Double]
betaGradLogPdfRaw [a, b, x] = [da, db, dx]
  where digamma_ab = digamma (a + b)
        da = log x       - digamma a + digamma_ab
        db = log (1 - x) - digamma b + digamma_ab
        dx = (a - 1)/x + (b - 1)/(1 - x)