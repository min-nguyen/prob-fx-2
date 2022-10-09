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

{- AD experiments
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
-}

{- Normal -}
-- Log pdf using Double
normalLogPdfRaw :: [Double] -> Double
normalLogPdfRaw [mean, variance, x]
  | variance <= 0 = error "normalLogPdfRaw: variance <= 0"
  | otherwise     = (-xm * xm / (2 * variance)) - log (m_sqrt_2_pi * sqrt variance)
  where xm = x - mean

-- Gradient of log pdf directly
normalGradLogPdfRaw :: [Double] -> [Double]
normalGradLogPdfRaw [mean, variance, x]
  | variance <= 0 = error "normalGradLogPdfRaw: variance <= 0"
  | otherwise     = [dm, dv, dx]
  where xm = x - mean
        dm = xm/variance
        dv = -1/(2 * variance) + ((xm/sqrt variance)**2)/(2*variance)
        dx = -dm

{- HalfNormal -}
-- Log pdf using Double
halfNormalLogPdfRaw :: [Double] -> Double
halfNormalLogPdfRaw [variance, x]
  = log 2 + normalLogPdfRaw [0, variance, x]

-- Gradient of log pdf directly
halfNormalGradLogPdfRaw :: [Double] -> [Double]
halfNormalGradLogPdfRaw [variance, x]
  | x < 0         = error "halfNormalGradLogPdfRaw: No gradient at x < 0"
  | variance <= 0 = error "halfNormalGradLogPdfRaw: variance <= 0"
  | otherwise     = tail $ normalGradLogPdfRaw [0, variance, x]

{- Cauchy -}
-- Log pdf using Double
cauchyLogPdfRaw :: [Double] -> Double
cauchyLogPdfRaw [loc, scale, x]
  | scale <= 0 = error "cauchyLogPdfRaw: scale <= 0"
  | otherwise     = -(log pi) + log scale - log (xloc**2 + scale**2)
  where xloc = x - loc

-- Gradient of log pdf directly
cauchyGradLogPdfRaw :: [Double] -> [Double]
cauchyGradLogPdfRaw [loc, scale, x]
  | scale <= 0 = error "cauchyGradLogPdfRaw: scale <= 0"
  | otherwise     = [dl, ds, dx]
  where xloc      = x - loc
        xlocSqrd  = xloc**2
        scaleSqrd = scale**2
        dl = (2 * xloc)/(xlocSqrd + scaleSqrd)
        ds = 1/scale - (2 * scale)/(xlocSqrd + scaleSqrd)
        dx = -dl

{- HalfCauchy -}
-- Log pdf using Double
halfCauchyLogPdfRaw :: [Double] -> Double
halfCauchyLogPdfRaw [scale, x]
  = log 2 + cauchyLogPdfRaw [0, scale, x]

-- Gradient of log pdf directly
halfCauchyGradLogPdfRaw :: [Double] -> [Double]
halfCauchyGradLogPdfRaw [scale, x]
  | scale <= 0 = error "cauchyGradLogPdfRaw: scale <= 0"
  | otherwise     = tail $ cauchyGradLogPdfRaw [0, scale, x]

{- Gamma -}
-- Log pdf using just Doubles
gammaLogPdfRaw :: [Double] -> Double
gammaLogPdfRaw [k, t, x]
  | x <= 0           = m_neg_inf
  | k <= 0 || t <= 0 = error "gammaLogPdfRaw: k <= 0 || t <= 0"
  | otherwise = (k - 1) * log x - (x/t) - logGamma k - (k * log t)

-- Gradient of log pdf directly
gammaGradLogPdfRaw :: [Double] -> [Double]
gammaGradLogPdfRaw [k, t, x]
  | k <= 0 || t <= 0 = error "gammaGradLogPdfRaw: k <= 0 || t <= 0"
  | x <= 0           = error "gammaGradLogPdfRaw: x <= 0 "
  | otherwise = [dk, dt, dx]
  where dk = log x - digamma k - log t
        dt = x/(t**2) - k/t
        dx = (k - 1)/x - 1/t

{- Beta -}
-- Log pdf using just Doubles
betaLogPdfRaw :: [Double] -> Double
betaLogPdfRaw [a, b, x]
    | a <= 0 || b <= 0 = error "betaLogPdfRaw:  a <= 0 || b <= 0 "
    | x <= 0 || x >= 1 = m_neg_inf
    | otherwise = (a-1)*log x + (b-1)*log1p (-x) - logBeta a b

-- Gradient of log pdf directly
betaGradLogPdfRaw :: [Double] -> [Double]
betaGradLogPdfRaw [a, b, x]
  | a <= 0 || b <= 0 = error "betaGradLogPdfRaw: a <= 0 || b <= 0"
  | x <= 0 || x >= 1 = error "betaGradLogPdfRaw: x <= 0 || x >= 0"
  | otherwise = [da, db, dx]
  where digamma_ab = digamma (a + b)
        da = log x       - digamma a + digamma_ab
        db = log (1 - x) - digamma b + digamma_ab
        dx = (a - 1)/x + (b - 1)/(1 - x)

{- Dirichlet -}
-- Log pdf using just Doubles
dirichletLogPdfRaw :: [[Double]] -> Double
dirichletLogPdfRaw [as, xs]
  | length xs /= length as     = m_neg_inf   -- | dimensions should match
  | abs (sum as - 1.0) > 1e-14 = m_neg_inf   -- | weights should sum to 1
  | abs (sum xs - 1.0) > 1e-14 = m_neg_inf   -- | data should sum to 1
  | any (<= 0) as              = m_neg_inf   -- | weights should be non-negative
  | any (<= 0) xs              = m_neg_inf   -- | data should be non-negative
  | otherwise = c + sum (zipWith (\a x -> (a - 1) * log x) as xs)
  where c = - sum (map logGamma as) + logGamma (sum as)

-- Gradient of log pdf directly
dirichletGradLogPdfRaw :: [[Double]] -> [[Double]]
dirichletGradLogPdfRaw [as, xs]
  | length xs /= length as     = error "dirichletGradLogPdfRaw: length xs /= length as"
  | abs (sum as - 1.0) > 1e-14 = error "dirichletGradLogPdfRaw: weights should sum to 1"
  | abs (sum xs - 1.0) > 1e-14 = error "dirichletGradLogPdfRaw: data should sum to 1"
  | any (<= 0) as              = error "dirichletGradLogPdfRaw: weights should be non-negative"
  | any (<= 0) xs              = error "dirichletGradLogPdfRaw: data should be non-negative"
  | otherwise = [zipWith derivA as xs, zipWith derivX as xs]
  where derivA a x  = -(digamma a) - m_eulerMascheroni + log x
        derivX a x = (a - 1) / x