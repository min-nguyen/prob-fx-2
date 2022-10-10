{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}


{- | A GADT encoding of (a selection of) primitive distributions
    along with their corresponding sampling and density functions.
-}

module PrimDist (
  -- * Primitive distribution
    PrimDist(..)
  , PrimVal
  , pattern PrimValDistPrf
  , pattern TypeableDistPrf
  -- * Sampling
  , sample
  , sampleInv
  -- , sampleBayes
  -- * Density
  , logProb
  , logProbRaw
  , gradLogProb) where

import Debug.Trace
import Data.Kind ( Constraint )
import Data.Map (Map)
import OpenSum (OpenSum)
import qualified Data.Vector as V
import qualified OpenSum
import Numeric.MathFunctions.Constants
    ( m_eulerMascheroni, m_neg_inf, m_sqrt_2_pi )
import Numeric.SpecFunctions (
  incompleteBeta, invIncompleteBeta, logBeta, logGamma, digamma, log1p, logChoose, logFactorial)
import Sampler
import LogP ( LogP(..) )
import Util ( boolToInt )
import Numeric.Log ( Log(..) )
import Control.Monad ((>=>), replicateM)
import Data.Typeable
import GHC.Real (infinity)
import Numeric.MathFunctions.Constants (m_neg_inf)
-- import qualified Control.Monad.Bayes.Class as MB

-- | Primitive distribution
data PrimDist a where
  Bernoulli
    :: Double           -- ^ probability of @True@
    -> PrimDist Bool
  Beta
    :: Double           -- ^ shape α
    -> Double           -- ^ shape β
    -> PrimDist Double
  Binomial
    :: Int              -- ^ number of trials
    -> Double           -- ^ probability of successful trial
    -> PrimDist Int
  Categorical
    :: [Double]         -- ^ list of @n@ probabilities
    -> PrimDist Int     -- ^ an index from @0@ to @n - 1@
  Cauchy
    :: Double           -- ^ location
    -> Double           -- ^ scale
    -> PrimDist Double
  HalfCauchy
    :: Double           -- ^ scale
    -> PrimDist Double
  Deterministic
    :: (Typeable a, Eq a)
    => a                -- ^ value of probability @1@
    -> PrimDist a
  Dirichlet
    :: [Double]         -- ^ concentrations
    -> PrimDist [Double]
  Discrete
    :: (Typeable a, Eq a, Show a)
    => [(a, Double)]    -- ^ values and associated probabilities
    -> PrimDist a
  UniformD
    :: Int              -- ^ lower-bound @a@
    -> Int              -- ^ upper-bound @b@
    -> PrimDist Int
  Gamma
    :: Double           -- ^ shape k
    -> Double           -- ^ scale θ
    -> PrimDist Double
  Normal
    :: Double           -- ^ mean
    -> Double           -- ^ standard deviation
    -> PrimDist Double
  HalfNormal
    :: Double           -- ^ standard deviation
    -> PrimDist Double
  Poisson
    :: Double           -- ^ rate λ
    -> PrimDist Int
  Uniform
    :: Double           -- ^ lower-bound @a@
    -> Double           -- ^ upper-bound @b@
    -> PrimDist Double

instance Show (PrimDist a) where
  show (Cauchy mu sigma) =
   "Cauchy(" ++ show mu ++ ", " ++ show sigma ++ ", " ++ ")"
  show (HalfCauchy sigma) =
   "HalfCauchy(" ++ show sigma ++ ", " ++ ")"
  show (Normal mu sigma) =
   "Normal(" ++ show mu ++ ", " ++ show sigma ++ ", " ++ ")"
  show (HalfNormal sigma) =
   "HalfNormal(" ++ show sigma ++ ", " ++ ")"
  show (Bernoulli p) =
   "Bernoulli(" ++ show p ++ ", " ++ ")"
  show (Binomial n p) =
   "Binomial(" ++ show n ++ ", " ++ show p ++ ", " ++  ")"
  show (Categorical ps) =
   "Categorical(" ++ show ps ++ ", " ++ ")"
  show (Beta a b) =
   "Beta(" ++ show a ++ ", " ++ show b ++ "," ++ ")"
  show (Gamma a b) =
   "Gamma(" ++ show a ++ ", " ++ show b ++ "," ++ ")"
  show (Uniform a b) =
   "Uniform(" ++ show a ++ ", " ++ show b ++ "," ++ ")"
  show (UniformD min max) =
   "UniformD(" ++ show min ++ ", " ++ show max ++ ", " ++ ")"
  show (Poisson l) =
   "Poisson(" ++ show l ++ ", " ++ ")"
  show (Discrete xs) =
   "Discrete(" ++ show xs ++ ", " ++ ")"
  show (Dirichlet xs) =
   "Dirichlet(" ++ show xs ++ ", " ++ ")"
  show (Deterministic x) =
   "Deterministic"

-- | Draw a value from a primitive distribution in the @Sampler@ monad
sample :: PrimDist a -> Sampler a
sample d = case d of
  (HalfCauchy scale ) -> sampleCauchy 0 scale >>= pure . abs
  (Cauchy loc scale ) -> sampleCauchy loc scale
  (HalfNormal σ )     -> sampleNormal 0 σ >>= pure . abs
  (Normal μ σ )       -> sampleNormal μ σ
  (Uniform min max )  -> sampleUniform min max
  (UniformD min max ) -> sampleUniformD min max
  (Gamma k θ )        -> sampleGamma k θ
  (Beta α β  )        -> sampleBeta α β
  (Binomial n p  )    -> sampleBinomial n p
  (Bernoulli p )      -> sampleBernoulli p
  (Discrete ps )      -> sampleCategorical (V.fromList $ fmap snd ps) >>= (\i -> pure $ fst $ ps !! i)
  (Categorical ps )   -> sampleDiscrete ps
  (Poisson λ )        -> samplePoisson λ
  (Dirichlet xs )     -> sampleDirichlet xs
  (Deterministic x)   -> pure x

-- | Draw a value from a primitive distribution in the @Sampler@ monad
sampleInv :: PrimDist a -> Double -> Sampler a
sampleInv d = case d of
  (HalfCauchy σ )     -> sampleCauchyInv 0 σ >=> pure . abs
  (Cauchy μ σ )       -> sampleCauchyInv μ σ
  (HalfNormal σ )     -> sampleNormalInv 0 σ >=> pure . abs
  (Normal μ σ )       -> sampleNormalInv μ σ
  (Uniform min max )  -> sampleUniformInv min max
  (UniformD min max ) -> sampleUniformDInv min max
  (Gamma k θ )        -> sampleGammaInv k θ
  (Beta α β  )        -> sampleBetaInv α β
  (Binomial n p  )    -> sampleBinomialInv n p
  (Bernoulli p )      -> sampleBernoulliInv p
  (Discrete xps )     -> sampleCategoricalInv (V.fromList $ fmap snd xps) >=> (\i -> pure $ fst $ xps !! i)
  (Categorical ps )   -> sampleDiscreteInv ps
  (Poisson λ )        -> samplePoissonInv λ
  (Dirichlet xs )     -> sampleDirichletInv xs
  (Deterministic x)   -> const (pure x)

-- | Compute the log density of a primitive distribution generating an observed value
logProb :: PrimDist a -> a -> LogP
logProb d = LogP . logProbRaw d

logProbRaw ::
  -- | distribution
     PrimDist a
  -- | observed value
  -> a
  -- | log density
  -> Double
logProbRaw (Normal μ σ) y
  = normalLogPdfRaw μ σ y
logProbRaw (HalfNormal σ) y
  = halfNormalLogPdfRaw σ y
logProbRaw (Cauchy loc scale) y
  = cauchyLogPdfRaw loc scale y
logProbRaw (HalfCauchy scale) y
  = halfCauchyLogPdfRaw scale y
logProbRaw (Gamma k θ) y
  = gammaLogPdfRaw k θ y
logProbRaw (Beta α β) y
  = betaLogPdfRaw α β y
logProbRaw (Uniform min max) y
  = uniformLogPdfRaw min max y
logProbRaw (Dirichlet as) ys
  = dirichletLogPdfRaw as ys
logProbRaw (Bernoulli p) i
  = bernoulliLogPdfRaw p i
logProbRaw (Binomial n p) y
  = binomialLogPdfRaw n p y
logProbRaw (Poisson λ) y
  = poissonLogPdfRaw λ y
logProbRaw (Deterministic x) y
  = deterministicLogPdfRaw x y
logProbRaw (Categorical ps) idx
  = categoricalLogPdfRaw ps idx
logProbRaw (Discrete ps) y
  = discreteLogPdfRaw ps y
logProbRaw (UniformD min max) y
  = uniformDLogPdfRaw min max y

-- | Given fixed parameters and observed value, compute the gradients of a distribution's log-pdf w.r.t its parameters
gradLogProb ::
     PrimDist a
  -> a
  -> PrimDist a
gradLogProb d y = error "to do"

{- Continuous distributions
-}

-- | Normal
normalLogPdfRaw :: Double -> Double -> Double -> Double
normalLogPdfRaw μ σ x
  | σ <= 0     = error "normalLogPdfRaw: σ <= 0"
  | otherwise  = -(xμ * xμ / (2 * (σ ** 2))) - log m_sqrt_2_pi - log σ
  where xμ = x - μ

normalGradLogPdfRaw :: Double -> Double -> Double -> (Double, Double, Double)
normalGradLogPdfRaw μ σ x
  | σ <= 0 = error "normalGradLogPdfRaw: σ <= 0"
  | otherwise     = (dμ, dσ, dx)
  where xμ = x - μ
        dμ = xμ/(σ ** 2)
        dσ = -1/σ + (xμ**2)/(σ ** 3)
        dx = -dμ

-- | HalfNormal
halfNormalLogPdfRaw :: Double -> Double -> Double
halfNormalLogPdfRaw σ x
  = log 2 + normalLogPdfRaw 0 σ x

halfNormalGradLogPdfRaw :: Double -> Double -> (Double, Double)
halfNormalGradLogPdfRaw σ x
  | x < 0         = error "halfNormalGradLogPdfRaw: No gradient at x < 0"
  | σ <= 0        = error "halfNormalGradLogPdfRaw: σ <= 0"
  | otherwise     = let (_, dσ, dx) = normalGradLogPdfRaw 0 σ x in (dσ, dx)

-- | Cauchy
cauchyLogPdfRaw :: Double -> Double -> Double -> Double
cauchyLogPdfRaw loc scale x
  | scale <= 0 = error "cauchyLogPdfRaw: scale <= 0"
  | otherwise     = -(log pi) + log scale - log (xloc**2 + scale**2)
  where xloc = x - loc

cauchyGradLogPdfRaw :: Double -> Double -> Double -> (Double, Double, Double)
cauchyGradLogPdfRaw loc scale x
  | scale <= 0 = error "cauchyGradLogPdfRaw: scale <= 0"
  | otherwise     = (dl, ds, dx)
  where xloc      = x - loc
        xlocSqrd  = xloc**2
        scaleSqrd = scale**2
        dl = (2 * xloc)/(xlocSqrd + scaleSqrd)
        ds = 1/scale - (2 * scale)/(xlocSqrd + scaleSqrd)
        dx = -dl

-- | HalfCauchy
halfCauchyLogPdfRaw :: Double -> Double -> Double
halfCauchyLogPdfRaw scale x
  = log 2 + cauchyLogPdfRaw 0 scale x

halfCauchyGradLogPdfRaw :: Double -> Double -> (Double, Double)
halfCauchyGradLogPdfRaw scale x
  | scale <= 0 = error "cauchyGradLogPdfRaw: scale <= 0"
  | otherwise  = let (_, ds, dx) = cauchyGradLogPdfRaw 0 scale x in (ds, dx)

-- | Gamma
gammaLogPdfRaw :: Double -> Double -> Double -> Double
gammaLogPdfRaw k t x
  | x <= 0           = m_neg_inf
  | k <= 0 || t <= 0 = error "gammaLogPdfRaw: k <= 0 || t <= 0"
  | otherwise = (k - 1) * log x - (x/t) - logGamma k - (k * log t)

gammaGradLogPdfRaw :: Double -> Double -> Double -> (Double, Double, Double)
gammaGradLogPdfRaw k t x
  | k <= 0 || t <= 0 = error "gammaGradLogPdfRaw: k <= 0 || t <= 0"
  | x <= 0           = error "gammaGradLogPdfRaw: x <= 0"
  | otherwise = (dk, dt, dx)
  where dk = log x - digamma k - log t
        dt = x/(t**2) - k/t
        dx = (k - 1)/x - 1/t

-- | Beta
betaLogPdfRaw :: Double -> Double -> Double -> Double
betaLogPdfRaw a b x
    | a <= 0 || b <= 0 = error "betaLogPdfRaw:  a <= 0 || b <= 0 "
    | x <= 0 || x >= 1 = m_neg_inf
    | otherwise = (a-1)*log x + (b-1)*log1p (-x) - logBeta a b

betaGradLogPdfRaw :: Double -> Double -> Double -> (Double, Double, Double)
betaGradLogPdfRaw a b x
  | a <= 0 || b <= 0 = error "betaGradLogPdfRaw: a <= 0 || b <= 0"
  | x <= 0 || x >= 1 = error "betaGradLogPdfRaw: x <= 0 || x >= 1"
  | otherwise = (da, db, dx)
  where digamma_ab = digamma (a + b)
        da = log x       - digamma a + digamma_ab
        db = log (1 - x) - digamma b + digamma_ab
        dx = (a - 1)/x + (b - 1)/(1 - x)

-- | Dirichlet
dirichletLogPdfRaw :: [Double] -> [Double] -> Double
dirichletLogPdfRaw as xs
  | length xs /= length as     = trace "dirichletLogPdfRaw: length xs /= length as" m_neg_inf
  | abs (sum xs - 1.0) > 1e-14 = trace "dirichletLogPdfRaw: data should sum to 1" m_neg_inf
  | any (<= 0) as              = trace "dirichletLogPdfRaw: weights should be positive" m_neg_inf
  | any (<  0) xs              = trace "dirichletLogPdfRaw: data should be non-negative" m_neg_inf
  | otherwise = c + sum (zipWith (\a x -> (a - 1) * log x) as xs)
  where c = - sum (map logGamma as) + logGamma (sum as)

dirichletGradLogPdfRaw :: [Double] -> [Double] -> ([Double], [Double])
dirichletGradLogPdfRaw as xs
  | length xs /= length as     = error "dirichletGradLogPdfRaw: length xs /= length as"
  | abs (sum xs - 1.0) > 1e-14 = error "dirichletGradLogPdfRaw: data should sum to 1"
  | any (<= 0) as              = error "dirichletGradLogPdfRaw: weights should be positive"
  | any (<  0) xs              = error "dirichletGradLogPdfRaw: data should be non-negative"
  | otherwise = (zipWith derivA as xs, zipWith derivX as xs)
  where derivA a x  = -(digamma a) - m_eulerMascheroni + log x
        derivX a x = (a - 1) / x

-- | Uniform
uniformLogPdfRaw :: Double -> Double -> Double -> Double
uniformLogPdfRaw min max x
  | max <  min         = error "uniformLogPdfRaw: max < min"
  | x < min || x > max = m_neg_inf
  | otherwise = -log(max - min)

{- Discrete distributions
-}

-- | Bernoulli
bernoulliLogPdfRaw :: Double -> Bool -> Double
bernoulliLogPdfRaw p y
  | y         = log p
  | otherwise = log (1 - p)

bernoulliGradLogPdfRaw :: Double -> Bool -> (Double, Bool)
bernoulliGradLogPdfRaw p y = (dp, False)
  where dp = 1/p - fromIntegral (boolToInt y)/(1 - p)

-- | Binomial
binomialLogPdfRaw :: Int -> Double -> Int -> Double
binomialLogPdfRaw n p y
  | y < 0 || y > n          = m_neg_inf
  | n < 0                   = error "binomialLogPdfRaw: n < 0"
  | otherwise               = logChoose n y + log p * y' + log1p (-p) * ny'
  where
    y'  = fromIntegral   y
    ny' = fromIntegral $ n - y

binomialGradLogPdfRaw :: Int -> Double -> Int -> (Int, Double, Int)
binomialGradLogPdfRaw n p y
  | y < 0 || y > n          = error "binomialGradLogPdfRaw: y < 0 || y > n"
  | n < 0                   = error "binomialGradLogPdfRaw: n < 0"
  | otherwise               = (dn, dp, dy)
  where dn = 0
        dp = fromIntegral n/p - fromIntegral (n - y)/(1 - p)
        dy = 0

-- | Poisson
poissonLogPdfRaw :: Double -> Int -> Double
poissonLogPdfRaw λ y
  | λ < 0     = error "poissonLogPdfRaw:  λ < 0 "
  | y < 0     = trace "poissonLogPdfRaw:  y < 0 " m_neg_inf
  | otherwise = log λ * fromIntegral y - logFactorial y - λ

-- | Deterministic
deterministicLogPdfRaw :: Eq a => a -> a -> Double
deterministicLogPdfRaw a b
  | a == b    = 0
  | otherwise = m_neg_inf

-- | Categorical
categoricalLogPdfRaw :: [Double] -> Int -> Double
categoricalLogPdfRaw ps idx
  | idx < 0 || idx >= length ps = trace "CategoricalLogPdf: idx < 0 || idx >= length ps" m_neg_inf
  | otherwise                   = log (ps !! idx)

-- | Discrete
discreteLogPdfRaw :: (Show a, Eq a) => [(a, Double)] -> a -> Double
discreteLogPdfRaw ps y =
  case lookup y ps of
      Nothing -> trace ("Couldn't find " ++ show y ++ " in Discrete dist") m_neg_inf
      Just p  -> log p

-- | UniformD
uniformDLogPdfRaw :: Int -> Int -> Int -> Double
uniformDLogPdfRaw min max x
  | max <  min         = error "uniformDLogPdfRaw: max < min"
  | x < min || x > max = m_neg_inf
  | otherwise          = - log (fromIntegral $ max - min + 1)


{- Dictionary proofs for constraints on primitive distributions
-}
data Dict c a where
  Dict :: c a => Dict c a

-- | An ad-hoc specification of primitive value types, for constraining the outputs of distributions
type PrimVal = '[Int, Double, [Double], Bool, String]

class    (Show x, OpenSum.Member x PrimVal) => IsPrimVal x

instance (Show x, OpenSum.Member x PrimVal) => IsPrimVal x

-- | For pattern-matching on an arbitrary @PrimDist@ with proof that it generates a primitive value
pattern PrimValDistPrf :: () => (IsPrimVal x) => PrimDist x -> PrimDist x
pattern PrimValDistPrf d <- d@(primValDistPrf -> Just Dict)

primValDistPrf :: PrimDist x -> Maybe (Dict IsPrimVal x)
primValDistPrf d = case d of
  HalfCauchy {} -> Just Dict
  Cauchy {} -> Just Dict
  Normal {} -> Just Dict
  HalfNormal  {} -> Just Dict
  Uniform  {} -> Just Dict
  UniformD {} -> Just Dict
  Gamma {} -> Just Dict
  Beta {} -> Just Dict
  Binomial {} -> Just Dict
  Bernoulli {} -> Just Dict
  Categorical {} -> Just Dict
  Poisson {} -> Just Dict
  Dirichlet {} -> Just Dict
  Deterministic {} -> Nothing
  Discrete {} -> Nothing

-- | For pattern-matching on an arbitrary @PrimDist@ with proof that it generates a primitive value
pattern TypeableDistPrf :: () => (Typeable x) => PrimDist x -> PrimDist x
pattern TypeableDistPrf d <- d@(typeableDistPrf -> Dict)

typeableDistPrf :: PrimDist x -> Dict Typeable x
typeableDistPrf d = case d of
  HalfCauchy {} ->  Dict
  Cauchy {} ->  Dict
  Normal {} ->  Dict
  HalfNormal  {} ->  Dict
  Uniform  {} ->  Dict
  UniformD {} ->  Dict
  Gamma {} ->  Dict
  Beta {} ->  Dict
  Binomial {} ->  Dict
  Bernoulli {} ->  Dict
  Categorical {} ->  Dict
  Poisson {} ->  Dict
  Dirichlet {} ->  Dict
  Discrete {} ->  Dict
  Deterministic {} ->  Dict

{- | Draw a value from a primitive distribution using the @MonadSample@ type class from Monad-Bayes
sampleBayes :: MB.MonadSample m => PrimDist a -> m a
sampleBayes d = case d of
  (Uniform a b )    -> MB.uniform a b
  (Categorical as ) -> MB.categorical (Vec.fromList as)
  (Discrete as )    -> MB.categorical (Vec.fromList (map snd as)) >>= (pure . fst . (as !!))
  (Normal mu std )  -> MB.normal mu std
  (Gamma k t )      -> MB.gamma k t
  (Beta a b )       -> MB.beta a b
  (Bernoulli p )    -> MB.bernoulli p
  (Binomial n p )   -> replicateM n (MB.bernoulli p) >>= (pure . length . filter (== True))
  (Poisson l )      -> MB.poisson l
  (Dirichlet as )   -> MB.dirichlet (Vec.fromList as) >>= pure . Vec.toList
  (Deterministic v) -> pure v
  _                 -> error ("Sampling from " ++ show d ++ " is not supported")
-}