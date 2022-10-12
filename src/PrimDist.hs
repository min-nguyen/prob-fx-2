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
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}


{- | A GADT encoding of (a selection of) primitive distributions
    along with their corresponding sampling and density functions.
-}

module PrimDist (
  -- * Primitive distribution
    PrimDist(..),
    Distribution(..),
    Bernoulli(..), Beta(..), Binomial(..), Cauchy(..), HalfCauchy(..), Categorical(..), Deterministic(..), Discrete(..), Dirichlet(..), Gamma(..), Normal(..), HalfNormal(..), Poisson(..), UniformD(..), Uniform(..),
    PrimVal
  -- , pattern PrimValDistPrf
  -- , pattern TypeableDistPrf
  -- * Sampling
  -- , sampleBayes
  -- * Density
  -- , gradLogProb
  ) where

import Debug.Trace
import Data.Kind ( Constraint )
import Data.Map (Map)
import Data.Functor
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
import Data.Bifunctor (second)
-- import qualified Control.Monad.Bayes.Class as MB

type PrimDist d a = (Distribution d, Typeable d, Support d ~ a)

class Distribution d where
  type family Support d :: *
  {- | Given a random double @r@ between 0 and 1, this is passed to a distribution's inverse
       cumulative density function to draw a sampled value.
  -}
  sampleInv   :: d -> Double -> Sampler (Support d)

  {- | Draw a value from a primitive distribution in the @Sampler@ monad.
  -}
  sample      :: d -> Sampler (Support d)

  {- | Compute the log density of a primitive distribution generating an observed value.
  -}
  logProbRaw  :: d -> Support d -> Double

  {- | Compute the log density as the LogP type
  -}
  logProb     :: d -> Support d -> LogP
  logProb d   = LogP . logProbRaw d

-- | Primitive distribution
-- type PrimDist d a = PrimDist {}

newtype Bernoulli = Bernoulli Double     -- ^ probability of @True@
  deriving Show

instance Distribution Bernoulli where
  type Support Bernoulli = Bool

  sampleInv :: Bernoulli -> Double -> Sampler Bool
  sampleInv (Bernoulli p) = sampleBernoulliInv p

  sample :: Bernoulli -> Sampler Bool
  sample (Bernoulli p) = sampleBernoulli p

  logProbRaw :: Bernoulli -> Bool -> Double
  logProbRaw (Bernoulli p) y
    | y         = log p
    | otherwise = log (1 - p)

data Beta = Beta Double Double  -- α, β
  deriving Show

instance Distribution Beta where
  type Support Beta = Double

  sampleInv :: Beta -> Double -> Sampler Double
  sampleInv (Beta α β) = sampleBetaInv α β

  sample :: Beta -> Sampler Double
  sample (Beta α β) = sampleBeta α β

  logProbRaw :: Beta -> Double -> Double
  logProbRaw (Beta α β) x
    | α <= 0 || β <= 0 = error "betaLogPdfRaw:  α <= 0 || β <= 0 "
    | x <= 0 || x >= 1 = m_neg_inf
    | otherwise = (α-1)*log x + (β-1)*log1p (-x) - logBeta α β

data Binomial = Binomial Int Double -- ^ number of trials, probability of success
  deriving Show

instance Distribution Binomial where
  type Support Binomial = Int

  sampleInv :: Binomial -> Double -> Sampler Int
  sampleInv (Binomial n p) = sampleBinomialInv n p

  sample :: Binomial -> Sampler Int
  sample (Binomial n p) = sampleBinomial n p

  logProbRaw :: Binomial -> Int -> Double
  logProbRaw (Binomial n p) y
    | y < 0 || y > n          = m_neg_inf
    | n < 0                   = error "binomialLogPdfRaw: n < 0"
    | otherwise               = logChoose n y + log p * y' + log1p (-p) * ny'
    where
      y'  = fromIntegral   y
      ny' = fromIntegral $ n - y

newtype Categorical = Categorical [Double]  -- ^ list of @n@ probabilities
  deriving Show

instance Distribution Categorical where
  type Support Categorical = Int            -- ^ an index from @0@ to @n - 1@

  sampleInv :: Categorical -> Double -> Sampler Int
  sampleInv (Categorical ps) = sampleCategoricalInv (V.fromList ps)

  sample :: Categorical -> Sampler Int
  sample (Categorical ps) = sampleCategorical (V.fromList ps)

  logProbRaw :: Categorical -> Int -> Double
  logProbRaw (Categorical ps) idx
    | idx < 0 || idx >= length ps = trace "CategoricalLogPdf: idx < 0 || idx >= length ps" m_neg_inf
    | otherwise                   = log (ps !! idx)

data Cauchy = Cauchy Double Double          -- ^ location, scale
  deriving Show

instance Distribution Cauchy where
  type Support Cauchy = Double

  sampleInv :: Cauchy -> Double -> Sampler Double
  sampleInv (Cauchy loc scale) = sampleCauchyInv loc scale

  sample :: Cauchy -> Sampler (Support Cauchy)
  sample (Cauchy loc scale) = sampleCauchy loc scale

  logProbRaw :: Cauchy -> Double -> Double
  logProbRaw (Cauchy loc scale) x
    | scale <= 0 = error "cauchyLogPdfRaw: scale <= 0"
    | otherwise     = -(log pi) + log scale - log (xloc**2 + scale**2)
    where xloc = x - loc

newtype HalfCauchy = HalfCauchy Double      -- ^ scale
  deriving Show

instance Distribution HalfCauchy where
  type Support HalfCauchy = Double

  sampleInv :: HalfCauchy -> Double -> Sampler Double
  sampleInv (HalfCauchy scale) r = sampleCauchyInv 0 scale r <&> abs

  sample :: HalfCauchy -> Sampler Double
  sample (HalfCauchy scale)  = sampleCauchy 0 scale <&> abs

  logProbRaw :: HalfCauchy -> Double -> Double
  logProbRaw (HalfCauchy scale) x
    | x < 0     = m_neg_inf
    | otherwise = log 2 + logProbRaw (Cauchy 0 scale) x

data Deterministic a where
  Deterministic
    :: (Show a, Typeable a, Eq a)
    => a                                  -- ^ value of probability @1@
    -> Deterministic a

instance Distribution (Deterministic a) where
  type Support (Deterministic a) = a

  sampleInv :: Deterministic a -> Double -> Sampler a
  sampleInv (Deterministic x) _ = pure x

  sample :: Deterministic a -> Sampler a
  sample (Deterministic x) = pure x

  logProbRaw :: Deterministic a -> a -> Double
  logProbRaw (Deterministic x) y
    | x == y    = 0
    | otherwise = m_neg_inf

newtype Dirichlet = Dirichlet  [Double]  -- ^ concentrations
  deriving Show

instance Distribution Dirichlet where
  type Support Dirichlet = [Double]

  sampleInv :: Dirichlet -> Double -> Sampler [Double]
  sampleInv (Dirichlet αs) = sampleDirichletInv αs

  sample :: Dirichlet -> Sampler [Double]
  sample (Dirichlet αs)    = sampleDirichlet αs

  logProbRaw :: Dirichlet -> [Double] -> Double
  logProbRaw (Dirichlet αs) xs
    | length xs /= length αs     = trace "dirichletLogPdfRaw: length xs /= length αs" m_neg_inf
    | abs (sum xs - 1.0) > 1e-14 = trace "dirichletLogPdfRaw: data should sum to 1" m_neg_inf
    | any (<= 0) αs              = trace "dirichletLogPdfRaw: weights should be positive" m_neg_inf
    | any (<  0) xs              = trace "dirichletLogPdfRaw: data should be non-negative" m_neg_inf
    | otherwise = c + sum (zipWith (\a x -> (a - 1) * log x) αs xs)
    where c = - sum (map logGamma αs) + logGamma (sum αs)

data Discrete a where
  Discrete
    :: (Show a, Typeable a, Eq a)
    => [(a, Double)]                    -- ^ values and associated probabilities
    -> Discrete a

instance Distribution (Discrete a) where
  type Support (Discrete a) = a

  sampleInv :: Discrete a -> Double -> Sampler a
  sampleInv (Discrete xps) = sampleDiscreteInv xps

  sample :: Discrete a -> Sampler a
  sample (Discrete xps)    = sampleDiscrete xps

  logProbRaw :: Discrete a -> a -> Double
  logProbRaw (Discrete xps) y =
    case lookup y xps of
      Nothing -> trace ("Couldn't find " ++ show y ++ " in Discrete dist") m_neg_inf
      Just p  -> log p

data UniformD = UniformD Int Int      -- ^ lower-bound @min@, upper-bound @max@
  deriving Show

instance Distribution UniformD where
  type Support UniformD = Int

  sampleInv :: UniformD -> Double -> Sampler Int
  sampleInv (UniformD min max) = sampleUniformDInv min max

  sample :: UniformD -> Sampler Int
  sample (UniformD min max)    = sampleUniformD min max

  logProbRaw :: UniformD -> Int -> Double
  logProbRaw (UniformD min max) idx
    | max <  min              = error "uniformDLogPdfRaw: max < min"
    | idx < min || idx > max  = m_neg_inf
    | otherwise               = - log (fromIntegral $ max - min + 1)

data Gamma = Gamma Double Double           -- ^ shape k, scale θ
  deriving Show

instance Distribution Gamma where
  type Support Gamma = Double

  sampleInv :: Gamma -> Double -> Sampler Double
  sampleInv (Gamma k θ) = sampleGammaInv k θ

  sample :: Gamma -> Sampler Double
  sample (Gamma k θ) = sampleGamma k θ

  logProbRaw :: Gamma -> Double -> Double
  logProbRaw (Gamma k θ) x
    | x <= 0           = m_neg_inf
    | k <= 0 || θ <= 0 = error "gammaLogPdfRaw: k <= 0 || θ <= 0"
    | otherwise = (k - 1) * log x - (x/θ) - logGamma k - (k * log θ)

data Normal = Normal Double Double      -- ^ mean μ, standard deviation σ
  deriving Show

instance Distribution Normal where
  type Support Normal = Double

  sampleInv :: Normal -> Double -> Sampler Double
  sampleInv (Normal μ σ) = sampleNormalInv μ σ

  sample :: Normal -> Sampler Double
  sample (Normal μ σ) = sampleNormal μ σ

  logProbRaw :: Normal -> Double -> Double
  logProbRaw (Normal μ σ) x
    | σ <= 0     = error "normalLogPdfRaw: σ <= 0"
    | otherwise  = -(xμ * xμ / (2 * (σ ** 2))) - log m_sqrt_2_pi - log σ
    where xμ = x - μ

newtype HalfNormal = HalfNormal Double      -- ^ standard deviation σ
  deriving Show

instance Distribution HalfNormal where
  type Support HalfNormal = Double

  sampleInv :: HalfNormal -> Double -> Sampler Double
  sampleInv (HalfNormal σ) r = sampleNormalInv 0 σ r <&> abs

  sample :: HalfNormal -> Sampler Double
  sample (HalfNormal σ) = sampleNormal 0 σ <&> abs

  logProbRaw :: HalfNormal -> Double -> Double
  logProbRaw (HalfNormal σ) x
    | x < 0     = m_neg_inf
    | otherwise = log 2 + logProbRaw (Normal 0 σ) x

newtype Poisson = Poisson Double           -- ^ rate λ
  deriving Show

instance Distribution Poisson where
  type Support Poisson = Int

  sampleInv :: Poisson -> Double -> Sampler Int
  sampleInv (Poisson λ) = samplePoissonInv λ

  sample :: Poisson -> Sampler Int
  sample (Poisson λ) = samplePoisson λ

  logProbRaw :: Poisson -> Int -> Double
  logProbRaw (Poisson λ) y
    | λ < 0     = error "poissonLogPdfRaw:  λ < 0 "
    | y < 0     = trace "poissonLogPdfRaw:  y < 0 " m_neg_inf
    | otherwise = log λ * fromIntegral y - logFactorial y - λ

data Uniform = Uniform Double Double      -- ^ lower-bound @min@, upper-bound @max@
  deriving Show

instance Distribution Uniform where
  type Support Uniform = Double

  sampleInv :: Uniform -> Double -> Sampler Double
  sampleInv (Uniform min max) = sampleUniformInv min max

  sample :: Uniform -> Sampler Double
  sample (Uniform min max) = sampleUniform min max

  logProbRaw :: Uniform -> Double -> Double
  logProbRaw (Uniform min max) x
    | max <  min         = error "uniformLogPdfRaw: max < min"
    | x < min || x > max = m_neg_inf
    | otherwise          = -log(max - min)

-- -- | Given fixed parameters and observed value, compute the gradients of a distribution's log-pdf w.r.t its parameters
-- gradLogProb ::
--      PrimDist a
--   -> a
--   -> PrimDist a
-- gradLogProb (Normal μ σ) x =
--   let (dμ, dσ, _) = normalGradLogPdfRaw μ σ x
--   in  Normal dμ dσ
-- gradLogProb (HalfNormal σ) x =
--   let (dσ, _) = halfNormalGradLogPdfRaw σ x
--   in  HalfNormal dσ
-- gradLogProb (Cauchy loc scale) x =
--   let (dloc, dscale, _) = cauchyGradLogPdfRaw loc scale x
--   in  Cauchy dloc dscale
-- gradLogProb (HalfCauchy scale) x =
--   let (dscale, _) = halfCauchyGradLogPdfRaw scale x
--   in  HalfNormal dscale
-- gradLogProb (Gamma k θ) x =
--   let (dk, dθ, _) = gammaGradLogPdfRaw k θ x
--   in  Gamma dk dθ
-- gradLogProb (Beta α β) x =
--   let (dα, dβ, _) = betaGradLogPdfRaw α β x
--   in Beta dα dβ
-- gradLogProb (Uniform min max) y =
--   Uniform 0 0
-- gradLogProb (Dirichlet as) ys =
--   let (das, _) = dirichletGradLogPdfRaw as ys
--   in  Dirichlet das
-- gradLogProb (Bernoulli p) x =
--   let (dp, _) = bernoulliGradLogPdfRaw p x
--   in  Bernoulli dp
-- gradLogProb (Binomial n p) y =
--   let (_, dp, _) = binomialGradLogPdfRaw n p y
--   in  Binomial 0 dp
-- gradLogProb (Poisson λ) y =
--   let dλ = poissonGradLogPdfRaw λ y
--   in  Poisson  dλ
-- gradLogProb (Categorical ps) idx =
--   Categorical (map (const 0) ps)
-- gradLogProb (Discrete ps) y =
--   Discrete (map (second (const 0)) ps)
-- gradLogProb (UniformD min max) y =
--   UniformD 0 0
-- gradLogProb (Deterministic x) y =
--   error "gradLogProb for Deterministic is undefined"

{- Continuous distributions
-}

-- | Normal

normalGradLogPdfRaw :: Double -> Double -> Double -> (Double, Double, Double)
normalGradLogPdfRaw μ σ x
  | σ <= 0 = error "normalGradLogPdfRaw: σ <= 0"
  | otherwise     = (dμ, dσ, dx)
  where xμ = x - μ
        dμ = xμ/(σ ** 2)
        dσ = -1/σ + (xμ**2)/(σ ** 3)
        dx = -dμ

-- | HalfNormal

halfNormalGradLogPdfRaw :: Double -> Double -> (Double, Double)
halfNormalGradLogPdfRaw σ x
  | x < 0         = error "halfNormalGradLogPdfRaw: No gradient at x < 0"
  | σ <= 0        = error "halfNormalGradLogPdfRaw: σ <= 0"
  | otherwise     = let (_, dσ, dx) = normalGradLogPdfRaw 0 σ x in (dσ, dx)

-- | Cauchy

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

halfCauchyGradLogPdfRaw :: Double -> Double -> (Double, Double)
halfCauchyGradLogPdfRaw scale x
  | scale <= 0 = error "cauchyGradLogPdfRaw: scale <= 0"
  | otherwise  = let (_, ds, dx) = cauchyGradLogPdfRaw 0 scale x in (ds, dx)

-- | Gamma
gammaGradLogPdfRaw :: Double -> Double -> Double -> (Double, Double, Double)
gammaGradLogPdfRaw k t x
  | k <= 0 || t <= 0 = error "gammaGradLogPdfRaw: k <= 0 || t <= 0"
  | x <= 0           = error "gammaGradLogPdfRaw: x <= 0"
  | otherwise = (dk, dt, dx)
  where dk = log x - digamma k - log t
        dt = x/(t**2) - k/t
        dx = (k - 1)/x - 1/t

-- | Beta


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

dirichletGradLogPdfRaw :: [Double] -> [Double] -> ([Double], [Double])
dirichletGradLogPdfRaw as xs
  | length xs /= length as     = error "dirichletGradLogPdfRaw: length xs /= length as"
  | abs (sum xs - 1.0) > 1e-14 = error "dirichletGradLogPdfRaw: data should sum to 1"
  | any (<= 0) as              = error "dirichletGradLogPdfRaw: weights should be positive"
  | any (<  0) xs              = error "dirichletGradLogPdfRaw: data should be non-negative"
  | otherwise = (zipWith derivA as xs, zipWith derivX as xs)
  where derivA a x  = -(digamma a) + digamma (sum as) + log x
        derivX a x = (a - 1) / x

{- Discrete distributions
-}


bernoulliGradLogPdfRaw :: Double -> Bool -> (Double, Bool)
bernoulliGradLogPdfRaw p y = (dp, y)
  where dp = 1/p - fromIntegral (boolToInt y)/(1 - p)

-- | Binomial

binomialGradLogPdfRaw :: Int -> Double -> Int -> (Int, Double, Int)
binomialGradLogPdfRaw n p y
  | y < 0 || y > n          = error "binomialGradLogPdfRaw: y < 0 || y > n"
  | n < 0                   = error "binomialGradLogPdfRaw: n < 0"
  | otherwise               = (dn, dp, dy)
  where dn = 0
        dp = fromIntegral n/p - fromIntegral (n - y)/(1 - p)
        dy = 0

-- | Poisson

poissonGradLogPdfRaw :: Double -> Int -> Double
poissonGradLogPdfRaw λ y
  | λ < 0     = error "poissonGradLogPdfRaw:  λ < 0 "
  | y < 0     = error "poissonGradLogPdfRaw:  y < 0 "
  | otherwise = (fromIntegral y/λ) - 1


{- Dictionary proofs for constraints on primitive distributions
-}
data Dict c a where
  Dict :: c a => Dict c a

-- | An ad-hoc specification of primitive value types, for constraining the outputs of distributions
type PrimVal = '[Int, Double, [Double], Bool, String]

class    (Show x, OpenSum.Member x PrimVal) => IsPrimVal x

instance (Show x, OpenSum.Member x PrimVal) => IsPrimVal x

-- -- | For pattern-matching on an arbitrary @PrimDist@ with proof that it generates a primitive value
-- pattern PrimValDistPrf :: () => (IsPrimVal x) => PrimDist x -> PrimDist x
-- pattern PrimValDistPrf d <- d@(primValDistPrf -> Just Dict)

-- primValDistPrf :: PrimDist x -> Maybe (Dict IsPrimVal x)
-- primValDistPrf d = case d of
--   HalfCauchy {} -> Just Dict
--   Cauchy {} -> Just Dict
--   Normal {} -> Just Dict
--   HalfNormal  {} -> Just Dict
--   Uniform  {} -> Just Dict
--   UniformD {} -> Just Dict
--   Gamma {} -> Just Dict
--   Beta {} -> Just Dict
--   Binomial {} -> Just Dict
--   Bernoulli {} -> Just Dict
--   Categorical {} -> Just Dict
--   Poisson {} -> Just Dict
--   Dirichlet {} -> Just Dict
--   Deterministic {} -> Nothing
--   Discrete {} -> Nothing

-- -- | For pattern-matching on an arbitrary @PrimDist@ with proof that it generates a primitive value
-- pattern TypeableDistPrf :: () => (Typeable x) => PrimDist x -> PrimDist x
-- pattern TypeableDistPrf d <- d@(typeableDistPrf -> Dict)

-- typeableDistPrf :: Distribution d => d -> Dict Typeable x
-- typeableDistPrf d = case d of
--   HalfCauchy {} ->  Dict
--   Cauchy {} ->  Dict
--   Normal {} ->  Dict
--   HalfNormal  {} ->  Dict
--   Uniform  {} ->  Dict
--   UniformD {} ->  Dict
--   Gamma {} ->  Dict
--   Beta {} ->  Dict
--   Binomial {} ->  Dict
--   Bernoulli {} ->  Dict
--   Categorical {} ->  Dict
--   Poisson {} ->  Dict
--   Dirichlet {} ->  Dict
--   Discrete {} ->  Dict
--   Deterministic {} ->  Dict

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