
{-# LANGUAGE FlexibleContexts #-}


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

{- | A GADT encoding of (a selection of) primitive distributions
    along with their corresponding sampling and density functions.
-}

module Dist
  (Distribution(..), DiffDistribution(..), Dist, DiffDist, Witness(..),
   Beta, mkBeta, Bernoulli, mkBernoulli, Binomial, mkBinomial, Categorical, mkCategorical, Cauchy, mkCauchy, HalfCauchy, mkHalfCauchy,
   Deterministic, mkDeterministic,
   ProxyDist(..), mkProxyDist, Discrete, mkDiscrete, Dirichlet, mkDirichlet, Gamma, mkGamma, Normal(..), mkNormal, HalfNormal, mkHalfNormal,
   Poisson, mkPoisson, Uniform, mkUniform, UniformD, mkUniformD) where

import           Debug.Trace ( trace )
import           Data.Proxy
import           Data.Kind ( Constraint, Type )
import           Data.List ( transpose )
import           Data.Functor ( (<&>) )
import           Data.Maybe
import           Data.Typeable ( Typeable )
import           Data.Type.Nat
import           Data.Fin
import           Numeric.MathFunctions.Constants
  ( m_eulerMascheroni, m_neg_inf, m_sqrt_2_pi, m_sqrt_2, m_pos_inf )
import           Numeric.SpecFunctions
  ( incompleteBeta, invIncompleteBeta, logBeta, logGamma, digamma, log1p, logChoose, logFactorial, invErfc, invIncompleteGamma)
import qualified Data.Vector as Vector
import           Data.Vector (Vector)
import           Sampler
import           LogP ( LogP(..) )
import           Util ( boolToInt, mean, covariance, variance )
import qualified Vec
import           Vec (Vec(..), TypeableSNatI)

{- import qualified Control.Monad.Bayes.Class as MB
   import           Numeric.Log ( Log(..) )
-}

{- Distributions that can be sampled from and conditioned against.
-}
-- | Shorthand for specifying a distribution @d@ and its type of support @a@
type Dist d a = (Distribution d, Base d ~ a)

class (Show d, Typeable d, Show (Base d), Typeable (Base d)) => Distribution d where
  type family Base d :: Type
  {- | Given a random double @r@ in (0, 1), this is passed to a distribution's inverse
       cumulative density function to draw a sampled value. -}
  draw   :: d -> Double -> Base d

  drawWithSampler :: d -> Sampler (Base d)
  drawWithSampler d = fmap (draw d) random

  {- | Compute the log density of a primitive distribution generating an observed value. -}
  logProb  :: d -> Base d -> LogP

  {- | Compute the probability density. -}
  prob        :: d -> Base d -> Double
  prob d      = exp . logProb d

  {- | Provide proof that @d@ is differentiable. -}
  isDifferentiable :: d -> Maybe (Witness DiffDistribution d)
  isDifferentiable _ = Nothing

-- | Dictionary proof
data Witness (c :: Type -> Constraint) a where
  Witness :: c a => Witness c a

{- Distributions that can be differentiated with respect to their parameters
-}
type DiffDist d a = (DiffDistribution d, Base d ~ a)

class (SNatI (Arity d), Distribution d) => DiffDistribution d where
  type family Arity d :: Nat
  {- | Compute the gradient log-probability. -}
  gradLogProb :: d -> Base d -> Vec (Arity d) Double

  safeAddGrad :: d -> Vec (Arity d) Double -> d

  toList :: d -> [Double]

-- | For deterministic values that do not affect inference
data Deterministic a = Deterministic a deriving (Show, Eq)

mkDeterministic :: (Show a, Eq a) => a -> Deterministic a
mkDeterministic = Deterministic

instance (Typeable a, Show a) => Distribution (Deterministic a) where
  type Base (Deterministic a) = a
  draw (Deterministic a) _ = a
  logProb _ _ = 0

-- | For storing a pair of distribution and observed value
data ProxyDist d = forall a. Dist d a => ProxyDist d a

mkProxyDist :: (Dist d a) => d -> a -> ProxyDist d
mkProxyDist = ProxyDist

instance (Show d) => Show (ProxyDist d) where
  show (ProxyDist d x) = "ProxyDist " ++ show x

instance (Dist d a) => Distribution (ProxyDist d) where
  type Base (ProxyDist d) = Base d

  draw :: Dist d a => ProxyDist d -> Double -> Base (ProxyDist d)
  draw (ProxyDist d y) _ = y

  logProb :: ProxyDist d -> a -> Double
  logProb (ProxyDist d y) _ = logProb d y

-- | Beta(α, β)
data Beta = Beta Double Double
  deriving Show

mkBeta :: Double -> Double -> Beta
mkBeta α β
  | α > 0 && β > 0 = Beta α β
  | otherwise      = error "Beta: α > 0 or β > 0 not met"

instance Distribution Beta where
  type Base Beta = Double

  draw :: Beta -> Double -> Double
  draw (Beta α β) = invIncompleteBeta α β
  logProb :: Beta -> Double -> Double
  logProb (Beta α β) x
    | x <= 0 || x >= 1 = m_neg_inf
    | otherwise = (α-1)*log x + (β-1)*log1p (-x) - logBeta α β

  isDifferentiable :: Beta -> Maybe (Witness DiffDistribution Beta)
  isDifferentiable _ = Just Witness

instance DiffDistribution Beta where
  type Arity Beta = Nat2

  gradLogProb :: Beta -> Double -> Vec Nat2 Double
  gradLogProb (Beta α β) x
    | x < 0 || x > 1 = error $ "betaGradLogPdfRaw: x <= 0 || x > 1" ++ show x
    | otherwise = da ::: db ::: VNil
    where digamma_ab = digamma (α + β)
          da = log x       - digamma α + digamma_ab
          db = log (1 - x) - digamma β + digamma_ab
          dx = (α - 1)/x + (β - 1)/(1 - x)

  safeAddGrad (Beta α β) (dα ::: dβ ::: VNil) = Beta α' β'
    where α' = let α_new = α + dα in if α_new <= 0 || isNaN α_new then α else α_new
          β' = let β_new = β + dβ in if β_new <= 0 || isNaN β_new then β else β_new

  toList :: Beta -> [Double]
  toList (Beta dα dβ) = [dα, dβ]

-- | Cauchy(location, scale)
data Cauchy = Cauchy Double Double
  deriving Show

mkCauchy ::  Double -> Double -> Cauchy
mkCauchy loc scale
  | scale > 0 = Cauchy loc scale
  | otherwise = error "Cauchy: scale > 0 not met"

instance Distribution Cauchy where
  type Base Cauchy = Double

  draw :: Cauchy -> Double -> Double
  draw (Cauchy loc scale) r = loc + scale * tan( pi * (r - 0.5) )

  logProb :: Cauchy -> Double -> Double
  logProb (Cauchy loc scale) x = -(log pi) + log scale - log (xloc**2 + scale**2)
    where xloc = x - loc

  isDifferentiable :: Cauchy -> Maybe (Witness DiffDistribution Cauchy)
  isDifferentiable _ = Just Witness

instance DiffDistribution Cauchy where
  type Arity Cauchy = Nat2

  gradLogProb :: Cauchy -> Double -> Vec Nat2 Double
  gradLogProb (Cauchy loc scale) x
    | scale <= 0 = error "cauchyGradLogPdfRaw: scale <= 0"
    | otherwise     = dloc ::: dscale ::: VNil
    where xloc      = x - loc
          xlocSqrd  = xloc**2
          scaleSqrd = scale**2
          dloc = (2 * xloc)/(xlocSqrd + scaleSqrd)
          dscale = 1/scale - (2 * scale)/(xlocSqrd + scaleSqrd)
          dx = -dloc

  safeAddGrad (Cauchy loc scale) (dloc ::: dscale ::: VNil) = Cauchy loc' scale'
    where loc'   = loc + dloc
          scale' = let new_scale = scale + dscale in if new_scale <= 0 then scale else new_scale

  toList :: Cauchy -> [Double]
  toList (Cauchy loc scale) = [loc, scale]

-- | HalfCauchy(scale)
newtype HalfCauchy = HalfCauchy Double
  deriving Show

mkHalfCauchy :: Double -> HalfCauchy
mkHalfCauchy scale
  | scale > 0 = HalfCauchy scale
  | otherwise = error "HalfCauchy: scale > 0 not met"

instance Distribution HalfCauchy where
  type Base HalfCauchy = Double

  draw :: HalfCauchy -> Double ->  Double
  draw (HalfCauchy scale) r = abs $ draw (Cauchy 0 scale) r

  logProb :: HalfCauchy -> Double -> Double
  logProb (HalfCauchy scale) x
    | x < 0     = m_neg_inf
    | otherwise = log 2 + logProb (Cauchy 0 scale) x

  isDifferentiable :: HalfCauchy -> Maybe (Witness DiffDistribution HalfCauchy)
  isDifferentiable _ = Just Witness

instance DiffDistribution HalfCauchy where
  type Arity HalfCauchy = Nat1

  gradLogProb :: HalfCauchy -> Double -> Vec Nat1 Double
  gradLogProb (HalfCauchy scale) x
    | x < 0      = error "cauchyGradLogProb: x < 0"
    | otherwise  = let (_ ::: dscale ::: VNil) = gradLogProb (Cauchy 0 scale) x
                   in Vec.singleton dscale

  safeAddGrad (HalfCauchy scale) (dscale ::: VNil) = HalfCauchy scale'
    where scale' = let new_scale = scale + dscale in if new_scale <= 0 then scale else new_scale

  toList :: HalfCauchy -> [Double]
  toList (HalfCauchy scale) = [scale]

-- | Dirichlet(αs)
--   @αs@ concentrations
newtype Dirichlet (n :: Nat) = Dirichlet (Vec n Double)
  deriving Show

mkDirichlet :: Vec n Double -> Dirichlet n
mkDirichlet αs
  | any (<= 0) αs = error "Dirichlet: αs > 0 not met"
  | length αs < 2 = error "Dirichlet: length αs >= 2 not met"
  | otherwise     = Dirichlet αs

instance (TypeableSNatI n) => Distribution (Dirichlet n) where
  type Base (Dirichlet n) = Vec n Double

  draw :: Dirichlet n -> Double -> Vec n Double
  draw (Dirichlet αs) r =
    let rs = Vec.linCongGen r snat
        xs = Vec.zipWith (\α r -> draw (Gamma α 1) r) αs rs
    in  Vec.map (/sum xs) xs

  logProb :: Dirichlet n -> Vec n Double -> Double
  logProb (Dirichlet αs) xs
    | length xs /= length αs     = trace "dirichletLogPdfRaw: length xs /= length αs" m_neg_inf
    | abs (sum xs - 1.0) > 1e-14 = trace "dirichletLogPdfRaw: data should sum to 1" m_neg_inf
    | any (<  0) xs              = trace "dirichletLogPdfRaw: data should be non-negative" m_neg_inf
    | otherwise = c + sum (Vec.zipWith (\a x -> (a - 1) * log x) αs xs)
    where c = - sum (Vec.map logGamma αs) + logGamma (sum αs)

  isDifferentiable :: Dirichlet n -> Maybe (Witness DiffDistribution (Dirichlet n))
  isDifferentiable _ = Just Witness

instance (TypeableSNatI n) => DiffDistribution (Dirichlet n) where
  type Arity (Dirichlet n) = n

  gradLogProb :: Dirichlet n -> Vec n Double -> Vec n Double
  gradLogProb (Dirichlet αs) xs
    | length xs /= length αs     = error "dirichletGradLogPdfRaw: length xs /= length αs"
    | abs (sum xs - 1.0) > 1e-14 = error "dirichletGradLogPdfRaw: data should sum to 1"
    | any (<  0) xs              = error "dirichletGradLogPdfRaw: data should be non-negative"
    | otherwise = Vec.zipWith derivA αs xs
    where derivA a x  = -(digamma a) + digamma (sum αs) + log x
          derivX a x = (a - 1) / x

  safeAddGrad (Dirichlet αs) dαs = Dirichlet (Vec.zipWith add_dα αs dαs)
    where add_dα α dα = let α_new = α + dα in if α_new <= 0 || isNaN (α_new)  then α else α_new

  toList :: Dirichlet n -> [Double]
  toList (Dirichlet dαs) = Vec.toList dαs

-- | Gamma(k, θ)
--   @k@ shape, @θ@ scale
data Gamma = Gamma Double Double
  deriving Show

mkGamma :: Double -> Double -> Gamma
mkGamma k θ
  | k > 0 && θ > 0 = Gamma k θ
  | otherwise      = error "Gamma: k > 0 && θ > 0 not met"

instance Distribution Gamma where
  type Base Gamma = Double

  draw :: Gamma -> Double -> Double
  draw (Gamma k θ) r = θ * invIncompleteGamma k r

  logProb :: Gamma -> Double -> Double
  logProb (Gamma k θ) x
    | x <= 0    = m_neg_inf
    | otherwise = (k - 1) * log x - (x/θ) - logGamma k - (k * log θ)

  isDifferentiable :: Gamma -> Maybe (Witness DiffDistribution Gamma)
  isDifferentiable _ = Just Witness

instance DiffDistribution Gamma where
  type Arity Gamma = Nat2

  gradLogProb :: Gamma -> Double -> Vec Nat2 Double
  gradLogProb (Gamma k θ) x
    | x <= 0   = error "gammaGradLogPdfRaw: x <= 0"
    | otherwise = dk ::: dθ ::: VNil
    where dk = log x - digamma k - log θ
          dθ = x/(θ**2) - k/θ
          dx = (k - 1)/x - 1/θ

  -- safeAddGrad :: Gamma -> Gamma -> Gamma
  safeAddGrad (Gamma k θ) (dk ::: dθ ::: VNil) = Gamma k' θ'
    where k' = let k_new = k + dk in if k_new <= 0 then k else k_new
          θ' = let θ_new = θ + dθ in if θ_new <= 0 then θ else θ_new

  toList :: Gamma -> [Double]
  toList (Gamma dk dθ) = [dk, dθ]

-- | Normal(μ, σ)
--   @μ@ mean, @σ@ standard deviation
data Normal = Normal Double Double
  deriving Show

mkNormal :: Double -> Double -> Normal
mkNormal μ σ
  | σ > 0 = Normal μ σ
  | otherwise = error "Normal: σ > 0 not met"

instance Distribution Normal where
  type Base Normal = Double

  draw :: Normal -> Double -> Double
  draw (Normal μ σ) r = (- invErfc (2 * r)) * (m_sqrt_2 * σ) + μ

  logProb :: Normal -> Double -> Double
  logProb (Normal μ σ) x = -(xμ * xμ / (2 * (σ ** 2))) - log m_sqrt_2_pi - log σ
    where xμ = x - μ

  isDifferentiable :: Normal -> Maybe (Witness DiffDistribution Normal)
  isDifferentiable _ = Just Witness

instance DiffDistribution Normal where
  type Arity Normal = Nat2

  gradLogProb :: Normal -> Double -> Vec Nat2 Double
  gradLogProb (Normal μ σ) x = dμ ::: dσ ::: VNil
    where xμ = x - μ
          dμ = xμ/(σ ** 2)
          dσ = -1/σ + (xμ**2)/(σ ** 3)
          dx = -dμ

  safeAddGrad (Normal μ σ) (dμ ::: dσ ::: VNil) = Normal μ' σ'
    where μ' = μ + dμ
          σ' = let σ_new = σ + dσ in if σ_new <= 0 then σ else σ_new

  toList :: Normal -> [Double]
  toList (Normal dμ dσ) = [dμ, dσ]

-- | HalfNormal(σ)
--   @σ@ standard deviation
newtype HalfNormal = HalfNormal Double
  deriving Show

mkHalfNormal :: Double -> HalfNormal
mkHalfNormal σ
  | σ > 0     = HalfNormal σ
  | otherwise = error "HalfNormal: σ > 0 not met"

instance Distribution HalfNormal where
  type Base HalfNormal = Double

  draw :: HalfNormal -> Double -> Double
  draw (HalfNormal σ) = abs . draw (Normal 0 σ)

  logProb :: HalfNormal -> Double -> Double
  logProb (HalfNormal σ) x
    | x < 0     = m_neg_inf
    | otherwise = log 2 + logProb (Normal 0 σ) x

  isDifferentiable :: HalfNormal -> Maybe (Witness DiffDistribution HalfNormal)
  isDifferentiable _ = Just Witness

instance DiffDistribution HalfNormal where
  type Arity HalfNormal = Nat1

  gradLogProb :: HalfNormal -> Double -> Vec Nat1 Double
  gradLogProb (HalfNormal σ) x
    | x < 0         = error "halfNormalGradLogPdfRaw: No gradient at x < 0"
    | otherwise     = let _ ::: dσ ::: VNil = gradLogProb (Normal 0 σ) x
                      in Vec.singleton dσ

  safeAddGrad (HalfNormal σ) (dσ ::: VNil) = HalfNormal σ'
    where σ' = let σ_new = σ + dσ in if σ_new <= 0 then σ else σ_new

  toList :: HalfNormal -> [Double]
  toList (HalfNormal σ) = [σ]

{-# INLINE invCMF #-}
invCMF
  :: (Int -> Double)  -- ^ probability mass function
  -> Double           -- ^ r
  -> Int
invCMF pmf r = f 0 r where
  f :: Int -> Double -> Int
  f i r = do
    let q  = pmf i
        r' = r - q
    if r' < 0 then i else f (i + 1) r'

invCMFNat :: forall n. SNatI n
  => (Fin (S n) -> Double)  -- ^ probability mass function
  -> Double                 -- ^ r
  -> Fin (S n)
invCMFNat pmf r = f FZ r where
  f :: Fin (S n) -> Double -> Fin (S n)
  f i r = do
    let q  = pmf i
        r' = r - q
    if r' < 0 then i else f (i + 1) r'

-- | Bernoulli(p)
--   @p@ probability of success
newtype Bernoulli = Bernoulli Double
  deriving Show

mkBernoulli :: Double -> Bernoulli
mkBernoulli p
  | p >= 0 && p <= 1 = Bernoulli p
  | otherwise        = error "Bernoulli: p >= 0 && p <= 1 not met"

instance Distribution Bernoulli where
  type Base Bernoulli = Bool

  draw :: Bernoulli -> Double -> Bool
  draw (Bernoulli p) r = r < p

  logProb :: Bernoulli -> Bool -> Double
  logProb (Bernoulli p) y
    | y         = log p
    | otherwise = log (1 - p)

bernoulliGradLogPdfRaw :: Bernoulli -> Bool -> Bernoulli
bernoulliGradLogPdfRaw (Bernoulli p) y = Bernoulli dp
  where dp = 1/p - fromIntegral (boolToInt y)/(1 - p)

-- | Binomial(n, p)
-- | @n@ number of trials, @p@ probability of success
data Binomial = Binomial Int Double
  deriving Show

mkBinomial :: Int -> Double -> Binomial
mkBinomial n p
  | p < 0 || p > 1 = error "Binomial: p >= 0 && p <= 1 not met"
  | n < 0          = error "Binomial: n >= 0 not met"
  | otherwise      = Binomial n p

instance Distribution Binomial where
  type Base Binomial = Int

  draw :: Binomial -> Double -> Int
  draw (Binomial n p) = invCMF (prob (Binomial n p))

  logProb :: Binomial -> Int -> Double
  logProb (Binomial n p) y
    | y < 0 || y > n          = m_neg_inf
    | otherwise               = logChoose n y + log p * y' + log1p (-p) * ny'
    where
      y'  = fromIntegral   y
      ny' = fromIntegral $ n - y

binomialGradLogPdfRaw :: Binomial -> Int -> Binomial
binomialGradLogPdfRaw (Binomial n p) y
  | y < 0 || y > n          = error "binomialGradLogPdfRaw: y < 0 || y > n"
  | otherwise               = Binomial dn dp
  where dn = 0
        dp = fromIntegral n/p - fromIntegral (n - y)/(1 - p)
        dy = 0

-- | Categorical(ps)
--   @ps@ probabilities of each category
newtype Categorical = Categorical [Double]
  deriving Show

mkCategorical :: [Double] -> Categorical
mkCategorical ps
  | null ps       = error "Categorical: ps must be non-empty"
  | any (< 0) ps  = error "Categorical: ps >= 0 not met"
  | any (> 0) ps  = Categorical (map (/ sum ps) ps)
  | otherwise     = Categorical (map (const $ 1/fromIntegral (length ps)) ps)

instance Distribution Categorical where
  type Base Categorical = Int            -- ^ an index from @0@ to @n - 1@

  draw :: Categorical  -> Double -> Int
  draw (Categorical ps) = invCMF (ps !!)

  logProb :: Categorical  -> Int -> Double
  logProb (Categorical ps) idx
    | idx < 0 || idx >= length ps = trace "CategoricalLogPdf: idx < 0 || idx >= length ps" m_neg_inf
    | otherwise                   = log (ps !! idx)

-- | Discrete(xps)
--   @xps@ values `x` and associated probabilities `p`
data Discrete a where
  Discrete
    :: (Show a, Typeable a, Eq a)
    => [(a, Double)]
    -> Discrete a

mkDiscrete :: (Show a, Typeable a, Eq a) => [(a, Double)] -> Discrete a
mkDiscrete xps
  | null xps     = error "Discrete: xps must be non-empty"
  | any (< 0) ps = error "Discrete: probabilities must be >= 0"
  | any (> 0) ps = Discrete (zip xs (map (/ sum ps) ps))
  | otherwise    = Discrete (zip xs (map (const $ 1/fromIntegral (length ps)) ps))
  where (xs, ps) = unzip xps

instance Show a => Show (Discrete a) where
  show (Discrete xps) = "Discrete " ++ show xps

instance (Show a, Typeable a) => Distribution (Discrete a) where
  type Base (Discrete a) = a

  draw :: Discrete a -> Double -> a
  draw (Discrete xps) r = xs !! invCMF (ps !!) r
    where (xs, ps) = unzip xps

  logProb :: Discrete a -> a -> Double
  logProb (Discrete xps) y = case lookup y xps of
      Nothing -> trace ("Couldn't find " ++ show y ++ " in Discrete dist") m_neg_inf
      Just p  -> log p

-- | Poisson(λ)
--   @λ@ rate
newtype Poisson = Poisson Double
  deriving Show

mkPoisson :: Double -> Poisson
mkPoisson λ
  | λ < 0     = error "Poisson:  λ >= 0 not met"
  | otherwise = Poisson λ

instance Distribution Poisson where
  type Base Poisson = Int

  draw :: Poisson -> Double -> Int
  draw (Poisson λ) = invCMF (prob (Poisson λ))

  logProb :: Poisson -> Int -> Double
  logProb (Poisson λ) y
    | y < 0     = trace "poissonLogPdfRaw:  y < 0 " m_neg_inf
    | otherwise = log λ * fromIntegral y - logFactorial y - λ

poissonGradLogPdfRaw :: Poisson -> Int -> Poisson
poissonGradLogPdfRaw (Poisson λ) y
  | y < 0     = error "poissonGradLogPdfRaw:  y < 0 "
  | otherwise = Poisson ((fromIntegral y/λ) - 1)

-- | ContinuousUniform(min, max)
--   @min@ lower-bound, @max@ upper-bound
data Uniform = Uniform Double Double
  deriving Show

mkUniform :: Double -> Double -> Uniform
mkUniform min max
  | min > max = error "Uniform: min <= max not met"
  | otherwise = Uniform min max

instance Distribution Uniform where
  type Base Uniform = Double

  draw :: Uniform -> Double -> Double
  draw (Uniform min max) r = min + (max - min) * r

  logProb :: Uniform -> Double -> Double
  logProb (Uniform min max) x
    | x < min || x > max = m_neg_inf
    | otherwise          = -log(max - min)

-- | DiscreteUniform(min, max)
--   @min@ lower-bound, @max@ upper-bound
data UniformD = UniformD Int Int
  deriving Show

mkUniformD :: Int -> Int -> UniformD
mkUniformD min max
  | min > max = error "UniformD: min <= max not met"
  | otherwise = UniformD min max

instance Distribution UniformD where
  type Base UniformD = Int

  draw :: UniformD -> Double -> Int
  draw (UniformD min max) r = floor (min' + (max' - min') * r)
     where min' = fromIntegral min
           max' = fromIntegral max + 1

  logProb :: UniformD -> Int -> Double
  logProb (UniformD min max) idx
    | idx < min || idx > max  = m_neg_inf
    | otherwise               = - log (fromIntegral $ max - min + 1)
