{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

module PrimDist where

import           Debug.Trace ( trace )
import           Data.Kind ( Constraint )
import           Data.List ( transpose )
import           Data.Functor ( (<&>) )
import           OpenSum (OpenSum)
import qualified OpenSum
import qualified Data.Vector as Vector
import           Data.Vector (Vector)
import           Numeric.MathFunctions.Constants
  ( m_eulerMascheroni, m_neg_inf, m_sqrt_2_pi, m_sqrt_2, m_pos_inf )
import           Numeric.SpecFunctions
  ( incompleteBeta, invIncompleteBeta, logBeta, logGamma, digamma, log1p, logChoose, logFactorial, invErfc, invIncompleteGamma)
import           Sampler
import           LogP ( LogP(..) )
import           Control.Monad ((>=>), replicateM)
import           Data.Typeable ( Typeable )
import           Util ( linCongGen, boolToInt, mean, covariance, variance )

{- import qualified Control.Monad.Bayes.Class as MB
   import           Numeric.Log ( Log(..) )
-}

{- Distributions that can be sampled from and conditioned against.
-}
class (Show d, Typeable d) => Distribution d where
  type family Support d :: *
  {- | Given a random double @r@ in (0, 1], this is passed to a distribution's inverse
       cumulative density function to draw a sampled value.
  -}
  sampleInv   :: d -> Double -> Support d

  {- | Draw a value from a primitive distribution in the @Sampler@ monad.
  -}
  sample      :: d -> Sampler (Support d)

  {- | Compute the log density of a primitive distribution generating an observed value.
  -}
  logProbRaw  :: d -> Support d -> Double

  {- | Compute the log density as the @LogP@ type
  -}
  logProb     :: d -> Support d -> LogP
  logProb d   = LogP . logProbRaw d

  {- | Compute the probability density
  -}
  prob        :: d -> Support d -> Double
  prob d      = exp . logProbRaw d

  {- | Provide proof that @d@ is differentiable.
  -}
  isDifferentiable :: d -> Maybe (Witness DiffDistribution d)

-- | Shorthand for specifying a distribution @d@ and its type of support @a@
type PrimDist d a = (Distribution d, Support d ~ a)

-- | Dictionary proof
data Witness c a where
  Witness :: c a => Witness c a

{- Distributions that can be differentiated with respect to their parameters
-}
class Distribution d => DiffDistribution d where
  {- | Compute the gradient log-probability.
  -}
  gradLogProb :: d -> Support d -> d

  safeAddGrad :: d -> d -> d

  zeroGrad :: d

  toList   :: d -> [Double]

  fromList :: [Double] -> d

  liftUnOp :: (Double -> Double) -> d -> d
  liftUnOp op = fromList . map op . toList

  liftBinOp :: (Double -> Double -> Double) -> d -> d -> d
  liftBinOp op d1 d2 = fromList $ zipWith op (toList d1) (toList d2)

  covarGrad    -- Assuming our distribution has D parameters
    :: [d]     -- ^ (f^{1:D})^{1:L}, an L-sized list of distributions      [(D α^1 β^1), (D α^2 β^2), .. (D α^L β^L)]
    -> [d]     -- ^ (g^{1:D})^{1:L}, an L-sized list of distributions
    -> d       -- ^ covar((f^{1:D})^{1:L}, (g^{1:D})^{1:L})
  covarGrad fs gs =
    fromList $ zipWith Util.covariance params_fs params_gs
    where fs' = map toList fs       -- list of L parameter-sets of size D    [[α^1, β^1], [α^2, β^2], .. [α^L, β^L]]
          params_fs = transpose fs' -- set of D parameter-lists of size L    [[α^1, α^2, .. α^L], [β^1, β^2, .. β^L]]
          gs' = map toList gs       -- list of L parameter-sets of size D    [[α^1, β^1], [α^2, β^2], .. [α^L, β^L]]
          params_gs = transpose gs' -- set of D parameter-lists of size L    [[α^1, α^2, .. α^L], [β^1, β^2, .. β^L]]

  varGrad     -- Assuming our distribution has D parameters
    :: [d]    -- ^ (g^{1:D})^{1:L}, an L-sized list of parameter sets
    -> d      -- ^ var((g^{1:D})^{1:L})
  varGrad gs =
    fromList $ map Util.variance params_gs
    where gs'       = map toList gs   -- list of L parameter-sets of size D    [[α^1, β^1], [α^2, β^2], .. [α^L, β^L]]
          params_gs = transpose gs'   -- set of D parameter-lists of size L    [[α^1, α^2, .. α^L], [β^1, β^2, .. β^L]]


-- | Beta(α, β)
data Beta = Beta Double Double
  deriving Show

instance Distribution Beta where
  type Support Beta = Double

  sampleInv :: Beta -> Double -> Double
  sampleInv (Beta α β) r
    | r >= 0 && r <= 1 = invIncompleteBeta α β r
    | otherwise        = error $ "Beta: r must be in [0,1] range. Got: " ++ show r

  sample :: Beta -> Sampler Double
  sample (Beta α β) = sampleBeta α β

  logProbRaw :: Beta -> Double -> Double
  logProbRaw (Beta α β) x
    | α <= 0 || β <= 0 = error "betaLogPdfRaw: α <= 0 || β <= 0 "
    | x <= 0 || x >= 1 = m_neg_inf
    | otherwise = (α-1)*log x + (β-1)*log1p (-x) - logBeta α β

  isDifferentiable :: Beta -> Maybe (Witness DiffDistribution Beta)
  isDifferentiable _ = Just Witness

instance DiffDistribution Beta where
  gradLogProb :: Beta -> Double -> Beta
  gradLogProb (Beta α β) x
    | α <= 0 || β <= 0 = error "betaGradLogPdfRaw: a <= 0 || β <= 0"
    | x <= 0 || x >= 1 = error "betaGradLogPdfRaw: x <= 0 || x >= 1"
    | otherwise = Beta da db
    where digamma_ab = digamma (α + β)
          da = log x       - digamma α + digamma_ab
          db = log (1 - x) - digamma β + digamma_ab
          dx = (α - 1)/x + (β - 1)/(1 - x)

  safeAddGrad :: Beta -> Beta -> Beta
  safeAddGrad (Beta α β) (Beta dα dβ) = Beta α' β'
    where α' = let α_new = α + dα in if α_new <= 0 then α else α_new
          β' = let β_new = β + dβ in if β_new <= 0 then β else β_new

  zeroGrad :: Beta
  zeroGrad = Beta 0 0

  toList :: Beta -> [Double]
  toList (Beta dα dβ) = [dα, dβ]

  fromList :: [Double] -> Beta
  fromList [dα, dβ] = (Beta dα dβ)

-- | Cauchy(location, scale)
data Cauchy = Cauchy Double Double
  deriving Show

instance Distribution Cauchy where
  type Support Cauchy = Double

  sampleInv :: Cauchy -> Double -> Double
  sampleInv (Cauchy loc scale) r
      | 0 < r && r < 0.5 = loc - scale / tan( pi * r )
      | 0.5 < r && r < 1 = loc + scale / tan( pi * (1 - r) )
      | otherwise = error  $ "Cauchy: r must be in [0,1] range. Got: " ++ show r

  sample :: Cauchy -> Sampler Double
  sample (Cauchy loc scale) = sampleCauchy loc scale

  logProbRaw :: Cauchy -> Double -> Double
  logProbRaw (Cauchy loc scale) x
    | scale <= 0 = error "cauchyLogPdfRaw: scale <= 0"
    | otherwise     = -(log pi) + log scale - log (xloc**2 + scale**2)
    where xloc = x - loc

  isDifferentiable :: Cauchy -> Maybe (Witness DiffDistribution Cauchy)
  isDifferentiable _ = Just Witness

instance DiffDistribution Cauchy where
  gradLogProb :: Cauchy -> Double -> Cauchy
  gradLogProb (Cauchy loc scale) x
    | scale <= 0 = error "cauchyGradLogPdfRaw: scale <= 0"
    | otherwise     = Cauchy dloc dscale
    where xloc      = x - loc
          xlocSqrd  = xloc**2
          scaleSqrd = scale**2
          dloc = (2 * xloc)/(xlocSqrd + scaleSqrd)
          dscale = 1/scale - (2 * scale)/(xlocSqrd + scaleSqrd)
          dx = -dloc

  safeAddGrad :: Cauchy -> Cauchy -> Cauchy
  safeAddGrad (Cauchy loc scale) (Cauchy dloc dscale) = Cauchy loc' scale'
    where loc'   = loc + dloc
          scale' = let new_scale = scale + dscale in if new_scale <= 0 then scale else new_scale

  zeroGrad :: Cauchy
  zeroGrad = Cauchy 0 0

  toList :: Cauchy -> [Double]
  toList (Cauchy loc scale) = [loc, scale]

  fromList [loc, scale] = (Cauchy loc scale)

-- | HalfCauchy(scale)
newtype HalfCauchy = HalfCauchy Double
  deriving Show

instance Distribution HalfCauchy where
  type Support HalfCauchy = Double

  sampleInv :: HalfCauchy -> Double ->  Double
  sampleInv (HalfCauchy scale) r = abs $ sampleInv (Cauchy 0 scale) r

  sample :: HalfCauchy -> Sampler Double
  sample (HalfCauchy scale)  = sample (Cauchy 0 scale) <&> abs

  logProbRaw :: HalfCauchy -> Double -> Double
  logProbRaw (HalfCauchy scale) x
    | x < 0     = m_neg_inf
    | otherwise = log 2 + logProbRaw (Cauchy 0 scale) x

  isDifferentiable :: HalfCauchy -> Maybe (Witness DiffDistribution HalfCauchy)
  isDifferentiable _ = Just Witness

instance DiffDistribution HalfCauchy where
  gradLogProb :: HalfCauchy -> Double -> HalfCauchy
  gradLogProb (HalfCauchy scale) x
    | scale <= 0 = error "cauchyGradLogPdfRaw: scale <= 0"
    | x < 0      = error "cauchyGradLogPdfRaw: x < 0"
    | otherwise  = let (Cauchy _ dscale) = gradLogProb (Cauchy 0 scale) x in HalfCauchy dscale

  safeAddGrad :: HalfCauchy -> HalfCauchy -> HalfCauchy
  safeAddGrad (HalfCauchy scale) (HalfCauchy dscale) = HalfCauchy scale'
    where scale' = let new_scale = scale + dscale in if new_scale <= 0 then scale else new_scale

  zeroGrad :: HalfCauchy
  zeroGrad = HalfCauchy 0

  toList :: HalfCauchy -> [Double]
  toList (HalfCauchy scale) = [scale]

  fromList [scale] = (HalfCauchy scale)

-- | Dirichlet(αs)
--   @αs@ concentrations
newtype Dirichlet = Dirichlet [Double]
  deriving Show

instance Distribution Dirichlet where
  type Support Dirichlet = [Double]

  sampleInv :: Dirichlet -> Double -> [Double]
  sampleInv (Dirichlet αs) r =
    let rs = take (length αs) (linCongGen r)
        xs = zipWith (\α r -> sampleInv (Gamma α 1) r) αs rs
    in  map (/sum xs) xs

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

  isDifferentiable :: Dirichlet -> Maybe (Witness DiffDistribution Dirichlet)
  isDifferentiable _ = Just Witness

instance DiffDistribution Dirichlet where
  gradLogProb :: Dirichlet -> [Double] -> Dirichlet
  gradLogProb (Dirichlet αs) xs
    | length xs /= length αs     = error "dirichletGradLogPdfRaw: length xs /= length αs"
    | abs (sum xs - 1.0) > 1e-14 = error "dirichletGradLogPdfRaw: data should sum to 1"
    | any (<= 0) αs              = error "dirichletGradLogPdfRaw: weights should be positive"
    | any (<  0) xs              = error "dirichletGradLogPdfRaw: data should be non-negative"
    | otherwise = Dirichlet (zipWith derivA αs xs)
    where derivA a x  = -(digamma a) + digamma (sum αs) + log x
          derivX a x = (a - 1) / x

  safeAddGrad :: Dirichlet -> Dirichlet -> Dirichlet
  safeAddGrad (Dirichlet αs) (Dirichlet dαs) = Dirichlet (zipWith add_dα αs dαs)
    where add_dα α dα = let α_new = α + dα in if α_new <= 0 then α else α_new

  zeroGrad :: Dirichlet
  zeroGrad = Dirichlet (repeat 0)

  toList :: Dirichlet -> [Double]
  toList (Dirichlet dαs) = dαs

  fromList dαs = Dirichlet dαs

-- | Gamma(k, θ)
--   @k@ shape, @θ@ scale
data Gamma = Gamma Double Double
  deriving Show

instance Distribution Gamma where
  type Support Gamma = Double

  sampleInv :: Gamma -> Double -> Double
  sampleInv (Gamma k θ) r
      | r == 0         = 0
      | r == 1         = m_pos_inf
      | r > 0 && r < 1 = θ * invIncompleteGamma k r
      | otherwise      = error $ "Gamma: r must be in [0,1] range. Got: " ++ show r

  sample :: Gamma -> Sampler Double
  sample (Gamma k θ) = sampleGamma k θ

  logProbRaw :: Gamma -> Double -> Double
  logProbRaw (Gamma k θ) x
    | x <= 0           = m_neg_inf
    | k <= 0 || θ <= 0 = error "gammaLogPdfRaw: k <= 0 || θ <= 0"
    | otherwise = (k - 1) * log x - (x/θ) - logGamma k - (k * log θ)

  isDifferentiable :: Gamma -> Maybe (Witness DiffDistribution Gamma)
  isDifferentiable _ = Just Witness

instance DiffDistribution Gamma where
  gradLogProb :: Gamma -> Double -> Gamma
  gradLogProb (Gamma k θ) x
    | k <= 0 || θ <= 0 = error "gammaGradLogPdfRaw: k <= 0 || t <= 0"
    | x <= 0           = error "gammaGradLogPdfRaw: x <= 0"
    | otherwise = Gamma dk dθ
    where dk = log x - digamma k - log θ
          dθ = x/(θ**2) - k/θ
          dx = (k - 1)/x - 1/θ

  safeAddGrad :: Gamma -> Gamma -> Gamma
  safeAddGrad (Gamma k θ) (Gamma dk dθ) = Gamma k' θ'
    where k' = let k_new = k + dk in if k_new <= 0 then k else k_new
          θ' = let θ_new = θ + dθ in if θ_new <= 0 then θ else θ_new

  zeroGrad :: Gamma
  zeroGrad = Gamma 0 0

  toList :: Gamma -> [Double]
  toList (Gamma dk dθ) = [dk, dθ]

  fromList [dk, dθ] = Gamma dk dθ

-- | Normal(μ, σ)
--   @μ@ mean, @σ@ standard deviation
data Normal = Normal Double Double
  deriving Show

instance Distribution Normal where
  type Support Normal = Double

  sampleInv :: Normal -> Double -> Double
  sampleInv (Normal μ σ) r
    | r == 0         = m_neg_inf
    | r == 1         = m_pos_inf
    | r == 0.5       = μ
    | r > 0 && r < 1 = (- invErfc (2 * r)) * (m_sqrt_2 * σ) + μ
    | otherwise      = error $ "Normal: r must be in [0,1] range. Got: " ++ show r

  sample :: Normal -> Sampler Double
  sample (Normal μ σ) = sampleNormal μ σ

  logProbRaw :: Normal -> Double -> Double
  logProbRaw (Normal μ σ) x
    | σ <= 0     = error "normalLogPdfRaw: σ <= 0"
    | otherwise  = -(xμ * xμ / (2 * (σ ** 2))) - log m_sqrt_2_pi - log σ
    where xμ = x - μ

  isDifferentiable :: Normal -> Maybe (Witness DiffDistribution Normal)
  isDifferentiable _ = Just Witness

instance DiffDistribution Normal where
  gradLogProb :: Normal -> Double -> Normal
  gradLogProb (Normal μ σ) x
    | σ <= 0      = error "normalGradLogPdfRaw: σ <= 0"
    | otherwise   = Normal dμ dσ
    where xμ = x - μ
          dμ = xμ/(σ ** 2)
          dσ = -1/σ + (xμ**2)/(σ ** 3)
          dx = -dμ

  safeAddGrad :: Normal -> Normal -> Normal
  safeAddGrad (Normal μ σ) (Normal dμ dσ) = Normal μ' σ'
    where μ' = μ + dμ
          σ' = let σ_new = σ + dσ in if σ_new <= 0 then σ else σ_new

  zeroGrad :: Normal
  zeroGrad = Normal 0 0

  toList :: Normal -> [Double]
  toList (Normal dμ dσ) = [dμ, dσ]

  fromList [dμ, dσ] = Normal dμ dσ

-- | HalfNormal(σ)
--   @σ@ standard deviation
newtype HalfNormal = HalfNormal Double
  deriving Show

instance Distribution HalfNormal where
  type Support HalfNormal = Double

  sampleInv :: HalfNormal -> Double -> Double
  sampleInv (HalfNormal σ) = abs . sampleInv (Normal 0 σ)

  sample :: HalfNormal -> Sampler Double
  sample (HalfNormal σ) = sample (Normal 0 σ) <&> abs

  logProbRaw :: HalfNormal -> Double -> Double
  logProbRaw (HalfNormal σ) x
    | x < 0     = m_neg_inf
    | otherwise = log 2 + logProbRaw (Normal 0 σ) x

  isDifferentiable :: HalfNormal -> Maybe (Witness DiffDistribution HalfNormal)
  isDifferentiable _ = Just Witness

instance DiffDistribution HalfNormal where
  gradLogProb :: HalfNormal -> Double -> HalfNormal
  gradLogProb (HalfNormal σ) x
    | x < 0         = error "halfNormalGradLogPdfRaw: No gradient at x < 0"
    | σ <= 0        = error "halfNormalGradLogPdfRaw: σ <= 0"
    | otherwise     = let Normal _ dσ = gradLogProb (Normal 0 σ) x in HalfNormal dσ

  safeAddGrad :: HalfNormal -> HalfNormal -> HalfNormal
  safeAddGrad (HalfNormal σ) (HalfNormal dσ) = HalfNormal σ'
    where σ' = let σ_new = σ + dσ in if σ_new <= 0 then σ else σ_new

  zeroGrad :: HalfNormal
  zeroGrad = HalfNormal 0

  toList :: HalfNormal -> [Double]
  toList (HalfNormal σ) = [σ]

  fromList [σ] = HalfNormal σ

{-# INLINE invCMF #-}
invCMF
  :: (Int -> Double)  -- ^ probability mass function
  -> Double           -- ^ r
  -> Int
invCMF pmf r = f 0 r
  where
    f :: Int -> Double -> Int
    f i r = do
      let q  = pmf i
          r' = r - q
      if r' < 0 then i else f (i + 1) r'

-- | Bernoulli(p)
--   @p@ probability of success
newtype Bernoulli = Bernoulli Double
  deriving Show

instance Distribution Bernoulli where
  type Support Bernoulli = Bool

  sampleInv :: Bernoulli -> Double -> Bool
  sampleInv (Bernoulli p) r = r < p

  sample :: Bernoulli -> Sampler Bool
  sample (Bernoulli p) = sampleBernoulli p

  logProbRaw :: Bernoulli -> Bool -> Double
  logProbRaw (Bernoulli p) y
    | y         = log p
    | otherwise = log (1 - p)

  isDifferentiable _ = Nothing

bernoulliGradLogPdfRaw :: Bernoulli -> Bool -> Bernoulli
bernoulliGradLogPdfRaw (Bernoulli p) y = Bernoulli dp
  where dp = 1/p - fromIntegral (boolToInt y)/(1 - p)

bernoulliAddGrad :: Bernoulli -> Bernoulli -> Bernoulli
bernoulliAddGrad (Bernoulli p) (Bernoulli dp) = Bernoulli dp
  where p' = max (min (p + dp) 1) 0

-- | Binomial(n, p)
-- | @n@ number of trials, @p@ probability of success
data Binomial = Binomial Int Double
  deriving Show

instance Distribution Binomial where
  type Support Binomial = Int

  sampleInv :: Binomial -> Double -> Int
  sampleInv (Binomial n p) = invCMF (prob (Binomial n p))

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

  isDifferentiable _ = Nothing

binomialGradLogPdfRaw :: Int -> Double -> Int -> (Int, Double, Int)
binomialGradLogPdfRaw n p y
  | y < 0 || y > n          = error "binomialGradLogPdfRaw: y < 0 || y > n"
  | n < 0                   = error "binomialGradLogPdfRaw: n < 0"
  | otherwise               = (dn, dp, dy)
  where dn = 0
        dp = fromIntegral n/p - fromIntegral (n - y)/(1 - p)
        dy = 0

-- | Categorical(ps)
--   @ps@ probabilities of each category
newtype Categorical = Categorical [Double]
  deriving Show

instance Distribution Categorical where
  type Support Categorical = Int            -- ^ an index from @0@ to @n - 1@

  sampleInv :: Categorical -> Double -> Int
  sampleInv (Categorical ps) = invCMF (ps !!)

  sample :: Categorical -> Sampler Int
  sample (Categorical ps) = sampleCategorical (Vector.fromList ps)

  logProbRaw :: Categorical -> Int -> Double
  logProbRaw (Categorical ps) idx
    | idx < 0 || idx >= length ps = trace "CategoricalLogPdf: idx < 0 || idx >= length ps" m_neg_inf
    | otherwise                   = log (ps !! idx)

  isDifferentiable _ = Nothing

-- | Deterministic(x)
data Deterministic a where
  Deterministic
    :: (Show a, Typeable a, Eq a)
    => a                                  -- ^ value of probability @1@
    -> Deterministic a

instance Show a => Show (Deterministic a) where
  show (Deterministic x) = "Deterministic " ++ show x

instance (Show a, Typeable a) => Distribution (Deterministic a) where
  type Support (Deterministic a) = a

  sampleInv :: Deterministic a -> Double -> a
  sampleInv (Deterministic x) _ = x

  sample :: Deterministic a -> Sampler a
  sample (Deterministic x) = pure x

  logProbRaw :: Deterministic a -> a -> Double
  logProbRaw (Deterministic x) y
    | x == y    = 0
    | otherwise = m_neg_inf

  isDifferentiable _ = Nothing

-- | Discrete(xps)
--   @xps@ values `x` and associated probabilities `p`
data Discrete a where
  Discrete
    :: (Show a, Typeable a, Eq a)
    => [(a, Double)]
    -> Discrete a

instance Show a => Show (Discrete a) where
  show (Discrete xps) = "Deterministic " ++ show xps

instance (Show a, Typeable a) => Distribution (Discrete a) where
  type Support (Discrete a) = a

  sampleInv :: Discrete a -> Double -> a
  sampleInv (Discrete xps) r = xs !! invCMF (ps !!) r
    where (xs, ps) = unzip xps

  sample :: Discrete a -> Sampler a
  sample (Discrete xps) = sampleDiscrete xps

  logProbRaw :: Discrete a -> a -> Double
  logProbRaw (Discrete xps) y =
    case lookup y xps of
      Nothing -> trace ("Couldn't find " ++ show y ++ " in Discrete dist") m_neg_inf
      Just p  -> log p

  isDifferentiable _ = Nothing

-- | Poisson(λ)
--   @λ@ rate
newtype Poisson = Poisson Double
  deriving Show

instance Distribution Poisson where
  type Support Poisson = Int

  sampleInv :: Poisson -> Double -> Int
  sampleInv (Poisson λ) = invCMF (prob (Poisson λ))

  sample :: Poisson -> Sampler Int
  sample (Poisson λ) = samplePoisson λ

  logProbRaw :: Poisson -> Int -> Double
  logProbRaw (Poisson λ) y
    | λ < 0     = error "poissonLogPdfRaw:  λ < 0 "
    | y < 0     = trace "poissonLogPdfRaw:  y < 0 " m_neg_inf
    | otherwise = log λ * fromIntegral y - logFactorial y - λ

  isDifferentiable _ = Nothing

poissonGradLogPdfRaw :: Double -> Int -> Double
poissonGradLogPdfRaw λ y
  | λ < 0     = error "poissonGradLogPdfRaw:  λ < 0 "
  | y < 0     = error "poissonGradLogPdfRaw:  y < 0 "
  | otherwise = (fromIntegral y/λ) - 1

-- | ContinuousUniform(min, max)
--   @min@ lower-bound, @max@ upper-bound
data Uniform = Uniform Double Double
  deriving Show

instance Distribution Uniform where
  type Support Uniform = Double

  sampleInv :: Uniform -> Double -> Double
  sampleInv (Uniform min max) r
    | r >= 0 && r <= 1 = min + (max - min) * r
    | otherwise        = error $ "Uniform: r must be in [0,1] range. Got: " ++ show r

  sample :: Uniform -> Sampler Double
  sample (Uniform min max) = sampleUniform min max

  logProbRaw :: Uniform -> Double -> Double
  logProbRaw (Uniform min max) x
    | max <  min         = error "uniformLogPdfRaw: max < min"
    | x < min || x > max = m_neg_inf
    | otherwise          = -log(max - min)

  isDifferentiable _ = Nothing

-- | DiscreteUniform(min, max)
--   @min@ lower-bound, @max@ upper-bound
data UniformD = UniformD Int Int
  deriving Show

instance Distribution UniformD where
  type Support UniformD = Int

  sampleInv :: UniformD -> Double -> Int
  sampleInv (UniformD min max) r
     | r >= 0 && r <= 1 = floor (min' + (max' - min') * r)
     | otherwise        = error $ "UniformD: r must be in [0,1] range. Got: " ++ show r
     where min' = fromIntegral min
           max' = fromIntegral max + 1

  sample :: UniformD -> Sampler Int
  sample (UniformD min max)    = sampleUniformD min max

  logProbRaw :: UniformD -> Int -> Double
  logProbRaw (UniformD min max) idx
    | max <  min              = error "uniformDLogPdfRaw: max < min"
    | idx < min || idx > max  = m_neg_inf
    | otherwise               = - log (fromIntegral $ max - min + 1)

  isDifferentiable _ = Nothing

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