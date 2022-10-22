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

module PrimDist
  (Distribution(..), DiffDistribution(..), PrimDist, Witness(..),
   Beta, mkBeta, Bernoulli, mkBernoulli, Binomial, mkBinomial, Categorical, mkCategorical, Cauchy, mkCauchy, HalfCauchy, mkHalfCauchy,
   Deterministic, mkDeterministic, Discrete, mkDiscrete, Dirichlet, mkDirichlet, Gamma, mkGamma, Normal, mkNormal, HalfNormal, mkHalfNormal, Poisson, mkPoisson, Uniform, mkUniform, UniformD, mkUniformD) where

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

  liftUnOp :: (Double -> Double) -> d -> d

  liftBinOp :: (Double -> Double -> Double) -> d -> d -> d

  zero :: d

  toList :: d -> [Double]

  fromList :: [Double] -> d

  (|+|) :: d -> d -> d
  (|+|) = liftBinOp (+)

  (|-|) :: d -> d -> d
  (|-|) = liftBinOp (-)

  (|*|) :: d -> d -> d
  (|*|) = liftBinOp (*)

  (|/|) :: d -> d -> d
  (|/|) = liftBinOp (/)

  (*|) :: Double -> d -> d
  (*|) x = liftUnOp (* x)

  (/|) :: d -> Double -> d
  (/|) d x = liftUnOp (/ x) d

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

mkBeta :: Double -> Double -> Beta
mkBeta α β
  | α > 0 && β > 0 = Beta α β
  | otherwise      = error "Beta: α > 0 or β > 0 not met"

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
    | x <= 0 || x >= 1 = m_neg_inf
    | otherwise = (α-1)*log x + (β-1)*log1p (-x) - logBeta α β

  isDifferentiable :: Beta -> Maybe (Witness DiffDistribution Beta)
  isDifferentiable _ = Just Witness

instance DiffDistribution Beta where
  gradLogProb :: Beta -> Double -> Beta
  gradLogProb (Beta α β) x
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

  liftUnOp :: (Double -> Double) -> Beta -> Beta
  liftUnOp f (Beta α β) = Beta (f α) (f β)

  liftBinOp :: (Double -> Double -> Double) -> Beta -> Beta -> Beta
  liftBinOp f (Beta α β) (Beta dα dβ) = Beta (f α dα) (f β dβ)

  zero :: Beta
  zero = Beta 0 0

  toList :: Beta -> [Double]
  toList (Beta dα dβ) = [dα, dβ]

  fromList :: [Double] -> Beta
  fromList [dα, dβ] = (Beta dα dβ)

-- | Cauchy(location, scale)
data Cauchy = Cauchy Double Double
  deriving Show

mkCauchy ::  Double -> Double -> Cauchy
mkCauchy loc scale
  | scale > 0 = Cauchy loc scale
  | otherwise = error "Cauchy: scale > 0 not met"

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
  logProbRaw (Cauchy loc scale) x = -(log pi) + log scale - log (xloc**2 + scale**2)
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

  liftUnOp :: (Double -> Double) -> Cauchy -> Cauchy
  liftUnOp f (Cauchy loc scale) = Cauchy (f loc) (f scale)

  liftBinOp :: (Double -> Double -> Double) -> Cauchy -> Cauchy -> Cauchy
  liftBinOp f (Cauchy loc scale) (Cauchy dloc dscale) = Cauchy (f loc dloc) (f scale dscale)

  zero :: Cauchy
  zero = Cauchy 0 0

  toList :: Cauchy -> [Double]
  toList (Cauchy loc scale) = [loc, scale]

  fromList :: [Double] -> Cauchy
  fromList [loc, scale] = (Cauchy loc scale)

-- | HalfCauchy(scale)
newtype HalfCauchy = HalfCauchy Double
  deriving Show

mkHalfCauchy :: Double -> HalfCauchy
mkHalfCauchy scale
  | scale > 0 = HalfCauchy scale
  | otherwise = error "HalfCauchy: scale > 0 not met"

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
    | x < 0      = error "cauchyGradLogPdfRaw: x < 0"
    | otherwise  = let (Cauchy _ dscale) = gradLogProb (Cauchy 0 scale) x in HalfCauchy dscale

  safeAddGrad :: HalfCauchy -> HalfCauchy -> HalfCauchy
  safeAddGrad (HalfCauchy scale) (HalfCauchy dscale) = HalfCauchy scale'
    where scale' = let new_scale = scale + dscale in if new_scale <= 0 then scale else new_scale

  liftUnOp :: (Double -> Double) -> HalfCauchy -> HalfCauchy
  liftUnOp f (HalfCauchy scale) = HalfCauchy (f scale)

  liftBinOp :: (Double -> Double -> Double) -> HalfCauchy -> HalfCauchy -> HalfCauchy
  liftBinOp f (HalfCauchy scale) (HalfCauchy dscale) = HalfCauchy (f scale dscale)

  zero :: HalfCauchy
  zero = HalfCauchy 0

  toList :: HalfCauchy -> [Double]
  toList (HalfCauchy scale) = [scale]

  fromList :: [Double] -> HalfCauchy
  fromList [scale] = (HalfCauchy scale)

-- | Dirichlet(αs)
--   @αs@ concentrations
newtype Dirichlet = Dirichlet [Double]
  deriving Show

mkDirichlet :: [Double] -> Dirichlet
mkDirichlet αs
  | any (<= 0) αs = error "Dirichlet: αs > 0 not met"
  | length αs < 2 = error "Dirichlet: length αs >= 2 not met"
  | otherwise     = Dirichlet αs

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
    | any (<  0) xs              = error "dirichletGradLogPdfRaw: data should be non-negative"
    | otherwise = Dirichlet (zipWith derivA αs xs)
    where derivA a x  = -(digamma a) + digamma (sum αs) + log x
          derivX a x = (a - 1) / x

  safeAddGrad :: Dirichlet -> Dirichlet -> Dirichlet
  safeAddGrad (Dirichlet αs) (Dirichlet dαs) = Dirichlet (zipWith add_dα αs dαs)
    where add_dα α dα = let α_new = α + dα in if α_new <= 0 then α else α_new

  liftUnOp :: (Double -> Double) -> Dirichlet -> Dirichlet
  liftUnOp f (Dirichlet αs) = Dirichlet (map f αs)

  liftBinOp :: (Double -> Double -> Double) -> Dirichlet -> Dirichlet -> Dirichlet
  liftBinOp f (Dirichlet αs) (Dirichlet dαs) = Dirichlet (zipWith f αs dαs)

  zero :: Dirichlet
  zero = Dirichlet (repeat 0)

  toList :: Dirichlet -> [Double]
  toList (Dirichlet dαs) = dαs

  fromList :: [Double] -> Dirichlet
  fromList dαs = Dirichlet dαs

-- | Gamma(k, θ)
--   @k@ shape, @θ@ scale
data Gamma = Gamma Double Double
  deriving Show

mkGamma :: Double -> Double -> Gamma
mkGamma k θ
  | k > 0 && θ > 0 = Gamma k θ
  | otherwise      = error "Gamma: k > 0 && θ > 0 not met"

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
    | x <= 0    = m_neg_inf
    | otherwise = (k - 1) * log x - (x/θ) - logGamma k - (k * log θ)

  isDifferentiable :: Gamma -> Maybe (Witness DiffDistribution Gamma)
  isDifferentiable _ = Just Witness

instance DiffDistribution Gamma where
  gradLogProb :: Gamma -> Double -> Gamma
  gradLogProb (Gamma k θ) x
    | x <= 0           = error "gammaGradLogPdfRaw: x <= 0"
    | otherwise = Gamma dk dθ
    where dk = log x - digamma k - log θ
          dθ = x/(θ**2) - k/θ
          dx = (k - 1)/x - 1/θ

  safeAddGrad :: Gamma -> Gamma -> Gamma
  safeAddGrad (Gamma k θ) (Gamma dk dθ) = Gamma k' θ'
    where k' = let k_new = k + dk in if k_new <= 0 then k else k_new
          θ' = let θ_new = θ + dθ in if θ_new <= 0 then θ else θ_new

  liftUnOp :: (Double -> Double) -> Gamma -> Gamma
  liftUnOp f (Gamma k θ) = Gamma (f k) (f θ)

  liftBinOp :: (Double -> Double -> Double) -> Gamma -> Gamma -> Gamma
  liftBinOp f (Gamma k θ) (Gamma dk dθ) = Gamma (f k dk) (f θ dθ)

  zero :: Gamma
  zero = Gamma 0 0

  toList :: Gamma -> [Double]
  toList (Gamma dk dθ) = [dk, dθ]

  fromList :: [Double] -> Gamma
  fromList [dk, dθ] = Gamma dk dθ

-- | Normal(μ, σ)
--   @μ@ mean, @σ@ standard deviation
data Normal = Normal Double Double
  deriving Show

mkNormal :: Double -> Double -> Normal
mkNormal μ σ
  | σ > 0 = Normal μ σ
  | otherwise = error "Normal: σ > 0 not met"

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
  logProbRaw (Normal μ σ) x = -(xμ * xμ / (2 * (σ ** 2))) - log m_sqrt_2_pi - log σ
    where xμ = x - μ

  isDifferentiable :: Normal -> Maybe (Witness DiffDistribution Normal)
  isDifferentiable _ = Just Witness

instance DiffDistribution Normal where
  gradLogProb :: Normal -> Double -> Normal
  gradLogProb (Normal μ σ) x = Normal dμ dσ
    where xμ = x - μ
          dμ = xμ/(σ ** 2)
          dσ = -1/σ + (xμ**2)/(σ ** 3)
          dx = -dμ

  safeAddGrad :: Normal -> Normal -> Normal
  safeAddGrad (Normal μ σ) (Normal dμ dσ) = Normal μ' σ'
    where μ' = μ + dμ
          σ' = let σ_new = σ + dσ in if σ_new <= 0 then σ else σ_new

  liftUnOp :: (Double -> Double) -> Normal -> Normal
  liftUnOp f (Normal μ σ) = Normal (f μ) (f σ)

  liftBinOp :: (Double -> Double -> Double) -> Normal -> Normal -> Normal
  liftBinOp f (Normal μ σ) (Normal dμ dσ) = Normal (f μ dμ) (f σ dσ)

  zero :: Normal
  zero = Normal 0 0

  toList :: Normal -> [Double]
  toList (Normal dμ dσ) = [dμ, dσ]

  fromList :: [Double] -> Normal
  fromList [dμ, dσ] = Normal dμ dσ

-- | HalfNormal(σ)
--   @σ@ standard deviation
newtype HalfNormal = HalfNormal Double
  deriving Show

mkHalfNormal :: Double -> HalfNormal
mkHalfNormal σ
  | σ > 0     = HalfNormal σ
  | otherwise = error "HalfNormal: σ > 0 not met"

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
    | otherwise     = let Normal _ dσ = gradLogProb (Normal 0 σ) x in HalfNormal dσ

  safeAddGrad :: HalfNormal -> HalfNormal -> HalfNormal
  safeAddGrad (HalfNormal σ) (HalfNormal dσ) = HalfNormal σ'
    where σ' = let σ_new = σ + dσ in if σ_new <= 0 then σ else σ_new

  liftUnOp :: (Double -> Double) -> HalfNormal -> HalfNormal
  liftUnOp f (HalfNormal σ) = HalfNormal (f σ)

  liftBinOp :: (Double -> Double -> Double) -> HalfNormal -> HalfNormal -> HalfNormal
  liftBinOp f (HalfNormal σ) (HalfNormal dσ) = HalfNormal (f σ dσ)

  zero :: HalfNormal
  zero = HalfNormal 0

  toList :: HalfNormal -> [Double]
  toList (HalfNormal σ) = [σ]

  fromList :: [Double] -> HalfNormal
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

mkBernoulli :: Double -> Bernoulli
mkBernoulli p
  | p >= 0 && p <= 1 = Bernoulli p
  | otherwise        = error "Bernoulli: p >= 0 && p <= 1 not met"

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

mkBinomial :: Int -> Double -> Binomial
mkBinomial n p
  | p < 0 || p > 1 = error "Binomial: p >= 0 && p <= 1 not met"
  | n < 0          = error "Binomial: n >= 0 not met"
  | otherwise      = Binomial n p

instance Distribution Binomial where
  type Support Binomial = Int

  sampleInv :: Binomial -> Double -> Int
  sampleInv (Binomial n p) = invCMF (prob (Binomial n p))

  sample :: Binomial -> Sampler Int
  sample (Binomial n p) = sampleBinomial n p

  logProbRaw :: Binomial -> Int -> Double
  logProbRaw (Binomial n p) y
    | y < 0 || y > n          = m_neg_inf
    | otherwise               = logChoose n y + log p * y' + log1p (-p) * ny'
    where
      y'  = fromIntegral   y
      ny' = fromIntegral $ n - y

  isDifferentiable _ = Nothing

binomialGradLogPdfRaw :: Int -> Double -> Int -> (Int, Double, Int)
binomialGradLogPdfRaw n p y
  | y < 0 || y > n          = error "binomialGradLogPdfRaw: y < 0 || y > n"
  | otherwise               = (dn, dp, dy)
  where dn = 0
        dp = fromIntegral n/p - fromIntegral (n - y)/(1 - p)
        dy = 0

-- | Categorical(ps)
--   @ps@ probabilities of each category
newtype Categorical = Categorical [Double]
  deriving Show

mkCategorical :: [Double] -> Categorical
mkCategorical ps
  | any (< 0) ps = error "Categorical: ps >= 0 not met"
  | null ps      = error "Categorical: ps must be non-empty"
  | otherwise    = Categorical ps

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

mkDeterministic :: (Show a, Typeable a, Eq a) => a -> Deterministic a
mkDeterministic x = Deterministic x

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

mkDiscrete :: (Show a, Typeable a, Eq a) => [(a, Double)] -> Discrete a
mkDiscrete xps
  | null xps = error "Discrete: xps must be non-empty"
  | any ((< 0) . snd) xps = error "Discrete: probabilities must be >= 0"
  | otherwise = Discrete xps

instance Show a => Show (Discrete a) where
  show (Discrete xps) = "Discrete " ++ show xps

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

mkPoisson :: Double -> Poisson
mkPoisson λ
  | λ < 0     = error "Poisson:  λ >= 0 not met"
  | otherwise = Poisson λ

instance Distribution Poisson where
  type Support Poisson = Int

  sampleInv :: Poisson -> Double -> Int
  sampleInv (Poisson λ) = invCMF (prob (Poisson λ))

  sample :: Poisson -> Sampler Int
  sample (Poisson λ) = samplePoisson λ

  logProbRaw :: Poisson -> Int -> Double
  logProbRaw (Poisson λ) y
    | y < 0     = trace "poissonLogPdfRaw:  y < 0 " m_neg_inf
    | otherwise = log λ * fromIntegral y - logFactorial y - λ

  isDifferentiable _ = Nothing

poissonGradLogPdfRaw :: Double -> Int -> Double
poissonGradLogPdfRaw λ y
  | y < 0     = error "poissonGradLogPdfRaw:  y < 0 "
  | otherwise = (fromIntegral y/λ) - 1

-- | ContinuousUniform(min, max)
--   @min@ lower-bound, @max@ upper-bound
data Uniform = Uniform Double Double
  deriving Show

mkUniform :: Double -> Double -> Uniform
mkUniform min max
  | min > max = error "Uniform: min <= max not met"
  | otherwise = Uniform min max

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
    | x < min || x > max = m_neg_inf
    | otherwise          = -log(max - min)

  isDifferentiable _ = Nothing

-- | DiscreteUniform(min, max)
--   @min@ lower-bound, @max@ upper-bound
data UniformD = UniformD Int Int
  deriving Show

mkUniformD :: Int -> Int -> UniformD
mkUniformD min max
  | min > max = error "UniformD: min <= max not met"
  | otherwise = UniformD min max

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