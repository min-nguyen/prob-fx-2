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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstrainedClassMethods #-}

{- | A GADT encoding of (a selection of) primitive distributions
    along with their corresponding sampling and density functions.
-}

module PrimDist
  (Distribution(..), DiffDistribution(..), PrimDist, Witness(..), (|+|), (|-|), (|/|), (|*|), (*|), covarGrad, varGrad,
   Beta, mkBeta, Bernoulli, mkBernoulli, Binomial, mkBinomial, Categorical, mkCategorical, Cauchy, mkCauchy, HalfCauchy, mkHalfCauchy,
   Deterministic, mkDeterministic, Discrete, mkDiscrete, Dirichlet, mkDirichlet, Gamma, mkGamma, Normal, mkNormal, HalfNormal, mkHalfNormal,
   Poisson, mkPoisson, Uniform, mkUniform, UniformD, mkUniformD) where

import           Debug.Trace ( trace )
import Data.Proxy
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
import           OpenSum (OpenSum)
import qualified OpenSum
import qualified Data.Vector as Vector
import           Data.Vector (Vector)
import           Sampler
import           LogP ( LogP(..) )
import           Util ( linCongGen, boolToInt, mean, covariance, variance )
import qualified Vec
import           Vec (Vec(..), TyNat)

{- import qualified Control.Monad.Bayes.Class as MB
   import           Numeric.Log ( Log(..) )
-}

{- Distributions that can be sampled from and conditioned against.
-}
class (Show d, Typeable d) => Distribution d where
  type family Support d :: Type
  {- | Given a random double @r@ in (0, 1), this is passed to a distribution's inverse
       cumulative density function to draw a sampled value. -}
  sampleInv   :: d -> Double -> Support d

  {- | Draw a value from a primitive distribution in the @Sampler@ monad. -}
  sample      :: d -> Sampler (Support d)

  {- | Compute the log density of a primitive distribution generating an observed value. -}
  logProbRaw  :: d -> Support d -> Double

  {- | Compute the log density as the @LogP@ type. -}
  logProb     :: d -> Support d -> LogP
  logProb d   = LogP . logProbRaw d

  {- | Compute the probability density. -}
  prob        :: d -> Support d -> Double
  prob d      = exp . logProbRaw d

  {- | Provide proof that @d@ is differentiable. -}
  isDifferentiable :: d -> Maybe (Witness DiffDistribution d)

-- | Shorthand for specifying a distribution @d@ and its type of support @a@
type PrimDist d a = (Distribution d, Support d ~ a)

-- | Dictionary proof
data Witness (c :: Type -> Constraint) a where
  Witness :: c a => Witness c a

{- Distributions that can be differentiated with respect to their parameters
-}
class Distribution d => DiffDistribution d where
  type family Arity d :: Nat
  {- | Compute the gradient log-probability. -}
  gradLogProb :: d -> Support d -> Vec (Arity d) Double

  liftUnOp  :: (Double -> Double) -> d -> d

  liftBinOp :: (Double -> Double -> Double) -> d -> d -> d

  zero      :: Proxy d -> Vec (Arity d) Double

  toList :: d -> [Double]

  fromList :: [Double] -> d

  safeAddGrad :: d -> Vec (Arity d) Double -> d

covarGrad ::            -- Assuming our distribution has D parameters
     [Vec n Double]     -- ^ (f^{1:D})^{1:L}, an L-sized list of D-arity parameter sets  [(D α^1 β^1), (D α^2 β^2), .. (D α^L β^L)]
  -> [Vec n Double]     -- ^ (g^{1:D})^{1:L}, an L-sized list of D-arity parameter sets
  -> Vec n Double       -- ^ covar((f^{1:D})^{1:L}, (g^{1:D})^{1:L})
covarGrad fs gs = Vec.zipWith Util.covariance params_fs params_gs
  where params_fs = Vec.transpose fs -- D-sized vector of L-sized lists    [[α^1, α^2, .. α^L], [β^1, β^2, .. β^L]]
        params_gs = Vec.transpose gs -- D-sized vector of L-sized lists    [[α^1, α^2, .. α^L], [β^1, β^2, .. β^L]]

varGrad ::            -- Assuming our distribution has D parameters
    [Vec n Double]    -- ^ (g^{1:D})^{1:L}, an L-sized list of D-arity parameter sets  [[α^1, β^1], [α^2, β^2], .. [α^L, β^L]]
  -> Vec n Double     -- ^ var((g^{1:D})^{1:L})
varGrad gs = Vec.map Util.variance params_gs
  where params_gs = Vec.transpose gs -- D-sized vector of L-sized lists    [[α^1, α^2, .. α^L], [β^1, β^2, .. β^L]]

(|+|) :: Vec n Double -> Vec n Double -> Vec n Double
(|+|) = Vec.zipWith (+)

(|-|) :: Vec n Double -> Vec n Double -> Vec n Double
(|-|) = Vec.zipWith (-)

(|*|) :: Vec n Double -> Vec n Double -> Vec n Double
(|*|) = Vec.zipWith (*)

(|/|) :: Vec n Double -> Vec n Double -> Vec n Double
(|/|) = Vec.zipWith (/)

(*|) :: Double -> Vec n Double -> Vec n Double
(*|) x = Vec.map (* x)


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
  sampleInv (Beta α β) = invIncompleteBeta α β

  sample :: Beta -> Sampler Double
  sample (Beta α β) = Sampler.sampleBeta α β

  logProbRaw :: Beta -> Double -> Double
  logProbRaw (Beta α β) x
    | x <= 0 || x >= 1 = m_neg_inf
    | otherwise = (α-1)*log x + (β-1)*log1p (-x) - logBeta α β

  isDifferentiable :: Beta -> Maybe (Witness DiffDistribution Beta)
  isDifferentiable _ = Just Witness

instance DiffDistribution Beta where
  type Arity Beta = Nat2

  gradLogProb :: Beta -> Double -> Vec Nat2 Double
  gradLogProb (Beta α β) x
    | x <= 0 || x >= 1 = error "betaGradLogPdfRaw: x <= 0 || x >= 1"
    | otherwise = da ::: db ::: VNil
    where digamma_ab = digamma (α + β)
          da = log x       - digamma α + digamma_ab
          db = log (1 - x) - digamma β + digamma_ab
          dx = (α - 1)/x + (β - 1)/(1 - x)

  safeAddGrad (Beta α β) (dα ::: dβ ::: VNil) = Beta α' β'
    where α' = let α_new = α + dα in if α_new <= 0 then α else α_new
          β' = let β_new = β + dβ in if β_new <= 0 then β else β_new

  liftUnOp :: (Double -> Double) -> Beta -> Beta
  liftUnOp f (Beta α β) = Beta (f α) (f β)

  liftBinOp :: (Double -> Double -> Double) -> Beta -> Beta -> Beta
  liftBinOp f (Beta α β) (Beta dα dβ) = Beta (f α dα) (f β dβ)

  zero _ = 0 ::: 0 ::: VNil

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
  sampleInv (Cauchy loc scale) r = loc + scale * tan( pi * (r - 0.5) )

  sample :: Cauchy -> Sampler Double
  sample (Cauchy loc scale) = Sampler.sampleCauchy loc scale

  logProbRaw :: Cauchy -> Double -> Double
  logProbRaw (Cauchy loc scale) x = -(log pi) + log scale - log (xloc**2 + scale**2)
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

  liftUnOp :: (Double -> Double) -> Cauchy -> Cauchy
  liftUnOp f (Cauchy loc scale) = Cauchy (f loc) (f scale)

  liftBinOp :: (Double -> Double -> Double) -> Cauchy -> Cauchy -> Cauchy
  liftBinOp f (Cauchy loc scale) (Cauchy dloc dscale) = Cauchy (f loc dloc) (f scale dscale)

  zero _ = 0 ::: 0 ::: VNil

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
  sample (HalfCauchy scale) = abs <$> sample (Cauchy 0 scale)

  logProbRaw :: HalfCauchy -> Double -> Double
  logProbRaw (HalfCauchy scale) x
    | x < 0     = m_neg_inf
    | otherwise = log 2 + logProbRaw (Cauchy 0 scale) x

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

  liftUnOp :: (Double -> Double) -> HalfCauchy -> HalfCauchy
  liftUnOp f (HalfCauchy scale) = HalfCauchy (f scale)

  liftBinOp :: (Double -> Double -> Double) -> HalfCauchy -> HalfCauchy -> HalfCauchy
  liftBinOp f (HalfCauchy scale) (HalfCauchy dscale) = HalfCauchy (f scale dscale)

  zero  _ = 0 ::: VNil


  toList :: HalfCauchy -> [Double]
  toList (HalfCauchy scale) = [scale]

  fromList :: [Double] -> HalfCauchy
  fromList [scale] = (HalfCauchy scale)

-- | Dirichlet(αs)
--   @αs@ concentrations
newtype Dirichlet (n :: Nat) = Dirichlet (Vec n Double)
  deriving Show

mkDirichlet :: Vec n Double -> Dirichlet n
mkDirichlet αs
  | any (<= 0) αs = error "Dirichlet: αs > 0 not met"
  | length αs < 2 = error "Dirichlet: length αs >= 2 not met"
  | otherwise     = Dirichlet αs

instance (TyNat n) => Distribution (Dirichlet n) where
  type Support (Dirichlet n) = Vec n Double

  sampleInv :: Dirichlet n -> Double -> Vec n Double
  sampleInv (Dirichlet αs) r =
    let rs = linCongGen r snat
        xs = Vec.zipWith (\α r -> sampleInv (Gamma α 1) r) αs rs
    in  Vec.map (/sum xs) xs

  sample :: Dirichlet n -> Sampler (Vec n Double)
  sample (Dirichlet αs) = fromJust . Vec.fromList <$> Sampler.sampleDirichlet (Vec.toList αs)

  logProbRaw :: Dirichlet n -> Vec n Double -> Double
  logProbRaw (Dirichlet αs) xs
    | length xs /= length αs     = trace "dirichletLogPdfRaw: length xs /= length αs" m_neg_inf
    | abs (sum xs - 1.0) > 1e-14 = trace "dirichletLogPdfRaw: data should sum to 1" m_neg_inf
    | any (<  0) xs              = trace "dirichletLogPdfRaw: data should be non-negative" m_neg_inf
    | otherwise = c + sum (Vec.zipWith (\a x -> (a - 1) * log x) αs xs)
    where c = - sum (Vec.map logGamma αs) + logGamma (sum αs)

  isDifferentiable :: Dirichlet n -> Maybe (Witness DiffDistribution (Dirichlet n))
  isDifferentiable _ = Just Witness

instance (TyNat n) => DiffDistribution (Dirichlet n) where
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
    where add_dα α dα = let α_new = α + dα in if α_new <= 0 then α else α_new

  liftUnOp :: (Double -> Double) -> Dirichlet n -> Dirichlet n
  liftUnOp f (Dirichlet αs) = Dirichlet (Vec.map f αs)

  liftBinOp :: (Double -> Double -> Double) -> Dirichlet n -> Dirichlet n -> Dirichlet n
  liftBinOp f (Dirichlet αs) (Dirichlet dαs) = Dirichlet (Vec.zipWith f αs dαs)

  zero _ = (Vec.replicate (snat @n) 0)

  toList :: Dirichlet n -> [Double]
  toList (Dirichlet dαs) = Vec.toList dαs

  fromList :: [Double] -> Dirichlet n
  fromList dαs = Dirichlet (fromJust $ Vec.fromList dαs)

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
  sampleInv (Gamma k θ) r = θ * invIncompleteGamma k r

  sample :: Gamma -> Sampler Double
  sample (Gamma k θ) = Sampler.sampleGamma k θ

  logProbRaw :: Gamma -> Double -> Double
  logProbRaw (Gamma k θ) x
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

  liftUnOp :: (Double -> Double) -> Gamma -> Gamma
  liftUnOp f (Gamma k θ) = Gamma (f k) (f θ)

  liftBinOp :: (Double -> Double -> Double) -> Gamma -> Gamma -> Gamma
  liftBinOp f (Gamma k θ) (Gamma dk dθ) = Gamma (f k dk) (f θ dθ)

  zero  _ = 0 ::: 0 ::: VNil


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
  sampleInv (Normal μ σ) r = (- invErfc (2 * r)) * (m_sqrt_2 * σ) + μ

  sample :: Normal -> Sampler Double
  sample (Normal μ σ) = Sampler.sampleNormal μ σ

  logProbRaw :: Normal -> Double -> Double
  logProbRaw (Normal μ σ) x = -(xμ * xμ / (2 * (σ ** 2))) - log m_sqrt_2_pi - log σ
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

  liftUnOp :: (Double -> Double) -> Normal -> Normal
  liftUnOp f (Normal μ σ) = Normal (f μ) (f σ)

  liftBinOp :: (Double -> Double -> Double) -> Normal -> Normal -> Normal
  liftBinOp f (Normal μ σ) (Normal dμ dσ) = Normal (f μ dμ) (f σ dσ)

  zero  _ = 0 ::: 0 ::: VNil


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
  type Arity HalfNormal = Nat1

  gradLogProb :: HalfNormal -> Double -> Vec Nat1 Double
  gradLogProb (HalfNormal σ) x
    | x < 0         = error "halfNormalGradLogPdfRaw: No gradient at x < 0"
    | otherwise     = let _ ::: dσ ::: VNil = gradLogProb (Normal 0 σ) x
                      in Vec.singleton dσ

  safeAddGrad (HalfNormal σ) (dσ ::: VNil) = HalfNormal σ'
    where σ' = let σ_new = σ + dσ in if σ_new <= 0 then σ else σ_new

  liftUnOp :: (Double -> Double) -> HalfNormal -> HalfNormal
  liftUnOp f (HalfNormal σ) = HalfNormal (f σ)

  liftBinOp :: (Double -> Double -> Double) -> HalfNormal -> HalfNormal -> HalfNormal
  liftBinOp f (HalfNormal σ) (HalfNormal dσ) = HalfNormal (f σ dσ)

  zero  _ = 0 ::: VNil


  toList :: HalfNormal -> [Double]
  toList (HalfNormal σ) = [σ]

  fromList :: [Double] -> HalfNormal
  fromList [σ] = HalfNormal σ

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
  type Support Bernoulli = Bool

  sampleInv :: Bernoulli -> Double -> Bool
  sampleInv (Bernoulli p) r = r < p

  sample :: Bernoulli -> Sampler Bool
  sample (Bernoulli p) = Sampler.sampleBernoulli p

  logProbRaw :: Bernoulli -> Bool -> Double
  logProbRaw (Bernoulli p) y
    | y         = log p
    | otherwise = log (1 - p)

  isDifferentiable :: Bernoulli -> Maybe (Witness DiffDistribution Bernoulli)
  isDifferentiable _ = Nothing

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
  type Support Binomial = Int

  sampleInv :: Binomial -> Double -> Int
  sampleInv (Binomial n p) = invCMF (prob (Binomial n p))

  sample :: Binomial -> Sampler Int
  sample (Binomial n p) = Sampler.sampleBinomial n p

  logProbRaw :: Binomial -> Int -> Double
  logProbRaw (Binomial n p) y
    | y < 0 || y > n          = m_neg_inf
    | otherwise               = logChoose n y + log p * y' + log1p (-p) * ny'
    where
      y'  = fromIntegral   y
      ny' = fromIntegral $ n - y

  isDifferentiable :: Binomial -> Maybe (Witness DiffDistribution Binomial)
  isDifferentiable _ = Nothing

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
  type Support Categorical = Int            -- ^ an index from @0@ to @n - 1@

  sampleInv :: Categorical  -> Double -> Int
  sampleInv (Categorical ps) = invCMF (ps !!)

  sample :: Categorical  -> Sampler Int
  sample (Categorical ps) = Sampler.sampleCategorical (Vector.fromList ps)

  logProbRaw :: Categorical  -> Int -> Double
  logProbRaw (Categorical ps) idx
    | idx < 0 || idx >= length ps = trace "CategoricalLogPdf: idx < 0 || idx >= length ps" m_neg_inf
    | otherwise                   = log (ps !! idx)

  isDifferentiable :: Categorical  -> Maybe (Witness DiffDistribution (Categorical))
  isDifferentiable _ = Nothing

-- | Deterministic(x)
data Deterministic a where
  Deterministic
    :: (Show a, Typeable a, Eq a)
    => a                                  -- ^ value of probability @1@
    -> Deterministic a

mkDeterministic :: (Show a, Typeable a, Eq a) => a -> Deterministic a
mkDeterministic = Deterministic

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

  isDifferentiable :: Deterministic a -> Maybe (Witness DiffDistribution (Deterministic a))
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
  | null xps     = error "Discrete: xps must be non-empty"
  | any (< 0) ps = error "Discrete: probabilities must be >= 0"
  | any (> 0) ps = Discrete (zip xs (map (/ sum ps) ps))
  | otherwise    = Discrete (zip xs (map (const $ 1/fromIntegral (length ps)) ps))
  where (xs, ps) = unzip xps

instance Show a => Show (Discrete a) where
  show (Discrete xps) = "Discrete " ++ show xps

instance (Show a, Typeable a) => Distribution (Discrete a) where
  type Support (Discrete a) = a

  sampleInv :: Discrete a -> Double -> a
  sampleInv (Discrete xps) r = xs !! invCMF (ps !!) r
    where (xs, ps) = unzip xps

  sample :: Discrete a -> Sampler a
  sample (Discrete xps) = Sampler.sampleDiscrete xps

  logProbRaw :: Discrete a -> a -> Double
  logProbRaw (Discrete xps) y = case lookup y xps of
      Nothing -> trace ("Couldn't find " ++ show y ++ " in Discrete dist") m_neg_inf
      Just p  -> log p

  isDifferentiable :: Discrete a -> Maybe (Witness DiffDistribution (Discrete a))
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
  sample (Poisson λ) = Sampler.samplePoisson λ

  logProbRaw :: Poisson -> Int -> Double
  logProbRaw (Poisson λ) y
    | y < 0     = trace "poissonLogPdfRaw:  y < 0 " m_neg_inf
    | otherwise = log λ * fromIntegral y - logFactorial y - λ

  isDifferentiable :: Poisson -> Maybe (Witness DiffDistribution Poisson)
  isDifferentiable _ = Nothing

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
  type Support Uniform = Double

  sampleInv :: Uniform -> Double -> Double
  sampleInv (Uniform min max) r = min + (max - min) * r

  sample :: Uniform -> Sampler Double
  sample (Uniform min max) = Sampler.sampleUniform min max

  logProbRaw :: Uniform -> Double -> Double
  logProbRaw (Uniform min max) x
    | x < min || x > max = m_neg_inf
    | otherwise          = -log(max - min)

  isDifferentiable :: Uniform -> Maybe (Witness DiffDistribution Uniform)
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
  sampleInv (UniformD min max) r = floor (min' + (max' - min') * r)
     where min' = fromIntegral min
           max' = fromIntegral max + 1

  sample :: UniformD -> Sampler Int
  sample (UniformD min max) = Sampler.sampleUniformD min max

  logProbRaw :: UniformD -> Int -> Double
  logProbRaw (UniformD min max) idx
    | idx < min || idx > max  = m_neg_inf
    | otherwise               = - log (fromIntegral $ max - min + 1)

  isDifferentiable :: UniformD -> Maybe (Witness DiffDistribution UniformD)
  isDifferentiable _ = Nothing

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