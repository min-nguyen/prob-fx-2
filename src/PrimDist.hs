{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}


{- | A GADT encoding of (a selection of) primitive distributions
    along with their corresponding sampling and density functions.
-}

module PrimDist (
  -- * Primitive distribution
    PrimDist(..)
  , PrimVal
  , IsPrimVal(..)
  , pattern PrimDistPrf
  , ErasedPrimDist(..)
  -- * Sampling
  , sample
  , sampleBayes
  -- * Density
  , prob
  , logProb) where

import Data.Kind ( Constraint )
import Data.Map (Map)
import OpenSum (OpenSum)
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector as Vec
import qualified Data.Vector.Unboxed as UV
import qualified OpenSum
import qualified System.Random.MWC.Distributions as MWC
import Statistics.Distribution ( ContDistr(density), DiscreteDistr(probability) )
import Statistics.Distribution.Beta ( betaDistr )
import Statistics.Distribution.Binomial ( binomial )
import Statistics.Distribution.CauchyLorentz ( cauchyDistribution )
import Statistics.Distribution.Dirichlet ( dirichletDensity, dirichletDistribution )
import Statistics.Distribution.DiscreteUniform ( discreteUniformAB )
import Statistics.Distribution.Gamma ( gammaDistr )
import Statistics.Distribution.Normal ( normalDistr )
import Statistics.Distribution.Poisson ( poisson )
import Statistics.Distribution.Uniform ( uniformDistr )
import Sampler
import LogP ( LogP(..) )
import Util ( boolToInt )
import Numeric.Log ( Log(Exp) )
import qualified Control.Monad.Bayes.Class as MB


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
    :: (Eq a, Show a, OpenSum.Member a PrimVal)
    => a                -- ^ value of probability @1@
    -> PrimDist a
  Dirichlet
    :: [Double]         -- ^ concentrations
    -> PrimDist [Double]
  Discrete
    :: (Eq a, Show a, OpenSum.Member a PrimVal)
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

instance Eq (PrimDist a) where
  (==) (Normal m s) (Normal m' s') = m == m' && s == s'
  (==) (Cauchy m s) (Cauchy m' s') = m == m' && s == s'
  (==) (HalfCauchy s) (HalfCauchy s') = s == s'
  (==) (HalfNormal s) (HalfNormal s') = s == s'
  (==) (Bernoulli p) (Bernoulli p') = p == p'
  (==) (Binomial n p) (Binomial n' p') = n == n' && p == p'
  (==) (Categorical ps) (Categorical ps') = ps == ps'
  (==) (Beta a b) (Beta a' b') = a == a' && b == b'
  (==) (Gamma a b) (Gamma a' b') = a == a' && b == b'
  (==) (Uniform a b) (Uniform a' b') = a == a' && b == b'
  (==) (UniformD min max) (UniformD min' max') = min == min' && max == max'
  (==) (Poisson l) (Poisson l') = l == l'
  (==) (Discrete xs) (Discrete xs') = xs == xs'
  (==) (Dirichlet xs) (Dirichlet xs')  = xs == xs'
  (==) (Deterministic x) (Deterministic x') = x == x'
  (==) _ _ = False

instance Show a => Show (PrimDist a) where
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
   "Deterministic(" ++ show x ++ ", " ++ ")"


-- | An ad-hoc specification of primitive value types, for constraining the outputs of distributions
type PrimVal = '[Int, Double, [Double], Bool, String]

-- | Proof that @x@ is a primitive value
data IsPrimVal x where
  IsPrimVal :: (Show x, OpenSum.Member x PrimVal) => IsPrimVal x

-- | For pattern-matching on an arbitrary @PrimDist@ with proof that it generates a primitive value
pattern PrimDistPrf :: () => (Show x, OpenSum.Member x PrimVal) => PrimDist x -> PrimDist x
pattern PrimDistPrf d <- d@(primDistPrf -> IsPrimVal)

-- | Proof that all primitive distributions generate a primitive value
primDistPrf :: PrimDist x -> IsPrimVal x
primDistPrf d = case d of
  HalfCauchy {} -> IsPrimVal
  Cauchy {} -> IsPrimVal
  Normal {} -> IsPrimVal
  HalfNormal  {} -> IsPrimVal
  Uniform  {} -> IsPrimVal
  UniformD {} -> IsPrimVal
  Gamma {} -> IsPrimVal
  Beta {} -> IsPrimVal
  Binomial {} -> IsPrimVal
  Bernoulli {} -> IsPrimVal
  Categorical {} -> IsPrimVal
  Discrete {} -> IsPrimVal
  Poisson {} -> IsPrimVal
  Dirichlet {} -> IsPrimVal
  Deterministic {} -> IsPrimVal

-- | For erasing the types of primitive distributions
data ErasedPrimDist where
  ErasedPrimDist :: forall a. Show a => PrimDist a -> ErasedPrimDist

instance Show ErasedPrimDist where
  show (ErasedPrimDist d) = show d

-- | Draw a value from a primitive distribution in the @Sampler@ monad
sample :: PrimDist a -> Sampler a
sample (HalfCauchy σ )  =
  createSampler (sampleCauchy 0 σ) >>= pure . abs
sample (Cauchy μ σ )  =
  createSampler (sampleCauchy μ σ)
sample (HalfNormal σ )  =
  createSampler (sampleNormal 0 σ) >>= pure . abs
sample (Normal μ σ )  =
  createSampler (sampleNormal μ σ)
sample (Uniform min max )  =
  createSampler (sampleUniform min max)
sample (UniformD min max )  =
  createSampler (sampleDiscreteUniform min max)
sample (Gamma k θ )        =
  createSampler (sampleGamma k θ)
sample (Beta α β  )         =
  createSampler (sampleBeta α β)
sample (Binomial n p  )     =
  createSampler (sampleBinomial n p) >>=  pure .  length . filter (== True)
sample (Bernoulli p )      =
  createSampler (sampleBernoulli p)
sample (Discrete ps )   =
  createSampler (sampleCategorical (V.fromList $ fmap snd ps)) >>= \i -> pure $ fst $ ps !! i
sample (Categorical ps )      =
  createSampler (sampleDiscrete ps)
sample (Poisson λ ) =
  createSampler (samplePoisson λ)
sample (Dirichlet xs ) =
  createSampler (sampleDirichlet xs)
sample (Deterministic x) = pure x

-- | Draw a value from a primitive distribution using the @MonadSample@ type class from Monad-Bayes
sampleBayes :: MB.MonadSample m => PrimDist a -> m a
sampleBayes (Uniform a b )    = MB.uniform a b
sampleBayes (Categorical as ) = MB.categorical (Vec.fromList as)
sampleBayes (Discrete as )    = MB.categorical (Vec.fromList (map snd as)) >>= (pure . fst . (as !!))
sampleBayes (Normal mu std )  = MB.normal mu std
sampleBayes (Gamma k t )      = MB.gamma k t
sampleBayes (Beta a b )       = MB.beta a b
sampleBayes (Bernoulli p )    = MB.bernoulli p
sampleBayes (Binomial n p )   = sequence (replicate n (MB.bernoulli p)) >>= (pure . length . filter (== True))
sampleBayes (Poisson l )      = MB.poisson l
sampleBayes (Dirichlet as )   = MB.dirichlet (Vec.fromList as) >>= pure . Vec.toList
sampleBayes (PrimDistPrf d)   = error ("Sampling from " ++ show d ++ " is not supported")

-- | Compute the density of a primitive distribution generating an observed value
prob ::
  -- | distribution
     PrimDist a
  -- | observed value
  -> a
  -- | density
  -> Double
prob (Dirichlet xs) ys =
  let xs' = map (/Prelude.sum xs) xs
  in  if Prelude.sum xs' /= 1 then error "dirichlet can't normalize" else
      case dirichletDistribution (UV.fromList xs')
      of Left e -> error "dirichlet error"
         Right d -> let Exp p = dirichletDensity d (UV.fromList ys)
                        in  exp p
prob (HalfCauchy σ) y
  = if y < 0 then 0 else 2 * density (cauchyDistribution 0 σ) y
prob (Cauchy μ σ) y
  = density (cauchyDistribution μ σ) y
prob (HalfNormal σ) y
  = if y < 0 then 0 else 2 * density (normalDistr 0 σ) y
prob (Normal μ σ) y
  = density (normalDistr μ σ) y
prob (Uniform min max) y
  = density (uniformDistr min max) y
prob (Gamma k θ) y
  = density (gammaDistr k θ) y
prob  (Beta α β) y
  = density (betaDistr α β) y
prob (UniformD min max) y
  = probability (discreteUniformAB min max) y
prob (Binomial n p) y
  = probability (binomial n p) y
prob (Bernoulli p) i
  = probability (binomial 1 p) (boolToInt i)
prob d@(Discrete ps) y
  = case lookup y ps of
      Nothing -> error $ "Couldn't find " ++ show y ++ " in categorical dist"
      Just p  -> p
prob (Categorical ps) y  = ps !! y
prob (Poisson λ) y       = probability (poisson λ) y
prob (Deterministic x) y = 1

-- | Compute the log density of a primitive distribution generating an observed value
logProb ::
  -- | distribution
     PrimDist a
  -- | observed value
  -> a
  -- | log density
  -> LogP
logProb d = LogP . log . prob d
