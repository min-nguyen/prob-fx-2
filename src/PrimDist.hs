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
  , pattern PrimDistPrf
  , pattern TypeableDistPrf
  -- * Sampling
  , sample
  , sampleInv
  -- , sampleBayes
  -- * Density
  , logProb
  , gradLogProb) where

import Data.Kind ( Constraint )
import Data.Map (Map)
import OpenSum (OpenSum)
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector as Vec
import qualified Data.Vector.Unboxed as UV
import qualified OpenSum
import qualified System.Random.MWC.Distributions as MWC
import Statistics.Distribution ( ContDistr(density, logDensity), DiscreteDistr(probability, logProbability) )
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
  (HalfCauchy σ )     -> sampleCauchy 0 σ >>= pure . abs
  (Cauchy μ σ )       -> sampleCauchy μ σ
  (HalfNormal σ )     -> sampleNormal 0 σ >>= pure . abs
  (Normal μ σ )       -> sampleNormal μ σ
  (Uniform min max )  -> sampleUniform min max
  (UniformD min max ) -> sampleUniformD min max
  (Gamma k θ )        -> sampleGamma k θ
  (Beta α β  )        -> sampleBeta α β
  (Binomial n p  )    -> sampleBinomial n p
  (Bernoulli p )      -> sampleBernoulli p
  (Discrete ps )      -> sampleCategorical (V.fromList $ fmap snd ps) >>= \i -> pure $ fst $ ps !! i
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
  (Discrete ps )      -> sampleCategoricalInv (V.fromList $ fmap snd ps) >=> \i -> pure $ fst $ ps !! i
  (Categorical ps )   -> sampleDiscreteInv ps
  (Poisson λ )        -> samplePoissonInv λ
  (Dirichlet xs )     -> sampleDirichletInv xs
  (Deterministic x)   -> const (pure x)

-- | Compute the log density of a primitive distribution generating an observed value
logProb ::
  -- | distribution
     PrimDist a
  -- | observed value
  -> a
  -- | log density
  -> LogP
logProb (Normal μ σ) y
  = LogP $ logDensity (normalDistr μ σ) y
logProb (HalfNormal σ) y
  = LogP $ if y < 0 then m_neg_inf else log 2 + logDensity (normalDistr 0 σ) y
logProb (Cauchy μ σ) y
  = LogP $ logDensity (cauchyDistribution μ σ) y
logProb (HalfCauchy σ) y
  = LogP $ if y < 0 then m_neg_inf else log 2 + logDensity (cauchyDistribution 0 σ) y
logProb (Gamma k θ) y
  = LogP $ logDensity (gammaDistr k θ) y
logProb (Beta α β) y
  = LogP $ logDensity (betaDistr α β) y
logProb (Uniform min max) y
  = LogP $ logDensity (uniformDistr min max) y
logProb (Dirichlet xs) ys =
  let xs' = map (/Prelude.sum xs) xs
  in  if Prelude.sum xs' /= 1 then error "Dirichlet can't normalize" else
      case dirichletDistribution (UV.fromList xs')
      of Left e -> error $ "Dirichlet error: " ++ e
         Right d -> let p = dirichletDensity d (UV.fromList ys)
                    in  LogP (ln p)
logProb (Bernoulli p) i
  = LogP $ logProbability (binomial 1 p) (boolToInt i)
logProb (Binomial n p) y
  = LogP $ logProbability (binomial n p) y
logProb (Poisson λ) y
  = LogP $ logProbability (poisson λ) y
logProb (Deterministic x) y
  = if x == y then LogP 0 else LogP m_neg_inf
logProb (Categorical ps) y
  = LogP $ log (ps !! y)
logProb d@(Discrete ps) y
  = case lookup y ps of
      Nothing -> error $ "Couldn't find " ++ show y ++ " in Discrete dist"
      Just p  -> LogP $ log p
logProb (UniformD min max) y
  = LogP $ logProbability (discreteUniformAB min max) y

-- | Given fixed parameters and observed value, compute the gradients of a distribution's log-pdf w.r.t its parameters
gradLogProb ::
     PrimDist a
  -> a
  -> PrimDist a
gradLogProb d y = error "to do"

{- Dictionary proofs for constraints on primitive distributions -}
data Dict c a where
  Dict :: c a => Dict c a

-- | An ad-hoc specification of primitive value types, for constraining the outputs of distributions
type PrimVal = '[Int, Double, [Double], Bool, String]

class    (Show x, OpenSum.Member x PrimVal) => IsPrimVal x

instance (Show x, OpenSum.Member x PrimVal) => IsPrimVal x

-- | For pattern-matching on an arbitrary @PrimDist@ with proof that it generates a primitive value
pattern PrimDistPrf :: () => (IsPrimVal x) => PrimDist x -> PrimDist x
pattern PrimDistPrf d <- d@(primDistPrf -> Just Dict)

primDistPrf :: PrimDist x -> Maybe (Dict IsPrimVal x)
primDistPrf d = case d of
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