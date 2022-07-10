{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module PrimDist where

import Data.Kind
import Data.Map (Map)
import Numeric.Log
import OpenSum (OpenSum)
import qualified Control.Monad.Bayes.Class as MB
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector as Vec
import qualified Data.Vector.Unboxed as UV
import qualified OpenSum
import qualified System.Random.MWC.Distributions as MWC
import Sampler
import Statistics.Distribution
import Statistics.Distribution.Beta
import Statistics.Distribution.Binomial
import Statistics.Distribution.CauchyLorentz
import Statistics.Distribution.Dirichlet
import Statistics.Distribution.DiscreteUniform
import Statistics.Distribution.Gamma
import Statistics.Distribution.Normal
import Statistics.Distribution.Poisson
import Statistics.Distribution.Uniform
import Util ( boolToInt )

-- ||| (Section 4.2.1) Primitive distributions
data PrimDist a where
  HalfCauchy    :: Double -> PrimDist Double
  Cauchy        :: Double -> Double -> PrimDist Double
  Normal        :: Double -> Double -> PrimDist Double
  HalfNormal    :: Double -> PrimDist Double
  Uniform       :: Double -> Double -> PrimDist Double
  UniformD  :: Int    -> Int    -> PrimDist Int
  Gamma         :: Double -> Double -> PrimDist Double
  Beta          :: Double -> Double -> PrimDist Double
  Binomial      :: Int    -> Double -> PrimDist Int
  Bernoulli     :: Double -> PrimDist Bool
  Categorical   :: [Double] -> PrimDist Int
  Poisson       :: Double -> PrimDist Int
  Dirichlet     :: [Double] -> PrimDist [Double]
  Discrete      :: (Eq a, Show a, OpenSum.Member a PrimVal) => [(a, Double)] -> PrimDist a
  Deterministic :: (Eq a, Show a, OpenSum.Member a PrimVal) => a -> PrimDist a

-- | For constraining the output types of disstributions
type PrimVal = '[Int, Double, [Double], Bool, String]

data Dict (a :: Constraint) where
  Dict :: a => Dict a

primDistDict :: PrimDist x -> Dict (Show x, OpenSum.Member x PrimVal)
primDistDict d = case d of
  HalfCauchy {} -> Dict
  Cauchy {} -> Dict
  Normal {} -> Dict
  HalfNormal  {} -> Dict
  Uniform  {} -> Dict
  UniformD {} -> Dict
  Gamma {} -> Dict
  Beta {} -> Dict
  Binomial {} -> Dict
  Bernoulli {} -> Dict
  Discrete {} -> Dict
  Categorical {} -> Dict
  Poisson {} -> Dict
  Dirichlet {} -> Dict
  Deterministic {} -> Dict

pattern PrimDistDict :: () => (Show x, OpenSum.Member x PrimVal) => PrimDist x -> PrimDist x
pattern PrimDistDict d <- d@(primDistDict -> Dict)

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
  
-- | For erasing the types of primitive distributions
data ErasedPrimDist where
  ErasedPrimDist :: forall a. Show a => PrimDist a -> ErasedPrimDist

instance Show ErasedPrimDist where
  show (ErasedPrimDist d) = show d
  
-- ||| (Section 6.1) Sampling functions
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

sampleBayes :: MB.MonadSample m => PrimDist a -> m a
sampleBayes (Uniform a b )     = MB.uniform a b
sampleBayes (Categorical as )     = MB.categorical (Vec.fromList as)
sampleBayes (Discrete as )  = MB.categorical (Vec.fromList (map snd as)) >>= (pure . fst . (as !!))
sampleBayes (Normal mu std )   = MB.normal mu std
sampleBayes (Gamma k t )       = MB.gamma k t
sampleBayes (Beta a b )        = MB.beta a b
sampleBayes (Bernoulli p )     = MB.bernoulli p
sampleBayes (Binomial n p )    = sequence (replicate n (MB.bernoulli p)) >>= (pure . length . filter (== True))
sampleBayes (Poisson l )       = MB.poisson l
sampleBayes (Dirichlet as )    = MB.dirichlet (Vec.fromList as) >>= pure . Vec.toList
sampleBayes (PrimDistDict d)       = error ("Sampling from " ++ show d ++ " is not supported")

-- ||| (Section 6.2) Probability density functions
prob :: PrimDist a -> a -> Double
prob (Dirichlet xs) ys =
  let xs' = map (/(Prelude.sum xs)) xs
  in  if Prelude.sum xs' /= 1 then error "dirichlet can't normalize" else
      case dirichletDistribution (UV.fromList xs')
      of Left e -> error "dirichlet error"
         Right d -> let Exp p = dirichletDensity d (UV.fromList ys)
                        in  exp p
prob (HalfCauchy σ) y
  = if y < 0 then 0 else
            2 * density (cauchyDistribution 0 σ) y
prob (Cauchy μ σ) y
  = density (cauchyDistribution μ σ) y
prob (HalfNormal σ) y
  = if y < 0 then 0 else
            2 * density (normalDistr 0 σ) y
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
prob (Categorical ps) y     = ps !! y
prob (Poisson λ) y       = probability (poisson λ) y
prob (Deterministic x) y = 1

logProb :: PrimDist a -> a -> Double
logProb d = log . prob d
