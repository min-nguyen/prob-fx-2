{-# LANGUAGE GADTs, TypeOperators #-}

module Dist where

import FreeT
import Sample
import Util
import Control.Lens hiding ((:>))
import Control.Monad.State
import Data.Maybe
import Data.Extensible hiding (wrap, Head)
import qualified Data.Vector as V
import Statistics.Distribution
import Statistics.Distribution.DiscreteUniform
import Statistics.Distribution.Normal
import Statistics.Distribution.Gamma
import Statistics.Distribution.Beta
import Statistics.Distribution.Binomial
import Statistics.Distribution.Uniform
import System.Random.MWC
import qualified System.Random.MWC.Distributions as MWC

data Dist a where
  -- normal         :: Dist Double
  NormalDist        :: Double -> Double -> Maybe Double -> (Double -> a) -> Dist a
  -- uniform        :: Dist Double
  UniformDist       :: Double -> Double -> Maybe Double -> (Double -> a) -> Dist a
  -- discr uniform  :: Dist Int
  DiscrUniformDist  :: Int    -> Int    -> Maybe Int -> (Int -> a) -> Dist a
  -- gamma          :: Dist Double
  GammaDist         :: Double -> Double -> Maybe Double -> (Double -> a) -> Dist a
  -- beta           :: Dist Double
  BetaDist          :: Double -> Double -> Maybe Double -> (Double -> a) -> Dist a
  -- binomial       :: Dist [Bool]
  BinomialDist      :: Int    -> Double -> Maybe Int -> (Int -> a) -> Dist a
  -- bernoulli      :: Dist Bool
  BernoulliDist     :: Double -> Maybe Bool -> (Bool -> a) -> Dist a
  -- categorical    :: Dist Int
  CategoricalDist   :: V.Vector (Int, Double) -> Maybe Int -> (Int -> a) -> Dist a
  -- discrete       :: Dist Int
  DiscreteDist      :: [(Int, Double)] -> Maybe Int -> (Int -> a) -> Dist a

hasObs :: Dist a -> Bool
hasObs d@(NormalDist _ _ obs _)       = isJust obs
hasObs d@(DiscrUniformDist _ _ obs _) = isJust obs
hasObs d@(UniformDist _ _ obs _)      = isJust obs
hasObs d@(GammaDist _ _ obs _)        = isJust obs
hasObs d@(BetaDist _ _ obs k)         = isJust obs
hasObs d@(BinomialDist _ _ obs _)     = isJust obs
hasObs d@(BernoulliDist _ obs _)      = isJust obs
hasObs d@(CategoricalDist _ obs _)    = isJust obs
hasObs d@(DiscreteDist _ obs _)       = isJust obs

instance Functor Dist where
  fmap f (NormalDist mu sigma y g) = NormalDist mu sigma y (f . g)
  fmap f (UniformDist min max y g) = UniformDist min max y (f . g)
  fmap f (DiscrUniformDist min max y g) = DiscrUniformDist min max y (f . g)
  fmap f (GammaDist a b y g) = GammaDist a b y (f . g)
  fmap f (BinomialDist n p y g) = BinomialDist n p y (f . g)
  fmap f (BernoulliDist p y g) = BernoulliDist p y (f . g)

instance Distribution (Dist a) where
  cumulative (NormalDist μ σ _ _) x
    = cumulative (normalDistr μ σ) x
  cumulative (UniformDist min max obs _) x
    = cumulative (uniformDistr min max) x
  cumulative (GammaDist k θ _ _) x 
    = cumulative (gammaDistr k θ) x
  cumulative (BetaDist α β _ _) x
    = cumulative (betaDistr α β) x
  cumulative (BinomialDist n p _ _) x
    = cumulative (binomial n p) x
  cumulative (BernoulliDist p _ _) x
    = cumulative (binomial 1 p) x
  cumulative (CategoricalDist ps _ _) x
    = foldr (\(a, ap) p -> if fromIntegral a <= x then p + ap else p) 0 ps
  cumulative (DiscreteDist ps _ _) x
    = foldr (\(a, ap) p -> if fromIntegral a <= x then p + ap else p) 0 ps

instance ContDistr (Dist a) where
  density (NormalDist μ σ _ _)          = density (normalDistr μ σ)
  density (UniformDist min max _ _)     = density (uniformDistr min max)
  density (GammaDist k θ _ _)           = density (gammaDistr k θ)
  density (BetaDist α β _ _)            = density (betaDistr α β)
  logDensity (NormalDist μ σ _ _)       = logDensity (normalDistr μ σ)
  logDensity (UniformDist min max _ _)  = logDensity (uniformDistr min max)
  logDensity (GammaDist k θ _ _)        = logDensity (gammaDistr k θ)
  logDensity (BetaDist α β _ _)         = logDensity (betaDistr α β)
  quantile (NormalDist μ σ _ _)         = quantile (normalDistr μ σ)
  quantile (UniformDist min max _ _)    = quantile (uniformDistr min max)
  quantile (GammaDist k θ _ _)          = quantile (gammaDistr k θ)
  quantile (BetaDist α β _ _)           = quantile (betaDistr α β)

instance DiscreteDistr (Dist a) where
  -- binomial: probability of `i` successful outcomes
  probability (BinomialDist n p _ _) i            = probability (binomial n p) i
  probability (BernoulliDist p _ _) i             = probability (binomial 1 p) i
  probability (CategoricalDist ps _ _) i          = snd (ps V.! i)
  probability (DiscreteDist ps _ _) i             = snd (ps !! i)
  probability (DiscrUniformDist min max _ _) i    = probability (discreteUniformAB min max) i
  logProbability (BinomialDist n p _ _) i         = logProbability (binomial n p) i
  logProbability (BernoulliDist p _ _) i          = logProbability (binomial 1 p) i
  logProbability (CategoricalDist ps _ _) i       = (log . snd) (ps V.! i)
  logProbability (DiscreteDist ps _ _) i          = (log . snd) (ps !! i)
  logProbability (DiscrUniformDist min max _ _) i = logProbability (discreteUniformAB min max) i

{- Returns 'Either LogProb SampledValue' -}
handleDist :: Dist a -> Sampler (Either Double a)
handleDist d = if hasObs d then return $ Left (logProb d) else Right <$> sample d

prob :: Dist a -> Double
prob d@(NormalDist _ _ obs _)     
  = density d (fromJust obs)
prob d@(DiscrUniformDist _ _ obs _) 
  = probability d (fromJust obs)
prob d@(UniformDist _ _ obs _)     
  = density d (fromJust obs)
prob d@(GammaDist _ _ obs _)          
  = density d (fromJust obs)
prob d@(BetaDist _ _ obs k)           
  = density d (fromJust obs)
prob d@(BinomialDist _ _ obs _)       
  = probability d (fromJust obs)
prob d@(BernoulliDist _ obs _)        
  = probability d (boolToInt $ fromJust obs)
prob d@(CategoricalDist _ obs _)     
  = probability d (fromJust obs)
prob d@(DiscreteDist _ obs _)        
  = probability d (fromJust obs)

{- Combines logDensity and logProbability.
   Log probability of double `x` from a distribution -}
logProb :: Dist a -> Double
logProb = log . prob 

sample :: Dist a -> Sampler a
sample (NormalDist μ σ obs k)  =
  createSampler (sampleNormal μ σ) >>= return . k
sample (UniformDist min max obs k )  = 
  createSampler (sampleUniform min max) >>= return . k
sample (DiscrUniformDist min max obs k)  = 
  createSampler (sampleDiscreteUniform min max) >>= return . k
sample (GammaDist k' θ obs k)        = 
  createSampler (sampleGamma k' θ) >>= return . k
sample (BetaDist α β  obs k)         = 
  createSampler (sampleBeta α β) >>= return . k
sample (BinomialDist n p  obs k)     = 
  createSampler (sampleBinomial n p) >>=  return . k . length
sample (BernoulliDist p obs k)      = 
  createSampler (sampleBernoulli p) >>= return . k
sample (CategoricalDist ps obs k)   = 
  createSampler (sampleCategorical (fmap snd ps)) >>= return . k
sample (DiscreteDist ps obs k)      = 
  createSampler (sampleDiscrete (map snd ps)) >>= return . k