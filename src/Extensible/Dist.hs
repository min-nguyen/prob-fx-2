{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, TypeOperators #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Extensible.Dist where

import Extensible.IO
import Extensible.Freer
import Sample
import Util
import Control.Lens hiding ((:>))
import Control.Monad.State
import Data.Maybe
import Data.Extensible hiding (wrap, Head, Member)
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
  NormalDist        :: Double -> Double -> Maybe Double -> Dist Double
  -- uniform        :: Dist Double
  UniformDist       :: Double -> Double -> Maybe Double -> Dist Double
  -- discr uniform  :: Dist Int
  DiscrUniformDist  :: Int    -> Int    -> Maybe Int -> Dist Int
  -- gamma          :: Dist Double
  GammaDist         :: Double -> Double -> Maybe Double -> Dist Double
  -- beta           :: Dist Double
  BetaDist          :: Double -> Double -> Maybe Double -> Dist Double
  -- binomial       :: Dist [Bool]
  BinomialDist      :: Int    -> Double -> Maybe Int -> Dist Int
  -- bernoulli      :: Dist Bool
  BernoulliDist     :: Double -> Maybe Bool -> Dist Bool
  -- categorical    :: Dist Int
  CategoricalDist   :: V.Vector (Int, Double) -> Maybe Int -> Dist Int
  -- discrete       :: Dist Int
  DiscreteDist      :: [(Int, Double)] -> Maybe Int -> Dist Int

sample :: Dist a -> Sampler a
sample (NormalDist μ σ obs)  =
  createSampler (sampleNormal μ σ) >>= return
sample (UniformDist min max obs  )  = 
  createSampler (sampleUniform min max) >>= return 
sample (DiscrUniformDist min max obs )  = 
  createSampler (sampleDiscreteUniform min max) >>= return 
sample (GammaDist k θ obs )        = 
  createSampler (sampleGamma k θ) >>= return 
sample (BetaDist α β  obs )         = 
  createSampler (sampleBeta α β) >>= return 
sample (BinomialDist n p  obs )     = 
  createSampler (sampleBinomial n p) >>=  return .  length
sample (BernoulliDist p obs )      = 
  createSampler (sampleBernoulli p) >>= return 
sample (CategoricalDist ps obs )   = 
  createSampler (sampleCategorical (fmap snd ps)) >>= return 
sample (DiscreteDist ps obs )      = 
  createSampler (sampleDiscrete (map snd ps)) >>= return 

runDist :: Member (Lift Sampler) rs => Freer (Dist : rs) a 
        -> Freer rs  a
runDist m = loop m where
  loop :: Member (Lift Sampler) rs => Freer (Dist : rs) a -> Freer rs a
  loop (Pure x) = return x
  loop (Free u k) = case decomp u of 
    Right d@(NormalDist m sigma y) 
      -> send (Lift (sample d)) >>= loop . k
    Left  u'  -> Free u' (loop . k)