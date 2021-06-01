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
-- sample (UniformDist min max obs k )  = 
--   createSampler (sampleUniform min max) >>= return . k
-- sample (DiscrUniformDist min max obs k)  = 
--   createSampler (sampleDiscreteUniform min max) >>= return . k
-- sample (GammaDist k' θ obs k)        = 
--   createSampler (sampleGamma k' θ) >>= return . k
-- sample (BetaDist α β  obs k)         = 
--   createSampler (sampleBeta α β) >>= return . k
-- sample (BinomialDist n p  obs k)     = 
--   createSampler (sampleBinomial n p) >>=  return . k . length
-- sample (BernoulliDist p obs k)      = 
--   createSampler (sampleBernoulli p) >>= return . k
-- sample (CategoricalDist ps obs k)   = 
--   createSampler (sampleCategorical (fmap snd ps)) >>= return . k
-- sample (DiscreteDist ps obs k)      = 
--   createSampler (sampleDiscrete (map snd ps)) >>= return . k

-- runDist :: Member Dist rs => Freer (Dist ': rs) a -> Freer (Sampler ': rs) a
-- runDist (Pure x) = return x
-- runDist (Free u q) = case prj u of
--      Just (NormalDist m sigma y) -> loop (lift (sample $ NormalDist m sigma y))

-- runDist :: forall rs a. Freer (Dist  ': rs) a -> Freer rs a
runDist :: Member (Lift Sampler) rs => Freer (Dist : rs) a 
        -> Freer rs  a
runDist m = loop m where
  loop :: Member (Lift Sampler) rs => Freer (Dist : rs) a -> Freer rs a
  loop (Pure x) = return x
  loop (Free u k) = case decomp u of 
    Right d@(NormalDist m sigma y) 
      -> send (Lift (sample d)) >>= loop . k
    Left  u'  -> Free u' (loop . k)

g :: Member (Lift Sampler) rs => Freer rs ()
g = do 
  send $ Lift (sample (NormalDist 0 0 Nothing))
  return () 
