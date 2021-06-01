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
runDist :: Freer (Dist : rs) a 
        -> Freer (Lift Sampler ': rs)  a
runDist  m = loop m where
  loop :: Freer (Dist : rs) a -> Freer (Lift Sampler ': rs) a
  -- At this point, all Reader requests have been handled
  loop (Pure x) = return x
  -- Handle if Reader request, else ignore and go through the rest of the tree (by leaving the request's continuation k there to handle it, but also composing this with 'loop' so that the reader handler can then carry on afterwards).
  loop (Free u k) = case decomp u of 
    Right d@(NormalDist m sigma y) 
      -> (send $ Lift (sample d) :: Freer (Lift Sampler ': rs) Double) >>= 
        \x -> loop (k x) -- (send $ Lift (sample d)) >>= k
    -- Left  u'  -> Free u' (loop . k)

g :: Member (Lift Sampler) rs => Freer rs ()
g = do 
  send $ Lift (sample (NormalDist 0 0 Nothing))
  return ()