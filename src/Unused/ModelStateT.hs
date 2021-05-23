{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, DeriveFunctor, DerivingStrategies, GeneralizedNewtypeDeriving #-}

module ModelStateT where

import Control.Monad.State
import Control.Monad.Trans.Reader (ReaderT, ask, mapReaderT, runReaderT)

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC.Dist

type Model s a = StateT s Sampler a

newtype Sampler a = Sampler {runSampler :: ReaderT MWC.GenIO IO a}
  deriving (Functor, Applicative, Monad)

createSampler :: (MWC.GenIO -> IO a) -> Sampler a
createSampler f = Sampler $ ask >>= lift . f

sampleNormal' :: Double -> Double -> (MWC.GenIO -> IO Double)
sampleNormal' μ σ = \gen -> MWC.Dist.normal μ σ gen

sampleNormal :: Double -> Double -> Sampler Double
sampleNormal μ σ = createSampler (sampleNormal'  μ σ)

model :: (s -> m (a, s)) -> StateT s m a
model = StateT

-- Applicative attempt
normalApp' :: Model x (Double -> Double -> Sampler Double)
normalApp' = model (\x -> do 
  return (\mu sigma -> do 
      y <- sampleNormal mu sigma
      return y, x)
      )

normalApp :: Double -> Double -> Model x Double
normalApp mu sigma = do 
  x <- normalApp' <*> (pure mu) <*> (pure sigma)
  lift x

-- Monadic attempt
normalMon :: Double -> Double -> Model x Double
normalMon mu sigma = model (\x -> do 
      y <- sampleNormal mu sigma
      return (y, x))