{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, DerivingStrategies,
             GeneralizedNewtypeDeriving #-}

module Sampler where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST (ST, runST, stToIO)
import Control.Monad.Trans (MonadIO, MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT, ask, mapReaderT, runReaderT)
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List
import Debug.Trace
import qualified Data.Vector as V
import Statistics.Distribution
import Statistics.Distribution.Normal
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC.Dist

import Util

{-
We need to
- Use "MWC.create :: PrimMonad m => m (Gen (PrimState m))"
- We can concretize the "PrimMonad m" as the "ST s" monad, so this becomes
  "MWC.create :: ST s (Gen (PrimState ST s))", which is equivalent to
  "MWC.create :: ST s (GenST s)"
- We need to store the "GenST s" as an immutable read-only state.

  Initially, this could be "Reader (GenST s)" if we were perform
  "runST :: forall a. (forall s. ST s a) -> a" on "MWC.create :: ST s (GenST s)"
  and then use "reader :: Monad m => (r -> a) -> ReaderT r m a" to wrap around
  computations that require us to read from the "GenST s"

  Alternatively, we could use the ReaderT monad, where its read-only state is
  "GenST s" and its monad is "ST s"
-}

-- | An 'ST' based random sampler using the @mwc-random@ package.
-- type GenST s = Gen (PrimState (ST s))
-- newtype SamplerST a = SamplerST (forall s. ReaderT (GenST s) (ST s) a)

-- type GenIO = Gen (PrimState IO) is the generator state
-- IO is the monadic context we need to retain, because creating an initial generator
-- operates in the IO monad (createSystemRandom :: IO GenIO)
newtype Sampler a = Sampler {runSampler :: ReaderT MWC.GenIO IO a}
  deriving (Functor, Applicative, Monad)

liftS :: IO a -> Sampler a
liftS f = Sampler $ lift f

-- To sample a random number, we run `sampleIO . createSampler $ sampleRandom`
-- for example.

-- | Takes a Sampler, provides it a random generator, and runs the sampler in the IO context
sampleIO :: Sampler a -> IO a
sampleIO m = MWC.createSystemRandom >>= (runReaderT . runSampler) m

-- | Takes a Sampler, provides it a fixed generator, and runs the sampler in the IO context
sampleIOFixed :: Sampler a -> IO a
sampleIOFixed m = MWC.create >>= (runReaderT . runSampler) m

-- | Takes a distribution which awaits a generator, and returns a Sampler
createSampler :: (MWC.GenIO -> IO a) -> Sampler a
createSampler f = Sampler $ ask >>= lift . f

{- Given distribution parameters, these functions await a generator and
   then sample a value from a distribution -}

sampleRandom :: MWC.GenIO -> IO Double
sampleRandom = \gen -> MWC.uniform gen

sampleNormal :: Double -> Double -> (MWC.GenIO -> IO Double)
sampleNormal μ σ = \gen -> MWC.Dist.normal μ σ gen

sampleUniform :: Double -> Double -> (MWC.GenIO -> IO Double)
sampleUniform min max = \gen -> MWC.uniformR (min, max) gen

sampleDiscreteUniform :: Int -> Int -> (MWC.GenIO -> IO Int)
sampleDiscreteUniform min max = \gen -> MWC.uniformR (min, max) gen

sampleGamma :: Double -> Double -> (MWC.GenIO -> IO Double)
sampleGamma k θ = \gen -> MWC.Dist.gamma k θ gen

sampleBeta :: Double -> Double -> (MWC.GenIO -> IO Double)
sampleBeta α β = \gen -> MWC.Dist.beta α β gen

sampleBernoulli :: Double -> (MWC.GenIO -> IO Bool)
sampleBernoulli p = \gen -> MWC.Dist.bernoulli p gen

sampleBinomial :: Int -> Double -> (MWC.GenIO -> IO [Bool])
sampleBinomial n p = \gen -> replicateM n (MWC.Dist.bernoulli p gen)

sampleCategorical :: V.Vector Double -> (MWC.GenIO -> IO Int)
sampleCategorical ps = trace (show ps) $ \gen -> MWC.Dist.categorical (ps) gen

sampleDiscrete :: [Double] -> (MWC.GenIO -> IO Int)
sampleDiscrete ps = \gen -> MWC.Dist.categorical (V.fromList ps) gen
