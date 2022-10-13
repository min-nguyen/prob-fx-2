{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | An IO-based sampling monad.
-}

module Sampler (
  -- * Sampler monad
    Sampler
  , liftIO
  , sampleIO
  , sampleIOFixed
  , mkSampler
  -- * Sampling functions
  -- ** Raw sampling
  -- $Raw-sampling
  , sampleRandom
  , sampleCauchy
  , sampleNormal
  , sampleUniform
  , sampleUniformD
  , sampleGamma
  , sampleBeta
  , sampleBernoulli
  , sampleBinomial
  , sampleCategorical
  , sampleDiscrete
  , samplePoisson
  , sampleDirichlet
  ) where

import Control.Monad ( replicateM, when, (>=>) )
import Control.Monad.Trans (MonadIO, MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT (..), ask, mapReaderT, runReaderT)
import Data.Map (Map)
import Data.Functor
import GHC.Word ( Word32 )
import qualified Data.Vector as V
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC.Dist
import qualified System.Random.MWC.Probability as MWC.Probability
import Statistics.Distribution ( ContDistr(quantile), ContGen(genContVar), DiscreteDistr(..) )
import Statistics.Distribution.Normal ( normalDistr )
import Statistics.Distribution.Uniform ( uniformDistr )
import Statistics.Distribution.Binomial ( binomial )
import Statistics.Distribution.Poisson ( poisson )
import Statistics.Distribution.Beta ( betaDistr )
import Statistics.Distribution.Gamma ( gammaDistr )
import Statistics.Distribution.CauchyLorentz ( cauchyDistribution )
import System.Random.MWC ( initialize )
import Util

-- | Sampler type, for running IO computations alongside a random number generator
newtype Sampler a = Sampler {runSampler :: ReaderT MWC.GenIO IO a}
  deriving (Functor, Applicative, Monad)

-- | Lift an @IO@ computation into @Sampler@
liftIO :: IO a -> Sampler a
liftIO f = Sampler $ lift f

-- | Takes a @Sampler@, provides it a random generator, and runs the sampler in the @IO@ context
sampleIO :: Sampler a -> IO a
sampleIO m = MWC.createSystemRandom >>= (runReaderT . runSampler) m

-- | Takes a @Sampler@, provides it a fixed generator, and runs the sampler in the @IO@ context
sampleIOFixed :: Sampler a -> IO a
sampleIOFixed m = MWC.create >>= (runReaderT . runSampler) m

-- | Takes a @Sampler@, provides it a custom fixed generator, and runs the sampler in the @IO@ context
sampleIOCustom :: Int -> Sampler a -> IO a
sampleIOCustom n m = initialize (V.singleton (fromIntegral n :: Word32)) >>= (runReaderT . runSampler) m

-- | Takes a distribution which awaits a generator, and returns a @Sampler@
mkSampler :: (MWC.GenIO -> IO a) -> Sampler a
mkSampler f = Sampler $ ask >>= lift . f

{- $Raw-sampling
  Given their distribution parameters, these functions await a generator and
  then sample a value from the distribution in the @IO@ monad.
-}

sampleRandom
  :: Sampler Double
sampleRandom = mkSampler MWC.uniform

sampleCauchy
  :: Double -- ^ location
  -> Double -- ^ scale
  -> Sampler Double
sampleCauchy μ σ = mkSampler $ genContVar (cauchyDistribution μ σ)

sampleNormal
  :: Double -- ^ mean
  -> Double -- ^ standard deviation
  -> Sampler Double
sampleNormal μ σ = mkSampler $ MWC.Dist.normal μ σ

sampleUniform
  :: Double -- ^ lower-bound
  -> Double -- ^ upper-bound
  -> Sampler Double
sampleUniform min max = mkSampler $ MWC.uniformR (min, max)

sampleUniformD
  :: Int -- ^ lower-bound
  -> Int -- ^ upper-bound
  -> Sampler Int
sampleUniformD min max = mkSampler $ MWC.uniformR (min, max)

sampleGamma
  :: Double -- ^ shape k
  -> Double -- ^ scale θ
  -> Sampler Double
sampleGamma k θ = mkSampler $ MWC.Dist.gamma k θ

sampleBeta
  :: Double -- ^ shape α
  -> Double -- ^ shape β
  -> Sampler Double
sampleBeta α β = mkSampler $ MWC.Dist.beta α β

sampleBernoulli
  :: Double -- ^ probability of @True@
  -> Sampler Bool
sampleBernoulli p = mkSampler $ MWC.Dist.bernoulli p

sampleBinomial
  :: Int    -- ^ number of trials
  -> Double -- ^ probability of successful trial
  -> Sampler Int
sampleBinomial n p = mkSampler $ (length . filter (== True) <$> ) . (replicateM n . MWC.Dist.bernoulli p)

sampleCategorical
  :: V.Vector Double -- ^ probabilities
  -> Sampler Int
sampleCategorical ps = mkSampler $ MWC.Dist.categorical ps

sampleDiscrete
  :: [(a, Double)] -- ^ probabilities
  -> Sampler a
sampleDiscrete xps = mkSampler (MWC.Dist.categorical (V.fromList ps)) <&> (xs !!)
  where (xs, ps) = unzip xps

samplePoisson
  :: Double   -- ^ rate λ
  -> Sampler Int
samplePoisson λ = mkSampler $ MWC.Probability.sample (MWC.Probability.poisson λ)

sampleDirichlet
  :: [Double] -- ^ concentrations
  -> Sampler [Double]
sampleDirichlet xs = mkSampler $ MWC.Dist.dirichlet xs
