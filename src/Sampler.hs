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
  -- ** Inverse CDF sampling
  -- $Inverse-sampling
  ) where

import Control.Monad ( replicateM, when, (>=>) )
import Control.Monad.Trans (MonadIO, MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT (..), ask, mapReaderT, runReaderT)
import Data.Map (Map)
import GHC.Word ( Word32 )
import GSL.Random.Dist
import qualified Data.Vector as V
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC.Dist
import qualified System.Random.MWC.Probability as MWC.Probability
import Statistics.Distribution ( ContGen(genContVar) )
import Statistics.Distribution.CauchyLorentz ( cauchyDistribution )
import System.Random.MWC ( initialize )

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
  -> Sampler [Bool]
sampleBinomial n p = mkSampler $ replicateM n . MWC.Dist.bernoulli p

sampleCategorical
  :: V.Vector Double -- ^ probabilities
  -> Sampler Int
sampleCategorical ps = mkSampler $ MWC.Dist.categorical ps

sampleDiscrete
  :: [Double] -- ^ probabilities
  -> Sampler Int
sampleDiscrete ps = mkSampler $ MWC.Dist.categorical (V.fromList ps)

samplePoisson
  :: Double   -- ^ rate λ
  -> Sampler Int
samplePoisson λ = mkSampler $ MWC.Probability.sample (MWC.Probability.poisson λ)

sampleDirichlet
  :: [Double] -- ^ concentrations
  -> Sampler [Double]
sampleDirichlet xs = mkSampler $ MWC.Dist.dirichlet xs

{- $Inverse-sampling
  Given a random double @r@ between 0 and 1, this is passed to a distribution's inverse
  cumulative density function to draw a sampled value.
-}

sampleCauchyInv
  :: Double -- ^ location
  -> Double -- ^ scale
  -> Double -- ^ r
  -> Sampler Double
sampleCauchyInv μ σ r = pure $ cauchyQInv (r - μ) σ

sampleNormalInv
  :: Double -- ^ mean
  -> Double -- ^ std
  -> Double -- ^ r
  -> Sampler Double
sampleNormalInv μ σ r = pure $ gaussianQInv (r - μ) σ

sampleUniformInv
  :: Double -- ^ lower-bound
  -> Double -- ^ upper-bound
  -> Double -- ^ r
  -> Sampler Double
sampleUniformInv min max r = pure $ flatQInv r min max

sampleUniformDInv
  :: Int    -- ^ lower-bound
  -> Int    -- ^ upper-bound
  -> Double -- ^ r
  -> Sampler Int
sampleUniformDInv min max =
    sampleUniformInv (fromIntegral min) (fromIntegral max + 1) >=> pure . floor

sampleGammaInv
  :: Double -- ^ shape k
  -> Double -- ^ shape θ
  -> Double -- ^ r
  -> Sampler Double
sampleGammaInv α β r = pure $ gammaQInv r α β

g = 2 ** 64

sampleBetaInv
  :: Double -- ^ shape α
  -> Double -- ^ shape β
  -> Double -- ^ r
  -> Sampler Double
sampleBetaInv α β r = pure $ betaQInv r α β

sampleBernoulliInv
  :: Double -- ^ probability of @True@
  -> Double -- ^ r
  -> Sampler Bool
sampleBernoulliInv p r = pure $ r < p

-- Get FFI to binomialQInv
-- sampleBinomialInv
--   :: Int    -- ^ number of trials
--   -> Double -- ^ probability of successful trial
--   -> Double -- ^ r
--   -> Sampler [Bool]
-- sampleBinomialInv n p r = mkSampler $ replicateM n . MWC.Dist.bernoulli p

fromPMF
  :: (Int -> Double)
  -> r
  -> Sampler Int
fromPMF pmf r = f 0 1
  where
    f i r = do
      when (r < 0) $ error "fromPMF: total PMF above 1"
      let q = pmf i
      when (q < 0 || q > 1) $ error "fromPMF: invalid probability value"
      b <- sampleBernoulliInv (q / r) r
      if b then pure i else f (i + 1) (r - q)
