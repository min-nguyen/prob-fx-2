{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

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
  , sampleRandomFrom
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

import           Control.Monad ( replicateM, when, (>=>) )
import           Control.Monad.Trans (MonadIO, MonadTrans, lift)
import           Control.Monad.Trans.Reader (ReaderT (..), ask, mapReaderT, runReaderT)
import           Data.Map (Map)
import           Data.Functor ( (<&>) )
import qualified Data.Vector as V
import           GHC.Word ( Word32 )
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC.Dist
import qualified System.Random.MWC.Probability as MWC.Probability
import           Statistics.Distribution ( ContDistr(quantile), ContGen(genContVar), DiscreteDistr(..) )
import           Statistics.Distribution.Normal ( normalDistr )
import           Statistics.Distribution.Uniform ( uniformDistr )
import           Statistics.Distribution.Binomial ( binomial )
import           Statistics.Distribution.Poisson ( poisson )
import           Statistics.Distribution.Beta ( betaDistr )
import           Statistics.Distribution.Gamma ( gammaDistr )
import           Statistics.Distribution.CauchyLorentz ( cauchyDistribution )
import           System.Random.MWC ( initialize )

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

sampleRandomFrom
  :: [a] -> Sampler a
sampleRandomFrom xs = sampleUniformD 0 (length xs - 1) >>= pure . (xs !!)

sampleCauchy
  :: Double -- ^ location
  -> Double -- ^ scale
  -> Sampler Double
sampleCauchy ?? ?? = mkSampler $ genContVar (cauchyDistribution ?? ??)

sampleNormal
  :: Double -- ^ mean
  -> Double -- ^ standard deviation
  -> Sampler Double
sampleNormal ?? ?? = mkSampler $ MWC.Dist.normal ?? ??

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
  -> Double -- ^ scale ??
  -> Sampler Double
sampleGamma k ?? = mkSampler $ MWC.Dist.gamma k ??

sampleBeta
  :: Double -- ^ shape ??
  -> Double -- ^ shape ??
  -> Sampler Double
sampleBeta ?? ?? = mkSampler $ MWC.Dist.beta ?? ??

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
  :: Double   -- ^ rate ??
  -> Sampler Int
samplePoisson ?? = mkSampler $ MWC.Probability.sample (MWC.Probability.poisson ??)

sampleDirichlet
  :: [Double] -- ^ concentrations
  -> Sampler [Double]
sampleDirichlet xs = mkSampler $ MWC.Dist.dirichlet xs

{- $Inverse-sampling
  Given a random double @r@ between 0 and 1, this is passed to a distribution's inverse
  cumulative density function to draw a sampled value.

{- Continuous cases.
-}

invCDF
  :: ContDistr d
  => d
  -> Double
  -> Sampler Double
invCDF d = pure . quantile d

sampleCauchyInv
  :: Double -- ^ location
  -> Double -- ^ scale
  -> Double -- ^ r
  -> Sampler Double
sampleCauchyInv ?? ?? = invCDF (cauchyDistribution ?? ??)

sampleNormalInv
  :: Double -- ^ mean
  -> Double -- ^ std
  -> Double -- ^ r
  -> Sampler Double
sampleNormalInv ?? ?? = invCDF (normalDistr ?? ??)

sampleUniformInv
  :: Double -- ^ lower-bound
  -> Double -- ^ upper-bound
  -> Double -- ^ r
  -> Sampler Double
sampleUniformInv min max = invCDF (uniformDistr min max)

sampleUniformDInv
  :: Int    -- ^ lower-bound
  -> Int    -- ^ upper-bound
  -> Double -- ^ r
  -> Sampler Int
sampleUniformDInv min max = sampleUniformInv (fromIntegral min) (fromIntegral max + 1) >=> pure . floor

sampleGammaInv
  :: Double -- ^ shape k
  -> Double -- ^ shape ??
  -> Double -- ^ r
  -> Sampler Double
sampleGammaInv k ??  = invCDF (gammaDistr k ??)

sampleBetaInv
  :: Double -- ^ shape ??
  -> Double -- ^ shape ??
  -> Double -- ^ r
  -> Sampler Double
sampleBetaInv ?? ?? = invCDF (betaDistr ?? ??)

sampleDirichletInv
  :: [Double]  -- ^ concentrations
  -> Double    -- ^ r
  -> Sampler [Double]
sampleDirichletInv as r = do
  let rs = take (length as) (linCongGen r)
  xs <- mapM (\(a, r) -> sampleGammaInv a 1 r) (zip as rs)
  let ys = map (/sum xs) xs
  return ys

{- Discrete cases.
-}

invCMF
  :: (Int -> Double)  -- ^ probability mass function
  -> Double           -- ^ r
  -> Sampler Int
invCMF pmf r = pure (f 0 r)
  where
    f :: Int -> Double -> Int
    f i r = do
      let q  = pmf i
          r' = r - q
      if r' < 0 then i else f (i + 1) r'

sampleBernoulliInv
  :: Double -- ^ probability of @True@
  -> Double -- ^ r
  -> Sampler Bool
sampleBernoulliInv p r = pure $ r < p

sampleBinomialInv
  :: Int    -- ^ number of trials
  -> Double -- ^ probability of successful trial
  -> Double -- ^ r
  -> Sampler Int
sampleBinomialInv n p = invCMF (probability (binomial n p))

sampleCategoricalInv
  :: V.Vector Double
  -> Double
  -> Sampler Int
sampleCategoricalInv ps = invCMF (ps V.!)

sampleDiscreteInv
  :: [(a, Double)]
  -> Double
  -> Sampler a
sampleDiscreteInv xps r = invCMF (ps !!) r <&> (xs !!)
  where (xs, ps) = unzip xps

samplePoissonInv
  :: Double       -- ^ rate ??
  -> Double       -- ^ r
  -> Sampler Int
samplePoissonInv ?? = invCMF (probability (poisson ??))
 -}