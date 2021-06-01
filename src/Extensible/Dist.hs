{-# LANGUAGE GADTs, TypeOperators #-}

module Extensible.Dist where

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
