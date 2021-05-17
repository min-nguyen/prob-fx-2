{-# LANGUAGE GADTs #-}

module Dist where

import qualified Data.Vector as V

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
  BinomialDist      :: Int    -> Double -> Maybe Int -> ([Bool] -> a) -> Dist a
  -- bernoulli      :: Dist Bool
  BernoulliDist     :: Double -> Maybe Bool -> (Bool -> a) -> Dist a
  -- categorical    :: Dist Int
  CategoricalDist   :: V.Vector (Int, Double) -> Maybe Int -> (Int -> a) -> Dist a
  -- discrete       :: Dist Int
  DiscreteDist      :: [(Int, Double)] -> Maybe Int -> (Int -> a) -> Dist a

instance Functor Dist where
  fmap f (NormalDist mu sigma y g) = NormalDist mu sigma y (f . g)
  fmap f (UniformDist min max y g) = UniformDist min max y (f . g)
  fmap f (DiscrUniformDist min max y g) = DiscrUniformDist min max y (f . g)
  fmap f (GammaDist a b y g) = GammaDist a b y (f . g)
  fmap f (BinomialDist n p y g) = BinomialDist n p y (f . g)
  fmap f (BernoulliDist p y g) = BernoulliDist p y (f . g)