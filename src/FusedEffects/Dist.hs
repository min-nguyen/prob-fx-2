{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications, UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module FusedEffects.Dist where

import qualified Data.Vector as Vec
import Control.Monad.Bayes.Class
import Control.Monad
import FusedEffects.Algebra
import FusedEffects.Sum
import Env
import Util
import OpenSum (OpenSum)
import qualified OpenSum as OpenSum

-- | Effect
type PrimVal = '[Int, Double, [Double], Bool, String]

data Dist m a where
  HalfCauchyDist    :: Double -> Maybe Double -> Maybe String -> Dist m Double
  CauchyDist        :: Double -> Double -> Maybe Double -> Maybe String -> Dist m Double
  NormalDist        :: Double -> Double -> Maybe Double -> Maybe String -> Dist m Double
  HalfNormalDist    :: Double -> Maybe Double -> Maybe String -> Dist m Double
  UniformDist       :: Double -> Double -> Maybe Double -> Maybe String -> Dist m Double
  DiscrUniformDist  :: Int    -> Int    -> Maybe Int -> Maybe String -> Dist m Int
  GammaDist         :: Double -> Double -> Maybe Double -> Maybe String -> Dist m Double
  BetaDist          :: Double -> Double -> Maybe Double -> Maybe String ->  Dist m Double
  BinomialDist      :: Int    -> Double -> Maybe Int -> Maybe String -> Dist m Int
  BernoulliDist     :: Double -> Maybe Bool -> Maybe String -> Dist m Bool
  CategoricalDist   :: (Eq a, Show a, OpenSum.Member a PrimVal) => [(a, Double)] -> Maybe a -> Maybe String -> Dist m a
  DiscreteDist      :: [Double] -> Maybe Int -> Maybe String -> Dist m Int
  PoissonDist       :: Double -> Maybe Int -> Maybe String -> Dist m Int
  DirichletDist     :: [Double] -> Maybe [Double] -> Maybe String -> Dist m [Double]
  DeterministicDist :: (Eq a, Show a, OpenSum.Member a PrimVal) => a -> Maybe a -> Maybe String -> Dist m a

-- | Carrier
newtype DistC m a = DistC (MonadInfer m => m a) deriving (Functor)

runDistC :: MonadInfer m => DistC m a -> m a
runDistC (DistC m) = m

instance Monad m => Applicative (DistC m) where 
  pure x              = DistC $ pure x
  DistC x <*> DistC y = DistC (x <*> y)

instance (MonadInfer m, Monad m) => Monad (DistC m) where 
  DistC x >>= f = DistC (x >>= (runDistC . f))

-- | Algebra
instance (MonadInfer m, Algebra sig m) => Algebra (Dist :+: sig) (DistC m) where
  alg :: (MonadInfer m, Functor ctx)
      => (forall x. ctx (n x) -> DistC m (ctx x))
      -> (Dist :+: sig) n a
      -> ctx () 
      -> DistC m (ctx a)
  alg hdl sig ctx = DistC $ case sig of
    L d -> case getObs d of
        (Just y) -> undefined
    R other   -> alg (runDistC . hdl) other ctx
  {-# INLINE alg #-}

getObs :: Dist m a -> Maybe a
getObs d@(HalfCauchyDist _ obs _)     =  obs
getObs d@(CauchyDist _ _ obs _)       =  obs
getObs d@(HalfNormalDist _ obs _)     =  obs
getObs d@(NormalDist _ _ obs _)       =  obs
getObs d@(DiscrUniformDist _ _ obs _) =  obs
getObs d@(UniformDist _ _ obs _)      =  obs
getObs d@(GammaDist _ _ obs _)        =  obs
getObs d@(BetaDist _ _ obs _)         =  obs
getObs d@(BinomialDist _ _ obs _)     =  obs
getObs d@(BernoulliDist _ obs _)      =  obs
getObs d@(CategoricalDist _ obs _)    =  obs
getObs d@(DiscreteDist _ obs _)       =  obs
getObs d@(PoissonDist _ obs _)        =  obs
getObs d@(DirichletDist _ obs _)      =  obs
getObs d@(DeterministicDist _ obs _)  =  obs


sampleBayes :: MonadSample m => Dist m a -> m a
sampleBayes (UniformDist a b _ _)     = uniform a b
sampleBayes (DiscreteDist as _ _)     = categorical (Vec.fromList as)
sampleBayes (CategoricalDist as _ _)  = categorical (Vec.fromList (map snd as)) >>= (pure . fst . (as !!))
sampleBayes (NormalDist mu std _ _)   = normal mu std
sampleBayes (GammaDist k t _ _)       = gamma k t
sampleBayes (BetaDist a b _ _)        = beta a b
sampleBayes (BernoulliDist p _ _)     = bernoulli p
sampleBayes (BinomialDist n p _ _)    = replicateM n (bernoulli p) >>= (pure . length . filter (== True))
sampleBayes (PoissonDist l _ _)       = poisson l
sampleBayes (DirichletDist as _ _)    = dirichlet (Vec.fromList as) >>= pure . Vec.toList
