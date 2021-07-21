{-# LANGUAGE RankNTypes, GADTs, TypeApplications, FlexibleInstances, DerivingStrategies, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, PolyKinds, UndecidableSuperClasses, TemplateHaskell, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, OverloadedLabels, UndecidableInstances, FunctionalDependencies, TypeFamilyDependencies #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Extensible.Model where

import Util
import Extensible.Dist
import Extensible.Freer
-- import Extensible.Reader
import Extensible.AffineReader
import Extensible.OpenSum (OpenSum)
import qualified Extensible.OpenSum as OpenSum
import Extensible.Sampler
import Extensible.State
import Extensible.IO
import GHC.Generics
import GHC.Types
import GHC.TypeLits
import Data.Maybe
import Data.Kind
import Data.Proxy
import Data.Profunctor.Unsafe
-- import Data.Extensible hiding (wrap, Head, Member)
import qualified Extensible.OpenProduct as OP
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Trans.Class ( MonadTrans(lift) )
-- import Control.Monad.Reader
import Statistics.Distribution
import Statistics.Distribution.DiscreteUniform
import Statistics.Distribution.Normal
import Statistics.Distribution.Gamma
import Statistics.Distribution.Beta
import Statistics.Distribution.Binomial
import Statistics.Distribution.Uniform
import System.Random.MWC
import qualified System.Random.MWC.Distributions as MWC
import qualified Data.Vector as V
import Unsafe.Coerce

type family Maybes (as :: [k]) = (bs :: [k]) | bs -> as where
  Maybes ((f , v) : as) = (f , Maybe v) : Maybes as
  Maybes (a : as) = Maybe a : Maybes as
  Maybes '[] = '[]

type MRec s = OP.OpenProduct (Maybes s)

newtype Model env es a =
  Model { runModel :: (Member Dist es, Member (AffReader (OP.AsList env)) es, Member Sample es) => Freer es a }
  deriving Functor

instance Applicative (Model env es) where
  pure = Model . pure
  (<*>) = ap

instance Monad (Model env es) where
  return = pure
  Model f >>= k = Model $ do
    f' <- f
    runModel $ k f'

printM :: Member Sample es => String -> Model env es ()
printM = Model . prinT

putM :: Member (State s) es => s -> Model env es ()
putM = Model . put

getM :: Member (State s) es => Model env es s
getM = Model get

modifyM :: Member (State s) es => (s -> s) -> Model env es ()
modifyM = Model . modify

runStateM :: Model env (State [Int]:es) a -> Model env es (a, [Int])
runStateM m = Model $ runState [] $ runModel m

normalLens :: forall s es a k. (a ~ Double) => OP.Lookup s k a
  => Double -> Double -> OP.Key k
  -> Lens' (OP.OpenProduct s) a
normalLens mu sigma field =
  lens (\s -> OP.getOP field s) (\s b -> OP.setOP field b s)

{- Distribution smart constructors -}

dirichlet :: [Double] -> Model s es [Double]
dirichlet xs = Model $ do
  send (DirichletDist xs Nothing Nothing)

dirichlet' :: forall s es a k. (a ~ [Double]) => OP.Lookup (OP.AsList s) k [a]
  => [Double] -> OP.Key k
  -> Model s es [Double]
dirichlet' xs field = Model $ do
  let tag = Just $ OP.keyToStr field
      (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (DirichletDist xs maybe_y tag)

discrete :: [Double] -> Model s es Int
discrete xs = Model $ do
  send (DiscreteDist xs Nothing Nothing)

discrete' :: forall s es a k. (a ~ Int) => OP.Lookup (OP.AsList s) k [a]
  => [Double] -> OP.Key k
  -> Model s es Int
discrete' xs field = Model $ do
  let tag = Just $ OP.keyToStr field
      (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (DiscreteDist xs maybe_y tag)

categorical :: (Eq a, Show a, OpenSum.Member a PrimVal) => [(a, Double)] -> Model s es a
categorical xs = Model $ do
  send (CategoricalDist xs Nothing Nothing)

categorical' :: forall s es a k. (Eq a, Show a, OpenSum.Member a PrimVal) => OP.Lookup (OP.AsList s) k [a]
  => [(a, Double)] -> OP.Key k
  -> Model s es a
categorical' xs field = Model $ do
  let tag = Just $ OP.keyToStr field
      (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (CategoricalDist xs maybe_y tag)

normal :: Double -> Double -> Model s es Double
normal mu sigma = Model $ do
  send (NormalDist mu sigma Nothing Nothing)

normal' :: forall s es a k. (a ~ Double) => OP.Lookup (OP.AsList s) k [a]
  => Double -> Double -> OP.Key k
  -> Model s es Double
normal' mu sigma field = Model $ do
  let tag = Just $ OP.keyToStr field
      (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (NormalDist mu sigma maybe_y tag)

halfNormal :: Double -> Model s es Double
halfNormal sigma = Model $ do
  send (HalfNormalDist sigma Nothing Nothing)

halfNormal' :: forall s es a k. (a ~ Double) => OP.Lookup (OP.AsList s) k [a]
  => Double -> OP.Key k
  -> Model s es Double
halfNormal' sigma field = Model $ do
  let tag = Just $ OP.keyToStr field
      (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (HalfNormalDist sigma maybe_y tag)

cauchy :: Double -> Double -> Model s es Double
cauchy mu sigma = Model $ do
  send (CauchyDist mu sigma Nothing Nothing)

cauchy' :: forall s es a k. (a ~ Double) => OP.Lookup (OP.AsList s) k [a]
  => Double -> Double -> OP.Key k
  -> Model s es Double
cauchy' mu sigma field = Model $ do
  let tag = Just $ OP.keyToStr field
      (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (CauchyDist mu sigma maybe_y tag)

halfCauchy :: Double -> Model s es Double
halfCauchy sigma = Model $ do
  send (HalfCauchyDist sigma Nothing Nothing)

halfCauchy' :: forall s es a k. (a ~ Double) => OP.Lookup (OP.AsList s) k [a]
  => Double -> OP.Key k
  -> Model s es Double
halfCauchy' sigma field = Model $ do
  let tag = Just $ OP.keyToStr field
      (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (HalfCauchyDist sigma maybe_y tag)

bernoulli :: Double -> Model s es Bool
bernoulli p = Model $ do
  send (BernoulliDist p Nothing Nothing)

bernoulli' :: forall s es a k. (a ~ Bool) => OP.Lookup (OP.AsList s) k [a]
  => Double -> OP.Key k
  -> Model s es Bool
bernoulli' p field = Model $ do
  let tag = Just $ OP.keyToStr field
      (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (BernoulliDist p maybe_y tag)

binomial :: Int -> Double -> Model s es Int
binomial n p = Model $ do
  send (BinomialDist n p Nothing Nothing)

binomial' :: forall s es a k. (a ~  Int) => (OP.Lookup (OP.AsList s) k [a])
  => Int -> Double -> OP.Key k
  -> Model s es Int
binomial' n p field = Model $ do
  let tag = Just $ OP.keyToStr field
      (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (BinomialDist n p maybe_y tag)

gamma :: Double -> Double -> Model s es Double
gamma k θ = Model $ do
  send (GammaDist k θ Nothing Nothing)

gamma' :: forall s es a k. (a ~ Double) => (OP.Lookup (OP.AsList s) k [a])
  => Double -> Double -> OP.Key k
  -> Model s es Double
gamma' k θ field = Model $ do
  let tag = Just $ OP.keyToStr field
      (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (GammaDist k θ maybe_y tag)

beta :: Double -> Double -> Model s es Double
beta α β = Model $ do
  send (BetaDist α β Nothing Nothing)

beta' :: forall s es a k. (a ~ Double) => (OP.Lookup (OP.AsList s) k [a])
  => Double -> Double -> OP.Key k
  -> Model s es Double
beta' α β field = Model $ do
  let tag = Just $ OP.keyToStr field
      (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (BetaDist α β maybe_y tag)

uniform :: Double -> Double -> Model s es Double
uniform min max = Model $ do
  send (UniformDist min max Nothing Nothing)

uniform' :: forall s es a k. (a ~ Double) => (OP.Lookup (OP.AsList s) k [a])
  => Double -> Double -> OP.Key k
  -> Model s es Double
uniform' min max field = Model $ do
  let tag = Just $ OP.keyToStr field
      (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (UniformDist min max maybe_y tag)

poisson :: Double -> Model s es Int
poisson λ = Model $ do
  send (PoissonDist λ Nothing Nothing)

poisson' :: forall s es a k. (a ~ Int) => (OP.Lookup (OP.AsList s) k [a])
  => Double -> OP.Key k
  -> Model s es Int
poisson' λ field = Model $ do
  let tag = Just $ OP.keyToStr field
      (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (PoissonDist λ maybe_y tag)

-- -- fromFieldOptic :: Lookup (AsList s) k a => KnownSymbol k => Extensible f p t =>
-- --   FieldOptic k -> Lens' (AsList s :& Field Identity) a
-- -- fromFieldOptic l = l

-- -- fromFieldOptic :: -- Lookup (AsList s) k a =>
-- --   forall  k xs.
-- --   (Extensible Identity (->) (:&)
-- --   , ExtensibleConstr (:&) xs (Field Identity) (k ':> Double)
-- --   , Lookup xs k Double
-- --   , Labelling k (->)
-- --   , Wrapper Identity)
-- --   => FieldOptic k -> Lens' (Record xs) Double
-- -- fromFieldOptic l = l

-- -- toFieldName :: forall s k a. KnownSymbol k => Lookup (AsList s) k a =>
-- --   FieldOptic k -> String -- Lens' (AsList s :& Field Identity) a
-- -- toFieldName l = symbolVal (Proxy @k)
