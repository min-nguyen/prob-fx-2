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

type MRec env = OP.OpenProduct (Maybes env)

newtype Model env ts a =
  Model { runModel :: (Member Dist ts, Member (AffReader env) ts, Member Sample ts) => Freer ts a }
  deriving Functor

newtype Model' env ts a =
  Model' { runModel' :: (Member Dist ts, Member (AffReader  env) ts, Member Sample ts) => Freer ts a }
  deriving Functor

instance Applicative (Model env ts) where
  pure = Model . pure
  (<*>) = ap

instance Monad (Model env ts) where
  return = pure
  Model f >>= k = Model $ do
    f' <- f
    runModel $ k f'

printM :: Member Sample ts => String -> Model env ts ()
printM = Model . prinT

putM :: Member (State s) ts => s -> Model env ts ()
putM = Model . put

getM :: Member (State s) ts => Model env ts s
getM = Model get

modifyM :: Member (State s) ts => (s -> s) -> Model env ts ()
modifyM = Model . modify

runStateM :: Model env (State [Int]:ts) a -> Model env ts (a, [Int])
runStateM m = Model $ runState [] $ runModel m

normalLens :: forall env ts a k. (a ~ Double) => OP.Lookup env k a
  => Double -> Double -> OP.Var k
  -> Lens' (OP.OpenProduct env) a
normalLens mu sigma field =
  lens (\env -> OP.getOP field env) (\env b -> OP.setOP field b env)

{- Distribution smart constructors -}

deterministic :: (Eq a, Show a, OpenSum.Member a PrimVal) => a -> Model env ts a
deterministic x = Model $ do
  send (DeterministicDist x Nothing Nothing)

deterministic' :: forall env ts a k. (Eq a, Show a, OpenSum.Member a PrimVal) => OP.Lookup (OP.AsList env) k [a]
  => a -> OP.Var k -> Model env ts a
deterministic' x field = Model $ do
  let tag = Just $ OP.varToStr field
  maybe_y <- ask @env field
  send (DeterministicDist x maybe_y tag)

dirichlet :: [Double] -> Model env ts [Double]
dirichlet xs = Model $ do
  send (DirichletDist xs Nothing Nothing)

dirichlet' :: forall env ts a k. (a ~ [Double]) => OP.Lookup (OP.AsList env) k [a]
  => [Double] -> OP.Var k
  -> Model env ts [Double]
dirichlet' xs field = Model $ do
  let tag = Just $ OP.varToStr field
  maybe_y <- ask @env field
  send (DirichletDist xs maybe_y tag)

discrete :: [Double] -> Model env ts Int
discrete xs = Model $ do
  send (DiscreteDist xs Nothing Nothing)

discrete' :: forall env ts a k. (a ~ Int) => OP.Lookup (OP.AsList env) k [a]
  => [Double] -> OP.Var k
  -> Model env ts Int
discrete' xs field = Model $ do
  let tag = Just $ OP.varToStr field
  maybe_y <- ask @env field
  send (DiscreteDist xs maybe_y tag)

categorical :: (Eq a, Show a, OpenSum.Member a PrimVal) => [(a, Double)] -> Model env ts a
categorical xs = Model $ do
  send (CategoricalDist xs Nothing Nothing)

categorical' :: forall env ts a k. (Eq a, Show a, OpenSum.Member a PrimVal) => OP.Lookup (OP.AsList env) k [a]
  => [(a, Double)] -> OP.Var k
  -> Model env ts a
categorical' xs field = Model $ do
  let tag = Just $ OP.varToStr field
  maybe_y <- ask @env field
  send (CategoricalDist xs maybe_y tag)

normal :: Double -> Double -> Model env ts Double
normal mu sigma = Model $ do
  send (NormalDist mu sigma Nothing Nothing)

normal' :: forall env ts a k. (a ~ Double) => OP.Lookup (OP.AsList env) k [a]
  => Double -> Double -> OP.Var k
  -> Model env ts Double
normal' mu sigma field = Model $ do
  let tag = Just $ OP.varToStr field
  maybe_y <- ask @env field
  send (NormalDist mu sigma maybe_y tag)

halfNormal :: Double -> Model env ts Double
halfNormal sigma = Model $ do
  send (HalfNormalDist sigma Nothing Nothing)

halfNormal' :: forall env ts a k. (a ~ Double) => OP.Lookup (OP.AsList env) k [a]
  => Double -> OP.Var k
  -> Model env ts Double
halfNormal' sigma field = Model $ do
  let tag = Just $ OP.varToStr field
  maybe_y <- ask @env field
  send (HalfNormalDist sigma maybe_y tag)

cauchy :: Double -> Double -> Model env ts Double
cauchy mu sigma = Model $ do
  send (CauchyDist mu sigma Nothing Nothing)

cauchy' :: forall env ts a k. (a ~ Double) => OP.Lookup (OP.AsList env) k [a]
  => Double -> Double -> OP.Var k
  -> Model env ts Double
cauchy' mu sigma field = Model $ do
  let tag = Just $ OP.varToStr field
  maybe_y <- ask @env field
  send (CauchyDist mu sigma maybe_y tag)

halfCauchy :: Double -> Model env ts Double
halfCauchy sigma = Model $ do
  send (HalfCauchyDist sigma Nothing Nothing)

halfCauchy' :: forall env ts a k. (a ~ Double) => OP.Lookup (OP.AsList env) k [a]
  => Double -> OP.Var k
  -> Model env ts Double
halfCauchy' sigma field = Model $ do
  let tag = Just $ OP.varToStr field
  maybe_y <- ask @env field
  send (HalfCauchyDist sigma maybe_y tag)

bernoulli :: Double -> Model env ts Bool
bernoulli p = Model $ do
  send (BernoulliDist p Nothing Nothing)

bernoulli' :: forall env ts a k. (a ~ Bool) => OP.Lookup (OP.AsList env) k [a]
  => Double -> OP.Var k
  -> Model env ts Bool
bernoulli' p field = Model $ do
  let tag = Just $ OP.varToStr field
  maybe_y <- ask @env field
  send (BernoulliDist p maybe_y tag)

binomial :: Int -> Double -> Model env ts Int
binomial n p = Model $ do
  send (BinomialDist n p Nothing Nothing)

binomial' :: forall env ts a k. (a ~  Int) => (OP.Lookup (OP.AsList env) k [a])
  => Int -> Double -> OP.Var k
  -> Model env ts Int
binomial' n p field = Model $ do
  let tag = Just $ OP.varToStr field
  maybe_y <- ask @env field
  send (BinomialDist n p maybe_y tag)

gamma :: Double -> Double -> Model env ts Double
gamma k θ = Model $ do
  send (GammaDist k θ Nothing Nothing)

gamma' :: forall env ts a k. (a ~ Double) => (OP.Lookup (OP.AsList env) k [a])
  => Double -> Double -> OP.Var k
  -> Model env ts Double
gamma' k θ field = Model $ do
  let tag = Just $ OP.varToStr field
  maybe_y <- ask @env field
  send (GammaDist k θ maybe_y tag)

beta :: Double -> Double -> Model env ts Double
beta α β = Model $ do
  send (BetaDist α β Nothing Nothing)

beta' :: forall env ts a k. (a ~ Double) => (OP.Lookup (OP.AsList env) k [a])
  => Double -> Double -> OP.Var k
  -> Model env ts Double
beta' α β field = Model $ do
  let tag = Just $ OP.varToStr field
  maybe_y <- ask @env field
  send (BetaDist α β maybe_y tag)

uniform :: Double -> Double -> Model env ts Double
uniform min max = Model $ do
  send (UniformDist min max Nothing Nothing)

uniform' :: forall env ts a k. (a ~ Double) => (OP.Lookup (OP.AsList env) k [a])
  => Double -> Double -> OP.Var k
  -> Model env ts Double
uniform' min max field = Model $ do
  let tag = Just $ OP.varToStr field
  maybe_y <- ask @env field
  send (UniformDist min max maybe_y tag)

poisson :: Double -> Model env ts Int
poisson λ = Model $ do
  send (PoissonDist λ Nothing Nothing)

poisson' :: forall env ts a k. (a ~ Int) => (OP.Lookup (OP.AsList env) k [a])
  => Double -> OP.Var k
  -> Model env ts Int
poisson' λ field = Model $ do
  let tag = Just $ OP.varToStr field
  maybe_y <- ask @env field
  send (PoissonDist λ maybe_y tag)

-- -- fromFieldOptic :: Lookup (AsList env) k a => KnownSymbol k => Extensible f p t =>
-- --   FieldOptic k -> Lens' (AsList env :& Field Identity) a
-- -- fromFieldOptic l = l

-- -- fromFieldOptic :: -- Lookup (AsList env) k a =>
-- --   forall  k xs.
-- --   (Extensible Identity (->) (:&)
-- --   , ExtensibleConstr (:&) xs (Field Identity) (k ':> Double)
-- --   , Lookup xs k Double
-- --   , Labelling k (->)
-- --   , Wrapper Identity)
-- --   => FieldOptic k -> Lens' (Record xs) Double
-- -- fromFieldOptic l = l

-- -- toFieldName :: forall env k a. KnownSymbol k => Lookup (AsList env) k a =>
-- --   FieldOptic k -> String -- Lens' (AsList env :& Field Identity) a
-- -- toFieldName l = symbolVal (Proxy @k)
