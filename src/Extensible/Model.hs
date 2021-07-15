{-# LANGUAGE RankNTypes, GADTs, TypeApplications, FlexibleInstances, DerivingStrategies, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, PolyKinds, UndecidableSuperClasses, TemplateHaskell, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, OverloadedLabels, UndecidableInstances, FunctionalDependencies, TypeFamilyDependencies #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Extensible.Model where

import Util
import Extensible.Dist
import Extensible.Freer
-- import Extensible.Reader
import Extensible.AffineReader
import Extensible.Sampler
import Extensible.State
import Extensible.IO
import GHC.Generics
import GHC.Types
import GHC.TypeLits
import Data.Maybe
import Data.Kind
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

-- mkField "infobs ρ β γ m c b μ σ mu sigma y ys label yObs weight bias obs_p trans_p"

type family Maybes (as :: [k]) = (bs :: [k]) | bs -> as where
  Maybes ((f , v) : as) = (f , Maybe v) : Maybes as
  Maybes (a : as) = Maybe a : Maybes as
  Maybes '[] = '[]

-- type family AsList (as :: [k]) = (bs :: [k]) | bs -> as where
--   -- AsList ((f :> [a]) : as) = ((f :> [a]) : AsList as)
--   AsList ((f :> a) : as)   = ((f :> [a]) : AsList as)
--   AsList '[] = '[]

-- type family AsList' a where
--   AsList' [a] = [a]
--   AsList' a   = [a]


-- HasVar: Lookup for maybe types
-- class OP.Lookup (AsList xs) k [v]  => HasVar xs k v where

-- instance OP.Lookup (AsList xs) k [v] => HasVar xs k v where

type MRec s = OP.OpenProduct (Maybes s)

{-
Idea : we can use State instead of Reader for the environment of observable variables.
This lets us use affine types in a way. When a variable is consumed by being conditioned against, we replace its value in the environment with Nothing. When a variable corresponds to a list of values, conditioning against it means removing the head element from the list and writing back the tail of the list. When the tail is empty, we replace the variable's value with Nothing.
  or:
We convert all the environment values to lists if they aren't already.
  or:
We can use reader on the user end, but then transform this during inference to use the state effect.
-}

{-
Idea : We can introduce print statements by adding them as a constructor of Sample
-}

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

normal :: Double -> Double -> Model s es Double
normal mu sigma = Model $ do
  send (NormalDist mu sigma Nothing)

normalLens :: forall s es a k. (a ~ Double) => OP.Lookup s k a
  => Double -> Double -> OP.Key k
  -> Lens' (OP.OpenProduct s) a
normalLens mu sigma field =
  lens (\s -> OP.getOP field s) (\s b -> OP.setOP field b s)

-- keyLens
normal' :: forall s es a k. (a ~ Double) => OP.Lookup (OP.AsList s) k [a]
  => Double -> Double -> OP.Key k
  -> Model s es Double
normal' mu sigma field = Model $ do
  let (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (NormalDist mu sigma maybe_y)

bernoulli :: Double -> Model s es Bool
bernoulli p = Model $ do
  send (BernoulliDist p Nothing)

bernoulli' :: forall s es a k. (a ~ Bool) => OP.Lookup (OP.AsList s) k [a]
  => Double -> OP.Key k
  -> Model s es Bool
bernoulli' p field = Model $ do
  let (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (BernoulliDist p maybe_y)

binomial :: Int -> Double -> Model s es Int
binomial n p = Model $ do
  send (BinomialDist n p Nothing)

binomial' :: forall s es a k. (a ~  Int) => (OP.Lookup (OP.AsList s) k [a])
  => Int -> Double -> OP.Key k
  -> Model s es Int
binomial' n p field = Model $ do
  let (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (BinomialDist n p maybe_y)

gamma :: Double -> Double -> Model s es Double
gamma k θ = Model $ do
  send (GammaDist k θ Nothing)

gamma' :: forall s es a k. (a ~ Double) => (OP.Lookup (OP.AsList s) k [a])
  => Double -> Double -> OP.Key k
  -> Model s es Double
gamma' k θ field = Model $ do
  let (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (GammaDist k θ maybe_y)

beta :: Double -> Double -> Model s es Double
beta α β = Model $ do
  send (BetaDist α β Nothing)

beta' :: forall s es a k. (a ~ Double) => (OP.Lookup (OP.AsList s) k [a])
  => Double -> Double -> OP.Key k
  -> Model s es Double
beta' α β field = Model $ do
  let (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (BetaDist α β maybe_y)

uniform :: Double -> Double -> Model s es Double
uniform min max = Model $ do
  send (UniformDist min max Nothing)

uniform' :: forall s es a k. (a ~ Double) => (OP.Lookup (OP.AsList s) k [a])
  => Double -> Double -> OP.Key k
  -> Model s es Double
uniform' min max field = Model $ do
  let (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (UniformDist min max maybe_y)

poisson :: Double -> Model s es Int
poisson λ = Model $ do
  send (PoissonDist λ Nothing)

poisson' :: forall s es a k. (a ~ Int) => (OP.Lookup (OP.AsList s) k [a])
  => Double -> OP.Key k
  -> Model s es Int
poisson' λ field = Model $ do
  let (getter, setter) = OP.mkGetterSetter field :: (Getting [a] (OP.OpenProduct (OP.AsList s)) [a], ASetter (OP.OpenProduct (OP.AsList s)) (OP.OpenProduct (OP.AsList s)) [a] [a])
  maybe_y <- ask getter setter
  send (PoissonDist λ maybe_y)

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
