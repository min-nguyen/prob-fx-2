{-# LANGUAGE RankNTypes, GADTs, TypeApplications, FlexibleInstances, DerivingStrategies, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, PolyKinds, UndecidableSuperClasses, TemplateHaskell, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, OverloadedLabels, UndecidableInstances, FunctionalDependencies, TypeFamilyDependencies #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model where

import Util
import Dist
import Freer
-- import Reader
import ObsReader
import OpenSum (OpenSum)
import qualified OpenSum as OpenSum
import Sampler
import State ( State, get, put, modify, runState )
import Writer
import IO
import GHC.Generics
import GHC.Types
import GHC.TypeLits
import Data.Maybe
import Data.Kind
import Data.Proxy
import Data.Profunctor.Unsafe
-- import Data.Extensible hiding (wrap, Head, Member)
import ModelEnv
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

type family Maybes (as :: [x]) = (bs :: [x]) | bs -> as where
  Maybes ((f , v) : as) = (f , Maybe v) : Maybes as
  Maybes (v : as) = Maybe v : Maybes as
  Maybes '[] = '[]

newtype Model env es v =
  Model { runModel :: (Member Dist es, Member (ObsReader env) es) => Prog es v }
  deriving Functor

instance Applicative (Model env es) where
  pure x = Model $ pure x
  (<*>) = ap

instance Monad (Model env es) where
  return = pure
  Model f >>= x = Model $ do
    f' <- f
    runModel $ x f'

printS :: Show a => a -> Sampler ()
printS x = liftS $ print x

printM :: Member Sample es => String -> Model env es ()
printM x = Model $ prinT x

tellM :: Member (Writer w) es => w -> Model env es ()
tellM w = Model $ tell w

runWriterM :: forall w env es v. Monoid w => Model env (Writer w : es) v -> Model env es (v, w)
runWriterM m = Model $ runWriter $ runModel m

putM :: Member (State s) es => s -> Model env es ()
putM x = Model $ put x

getM :: Member (State s) es => Model env es s
getM = Model get

modifyM :: Member (State s) es => (s -> s) -> Model env es ()
modifyM f = Model $ modify f

runStateM :: Model env (State [Int] : es) v -> Model env es (v, [Int])
runStateM m = Model $ runState [] $ runModel m

normalLens :: forall env es x.  Observable env x Double
  => Double -> Double -> ObsVar x
  -> Lens' (ModelEnv env) [Double]
normalLens mu sigma field =
  lens (\env -> getOP field env) (\env b -> setOP field b env)

{- Distribution smart constructors -}

deterministic :: (Eq v, Show v, OpenSum.Member v PrimVal)
  => v -> Model env es v
deterministic x = Model $ do
  send (DeterministicDist x Nothing Nothing)

deterministic' :: forall env es v x. (Eq v, Show v, OpenSum.Member v PrimVal)
  => Observable env x v
  => v -> ObsVar x -> Model env es v
deterministic' x field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (DeterministicDist x maybe_y tag)

dirichlet :: [Double] -> Model env es [Double]
dirichlet xs = Model $ do
  send (DirichletDist xs Nothing Nothing)

dirichlet' :: forall env es x. Observable env x [Double]
  => [Double] -> ObsVar x
  -> Model env es [Double]
dirichlet' xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (DirichletDist xs maybe_y tag)

discrete :: [Double] -> Model env es Int
discrete xs = Model $ do
  send (DiscreteDist xs Nothing Nothing)

discrete' :: forall env es x. Observable env x Int
  => [Double] -> ObsVar x
  -> Model env es Int
discrete' xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (DiscreteDist xs maybe_y tag)

categorical :: (Eq v, Show v, OpenSum.Member v PrimVal) => [(v, Double)] -> Model env es v
categorical xs = Model $ do
  send (CategoricalDist xs Nothing Nothing)

categorical' :: forall env es v x. (Eq v, Show v, OpenSum.Member v PrimVal) => Observable env x v
  => [(v, Double)] -> ObsVar x
  -> Model env es v
categorical' xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (CategoricalDist xs maybe_y tag)

normal :: Double -> Double -> Model env es Double
normal mu sigma = Model $ do
  send (NormalDist mu sigma Nothing Nothing)

normal' :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
normal' mu sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (NormalDist mu sigma maybe_y tag)

halfNormal :: Double -> Model env es Double
halfNormal sigma = Model $ do
  send (HalfNormalDist sigma Nothing Nothing)

halfNormal' :: forall env es x. Observable env x Double
  => Double -> ObsVar x
  -> Model env es Double
halfNormal' sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (HalfNormalDist sigma maybe_y tag)

cauchy :: Double -> Double -> Model env es Double
cauchy mu sigma = Model $ do
  send (CauchyDist mu sigma Nothing Nothing)

cauchy' :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
cauchy' mu sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (CauchyDist mu sigma maybe_y tag)

halfCauchy :: Double -> Model env es Double
halfCauchy sigma = Model $ do
  send (HalfCauchyDist sigma Nothing Nothing)

halfCauchy' :: forall env es x. Observable env x Double
  => Double -> ObsVar x
  -> Model env es Double
halfCauchy' sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (HalfCauchyDist sigma maybe_y tag)

bernoulli :: Double -> Model env es Bool
bernoulli p = Model $ do
  send (BernoulliDist p Nothing Nothing)

bernoulli' :: forall env es x. Observable env x Bool
  => Double -> ObsVar x
  -> Model env es Bool
bernoulli' p field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (BernoulliDist p maybe_y tag)

binomial :: Int -> Double -> Model env es Int
binomial n p = Model $ do
  send (BinomialDist n p Nothing Nothing)

binomial' :: forall env es x. Observable env x Int
  => Int -> Double -> ObsVar x
  -> Model env es Int
binomial' n p field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (BinomialDist n p maybe_y tag)

gamma :: Double -> Double -> Model env es Double
gamma x θ = Model $ do
  send (GammaDist x θ Nothing Nothing)

gamma' :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
gamma' x θ field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (GammaDist x θ maybe_y tag)

beta :: Double -> Double -> Model env es Double
beta α β = Model $ do
  send (BetaDist α β Nothing Nothing)

beta' :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
beta' α β field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (BetaDist α β maybe_y tag)

uniform :: Double -> Double -> Model env es Double
uniform min max = Model $ do
  send (UniformDist min max Nothing Nothing)

uniform' :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
uniform' min max field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (UniformDist min max maybe_y tag)

poisson :: Double -> Model env es Int
poisson λ = Model $ do
  send (PoissonDist λ Nothing Nothing)

poisson' :: forall env es x. Observable env x Int
  => Double -> ObsVar x
  -> Model env es Int
poisson' λ field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (PoissonDist λ maybe_y tag)

-- fromFieldOptic :: Observable (AsList env) x v => KnownSymbol x => Extensible f p t =>
--   FieldOptic x -> Lens' (AsList env :& Field Identity) v
-- fromFieldOptic l = l

-- fromFieldOptic :: -- Observable (AsList env) x v =>
--   forall  x xs.
--   (Extensible Identity (->) (:&)
--   , ExtensibleConstr (:&) xs (Field Identity) (x ':> Double)
--   , Observable xs x Double
--   , Labelling x (->)
--   , Wrapper Identity)
--   => FieldOptic x -> Lens' (Record xs) Double
-- fromFieldOptic l = l

-- toFieldName :: forall env x v. KnownSymbol x => Observable (AsList env) x v =>
--   FieldOptic x -> String -- Lens' (AsList env :& Field Identity) v
-- toFieldName l = symbolVal (Proxy @x)
