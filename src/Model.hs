{-# LANGUAGE RankNTypes, GADTs, TypeApplications, FlexibleInstances, DerivingStrategies, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, PolyKinds, UndecidableSuperClasses, TemplateHaskell, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, OverloadedLabels, UndecidableInstances, FunctionalDependencies, TypeFamilyDependencies #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model where

import Util
import Effects.Dist
import Prog
-- import Effects.Reader
import Effects.ObsReader
import OpenSum (OpenSum)
import qualified OpenSum as OpenSum
import Sampler
import Effects.State
import Effects.Writer
import Effects.Lift
import GHC.Generics
import GHC.Types
import GHC.TypeLits
import Data.Maybe
import Data.Kind
import Data.Proxy
import Data.Profunctor.Unsafe
-- import Data.Extensible hiding (wrap, Head, Member)
import Env
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

{- Transform multimodal model into program of samples and observes -}
handleCore :: Env env -> Model env (ObsReader env : Dist : es) a -> Prog (Observe : Sample : es) a
handleCore env m = (handleDist . handleObsRead env) (runModel m)

{- Wrap other effects and handlers into the Model type -}
-- | State
-- printS :: Member (Lift)

getStM :: (Member (State s) es) => Model env es s
getStM = Model getSt

modifyStM :: (Member (State s) es) => (s -> s) -> Model env es ()
modifyStM f = Model $ modify f

putStM :: (Member (State s) es) => s -> Model env es ()
putStM s = Model (putSt s)

handleStateM :: s -> Model env (State s : es) a -> Model env es (a, s)
handleStateM s m = Model $ handleState s $ runModel m

-- | Writer
tellM :: Member (Writer w) es => w -> Model env es ()
tellM w = Model $ tell w

handleWriterM :: Monoid w => Model env (Writer w : es) v -> Model env es (v, w)
handleWriterM m = Model $ handleWriter $ runModel m

-- | Lift
liftM :: forall es m env a. (Member (Lift m) es) => m a -> Model env es a
liftM m = Model $ call (Lift m)


normalLens :: forall env es x.  Observable env x Double
  => Double -> Double -> ObsVar x
  -> Lens' (Env env) [Double]
normalLens mu sigma field =
  lens (\env -> get field env) (\env b -> Env.set field b env)

{- Distribution smart constructors -}
deterministic' :: (Eq v, Show v, OpenSum.Member v PrimVal)
  => v -> Model env es v
deterministic' x = Model $ do
  call (DeterministicDist x Nothing Nothing)

deterministic :: forall env es v x. (Eq v, Show v, OpenSum.Member v PrimVal)
  => Observable env x v
  => v -> ObsVar x -> Model env es v
deterministic x field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (DeterministicDist x maybe_y tag)

dirichlet' :: [Double] -> Model env es [Double]
dirichlet' xs = Model $ do
  call (DirichletDist xs Nothing Nothing)

dirichlet :: forall env es x. Observable env x [Double]
  => [Double] -> ObsVar x
  -> Model env es [Double]
dirichlet xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (DirichletDist xs maybe_y tag)

discrete' :: [Double] -> Model env es Int
discrete' xs = Model $ do
  call (DiscreteDist xs Nothing Nothing)

discrete :: forall env es x. Observable env x Int
  => [Double] -> ObsVar x
  -> Model env es Int
discrete xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (DiscreteDist xs maybe_y tag)

categorical' :: (Eq v, Show v, OpenSum.Member v PrimVal) => [(v, Double)] -> Model env es v
categorical' xs = Model $ do
  call (CategoricalDist xs Nothing Nothing)

categorical :: forall env es v x. (Eq v, Show v, OpenSum.Member v PrimVal) => Observable env x v
  => [(v, Double)] -> ObsVar x
  -> Model env es v
categorical xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (CategoricalDist xs maybe_y tag)

normal' :: Double -> Double -> Model env es Double
normal' mu sigma = Model $ do
  call (NormalDist mu sigma Nothing Nothing)

normal :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
normal mu sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (NormalDist mu sigma maybe_y tag)

halfNormal' :: Double -> Model env es Double
halfNormal' sigma = Model $ do
  call (HalfNormalDist sigma Nothing Nothing)

halfNormal :: forall env es x. Observable env x Double
  => Double -> ObsVar x
  -> Model env es Double
halfNormal sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (HalfNormalDist sigma maybe_y tag)

cauchy' :: Double -> Double -> Model env es Double
cauchy' mu sigma = Model $ do
  call (CauchyDist mu sigma Nothing Nothing)

cauchy :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
cauchy mu sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (CauchyDist mu sigma maybe_y tag)

halfCauchy' :: Double -> Model env es Double
halfCauchy' sigma = Model $ do
  call (HalfCauchyDist sigma Nothing Nothing)

halfCauchy :: forall env es x. Observable env x Double
  => Double -> ObsVar x
  -> Model env es Double
halfCauchy sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (HalfCauchyDist sigma maybe_y tag)

bernoulli' :: Double -> Model env es Bool
bernoulli' p = Model $ do
  call (BernoulliDist p Nothing Nothing)

bernoulli :: forall env es x. Observable env x Bool
  => Double -> ObsVar x
  -> Model env es Bool
bernoulli p field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (BernoulliDist p maybe_y tag)

binomial' :: Int -> Double -> Model env es Int
binomial' n p = Model $ do
  call (BinomialDist n p Nothing Nothing)

binomial :: forall env es x. Observable env x Int
  => Int -> Double -> ObsVar x
  -> Model env es Int
binomial n p field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (BinomialDist n p maybe_y tag)

gamma' :: Double -> Double -> Model env es Double
gamma' x θ = Model $ do
  call (GammaDist x θ Nothing Nothing)

gamma :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
gamma x θ field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (GammaDist x θ maybe_y tag)

beta' :: Double -> Double -> Model env es Double
beta' α β = Model $ do
  call (BetaDist α β Nothing Nothing)

beta :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
beta α β field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (BetaDist α β maybe_y tag)

uniform' :: Double -> Double -> Model env es Double
uniform' min max = Model $ do
  call (UniformDist min max Nothing Nothing)

uniform :: forall env es x. Observable env x Double
  => Double -> Double -> ObsVar x
  -> Model env es Double
uniform min max field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (UniformDist min max maybe_y tag)

poisson' :: Double -> Model env es Int
poisson' λ = Model $ do
  call (PoissonDist λ Nothing Nothing)

poisson :: forall env es x. Observable env x Int
  => Double -> ObsVar x
  -> Model env es Int
poisson λ field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (PoissonDist λ maybe_y tag)