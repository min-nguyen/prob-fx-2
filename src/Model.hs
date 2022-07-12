{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Model where


import Control.Monad ( ap )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Effects.Dist
import Effects.Lift
import Effects.ObsReader
import Effects.State ( State, getSt, putSt, modify, handleState )
import Effects.Writer
import Env
import OpenSum (OpenSum)
import PrimDist
import Prog
import qualified OpenSum


-- ** Model definition
-- $ Probabilistic models are embedded using an algebraic effect approach, represented as syntax trees (uninterpreted programs).

-- | A @Model@ is indexed by a model environment @env@ containing random variables that can be provided observed values for, an effect signature @es@ of the possible effects a model can invoke, and an output type @a@ of values that the model generates.
newtype Model env es a =
  Model { runModel :: ( Member Dist es            -- ^ Models can call primitive distributions
                      , Member (ObsReader env) es -- ^ Models can read observed values from the environment
                      ) 
                   => Prog es a }
  deriving Functor

instance Applicative (Model env es) where
  pure x = Model $ pure x
  (<*>) = ap

instance Monad (Model env es) where
  return = pure
  Model f >>= x = Model $ do
    f' <- f
    runModel $ x f'

-- | The initial handler for models, specialising a model under a certain environment to produce a probabilistic program consisting of @Sample@ and @Observe@ operations. 
handleCore :: Env env -> Model env (ObsReader env : Dist : es) a -> Prog (Observe : Sample : es) a
handleCore env m = (handleDist . handleObsRead env) (runModel m)

-- ** Model operations
-- $ Smart constructors for calling primitive distributions from the model type.

deterministic' :: (Eq a, Show a, OpenSum.Member a PrimVal)
  => a -> Model env es a
deterministic' x = Model $ do
  call (Dist (Deterministic x) Nothing Nothing)

deterministic :: forall env es a x. (Eq a, Show a, OpenSum.Member a PrimVal)
  => Observable env x a
  => a -> Var x -> Model env es a
deterministic x field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Deterministic x) maybe_y tag)

dirichlet' :: [Double] -> Model env es [Double]
dirichlet' xs = Model $ do
  call (Dist (Dirichlet xs) Nothing Nothing)

dirichlet :: forall env es x. Observable env x [Double]
  => [Double] -> Var x
  -> Model env es [Double]
dirichlet xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Dirichlet xs) maybe_y tag)

categorical' :: [Double] -> Model env es Int
categorical' xs = Model $ do
  call (Dist (Categorical xs) Nothing Nothing)

categorical :: forall env es x. Observable env x Int
  => [Double] -> Var x
  -> Model env es Int
categorical xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Categorical xs) maybe_y tag)

discrete' :: (Eq a, Show a, OpenSum.Member a PrimVal) => [(a, Double)] -> Model env es a
discrete' xs = Model $ do
  call (Dist (Discrete xs) Nothing Nothing)

discrete :: forall env es a x. (Eq a, Show a, OpenSum.Member a PrimVal) => Observable env x a
  => [(a, Double)] -> Var x
  -> Model env es a
discrete xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Discrete xs) maybe_y tag)

normal' :: Double -> Double -> Model env es Double
normal' mu sigma = Model $ do
  call (Dist (Normal mu sigma) Nothing Nothing)

normal :: forall env es x. Observable env x Double
  => Double -> Double -> Var x
  -> Model env es Double
normal mu sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Normal mu sigma) maybe_y tag)

halfNormal' :: Double -> Model env es Double
halfNormal' sigma = Model $ do
  call (Dist (HalfNormal sigma) Nothing Nothing)

halfNormal :: forall env es x. Observable env x Double
  => Double -> Var x
  -> Model env es Double
halfNormal sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (HalfNormal sigma) maybe_y tag)

cauchy' :: Double -> Double -> Model env es Double
cauchy' mu sigma = Model $ do
  call (Dist (Cauchy mu sigma) Nothing Nothing)

cauchy :: forall env es x. Observable env x Double
  => Double -> Double -> Var x
  -> Model env es Double
cauchy mu sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Cauchy mu sigma) maybe_y tag)

halfCauchy' :: Double -> Model env es Double
halfCauchy' sigma = Model $ do
  call (Dist (HalfCauchy sigma) Nothing Nothing)

halfCauchy :: forall env es x. Observable env x Double
  => Double -> Var x
  -> Model env es Double
halfCauchy sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (HalfCauchy sigma) maybe_y tag)

bernoulli' :: Double -> Model env es Bool
bernoulli' p = Model $ do
  call (Dist (Bernoulli p) Nothing Nothing)

bernoulli :: forall env es x. Observable env x Bool
  => Double -> Var x
  -> Model env es Bool
bernoulli p field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Bernoulli p) maybe_y tag)

binomial' :: Int -> Double -> Model env es Int
binomial' n p = Model $ do
  call (Dist (Binomial n p) Nothing Nothing)

binomial :: forall env es x. Observable env x Int
  => Int -> Double -> Var x
  -> Model env es Int
binomial n p field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Binomial n p) maybe_y tag)

gamma' :: Double -> Double -> Model env es Double
gamma' x θ = Model $ do
  call (Dist (Gamma x θ) Nothing Nothing)

gamma :: forall env es x. Observable env x Double
  => Double -> Double -> Var x
  -> Model env es Double
gamma x θ field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Gamma x θ) maybe_y tag)

beta' :: Double -> Double -> Model env es Double
beta' α β = Model $ do
  call (Dist (Beta α β) Nothing Nothing)

beta :: forall env es x. Observable env x Double
  => Double -> Double -> Var x
  -> Model env es Double
beta α β field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Beta α β) maybe_y tag)

uniform' :: Double -> Double -> Model env es Double
uniform' min max = Model $ do
  call (Dist (Uniform min max) Nothing Nothing)

uniform :: forall env es x. Observable env x Double
  => Double -> Double -> Var x
  -> Model env es Double
uniform min max field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Uniform min max) maybe_y tag)

poisson' :: Double -> Model env es Int
poisson' λ = Model $ do
  call (Dist (Poisson λ) Nothing Nothing)

poisson :: forall env es x. Observable env x Int
  => Double -> Var x
  -> Model env es Int
poisson λ field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Poisson λ) maybe_y tag)


-- ** Other operations

-- **** State
getStM :: (Member (State s) es) => Model env es s
getStM = Model getSt

putStM :: (Member (State s) es) => s -> Model env es ()
putStM s = Model (putSt s)

handleStateM :: s -> Model env (State s : es) a -> Model env es (a, s)
handleStateM s m = Model $ handleState s $ runModel m

-- **** Writer
tellM :: Member (Writer w) es => w -> Model env es ()
tellM w = Model $ tell w

handleWriterM :: Monoid w => Model env (Writer w : es) a -> Model env es (a, w)
handleWriterM m = Model $ handleWriter $ runModel m

-- **** Lift
liftM :: (Member (Lift m) es) => m a -> Model env es a
liftM op = Model (call (Lift op))
