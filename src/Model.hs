{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

{- | An algebraic effect embedding of probabilistic models.
-}

module Model (
    Model(..)
  , ProbProg
  , handleCore
    -- * Distribution smart constructors
    -- $Smart-Constructors
  , bernoulli
  , bernoulli'
  , beta
  , beta'
  , binomial
  , binomial'
  , categorical
  , categorical'
  , cauchy
  , cauchy'
  , halfCauchy
  , halfCauchy'
  , deterministic
  , deterministic'
  , dirichlet
  , dirichlet'
  , discrete
  , discrete'
  , gamma
  , gamma'
  , normal
  , normal'
  , halfNormal
  , halfNormal'
  , poisson
  , poisson'
  , uniform
  , uniform'
  )
  where

import Control.Monad ( ap )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Effects.Dist ( handleDist, Dist(..), Observe, Sample )
import Effects.ObsRW
import Env
import OpenSum ( OpenSum )
import PrimDist
import Prog ( call, Member, Prog )
import qualified OpenSum
import Debug.Trace
import Sampler
import Effects.Lift
import Data.Typeable

{- | Models are parameterised by:

    1) a model environment @env@ containing random variables that can be provided observed values

    2) an effect signature @es@ of the possible effects a model can invoke

    3) an output type @a@ of values that the model generates.

    A model initially consists of (at least) two effects: @Dist@ for calling primitive distributions
    and @ObsRW env@ for reading from @env@.
-}
newtype Model env es a =
  Model { runModel :: ( Member Dist es        -- models can call primitive distributions
                      , Member (ObsRW env) es -- models can read observed values from their environment
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

{- | Probabilistic programs are those with effects for conditioning and sampling.
-}
type ProbProg a = Prog [Observe, Sample, Lift Sampler] a

{- | The initial handler for models, specialising a model under a certain environment
     to produce a probabilistic program consisting of @Sample@ and @Observe@ operations.
-}
handleCore :: Env env -> Model env (ObsRW env : Dist : es) a -> Prog (Observe : Sample : es) (a, Env env)
handleCore env_in m = (handleDist . handleObsRW env_in) (runModel m)

{- $Smart-Constructors

    Smart constructors for calling primitive distribution operations inside models,
    where each distribution comes with a primed and an unprimed variant.

    An unprimed distribution takes the standard distribution parameters as well as
    an observable variable. This lets one later provide observed values for that
    variable to be conditioned against:

    @
    exampleModel :: Observable env "b" Bool => Model env es Bool
    exampleModel = bernoulli 0.5 #b
    @

    A primed distribution takes no observable variable and so cannot be conditioned against;
    this will always representing sampling from that distribution:

    @
    exampleModel' :: Model env es Bool
    exampleModel' = bernoulli' 0.5
    @
-}

callDist :: forall env x d a es. (PrimDist d a,  Show a) => Observable env x a => d -> Var x -> Model env es a
callDist d field = Model $ do
  let tag =  Just $ varToStr field
  maybe_y <- oAsk @env field
  y <- call (Dist d maybe_y tag)
  oTell @env field y
  pure y

callDist' :: (PrimDist d a) => d -> Model env es a
callDist' d = Model $ call (Dist d Nothing Nothing)

deterministic :: (Typeable a, Eq a, Show a, Observable env x a) => a
  -> Var x
  -> Model env es a
deterministic x = callDist (Deterministic x)

deterministic' :: (Typeable a, Eq a, Show a) =>
  -- | value to be deterministically generated
     a
  -> Model env es a
deterministic' x = callDist' (Deterministic x)

dirichlet ::Observable env x [Double] =>
     [Double]
  -> Var x
  -> Model env es [Double]
dirichlet xs = callDist (Dirichlet xs)

dirichlet' ::
  -- | concentration parameters
     [Double]
  -> Model env es [Double]
dirichlet' xs = callDist' (Dirichlet xs)

discrete :: (Typeable a, Eq a, Show a, Observable env x a) =>
     [(a, Double)]
  -> Var x
  -> Model env es a
discrete ps = callDist (Discrete ps)

discrete' :: (Typeable a, Eq a, Show a) =>
  -- | primitive values and their probabilities
     [(a, Double)]
  -> Model env es a
discrete' ps = callDist' (Discrete ps)

categorical :: Observable env x Int =>
     [Double]
  -> Var x
  -> Model env es Int
categorical xs = callDist (Categorical xs)

categorical'
  -- | list of @n@ probabilities
  :: [Double]
  -- | integer index from @0@ to @n - 1@
  -> Model env es Int
categorical' xs = callDist' (Categorical xs)

normal :: Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> Model env es Double
normal mu sigma = callDist (Normal mu sigma)

normal'
  -- | mean
  :: Double
  -- | standard deviation
  -> Double
  -> Model env es Double
normal' mu sigma = callDist' (Normal mu sigma)

halfNormal :: Observable env x Double
  => Double
  -> Var x
  -> Model env es Double
halfNormal sigma = callDist (HalfNormal sigma)

halfNormal'
  -- | standard deviation
  :: Double
  -> Model env es Double
halfNormal' sigma = Model $ do
  call (Dist (HalfNormal sigma) Nothing Nothing)

cauchy :: Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> Model env es Double
cauchy mu sigma = callDist (Cauchy mu sigma)

cauchy'
  -- | location
  :: Double
  -- | scale
  -> Double
  -> Model env es Double
cauchy' mu sigma = callDist' (Cauchy mu sigma)

halfCauchy :: Observable env x Double =>
     Double
  -> Var x
  -> Model env es Double
halfCauchy sigma = callDist (HalfCauchy sigma)

halfCauchy' ::
  -- | scale
     Double
  -> Model env es Double
halfCauchy' sigma = Model $ do
  call (Dist (HalfCauchy sigma) Nothing Nothing)

bernoulli :: Observable env x Bool =>
     Double
  -> Var x
  -> Model env es Bool
bernoulli p = callDist (Bernoulli p)

bernoulli' ::
  -- | probability of @True@
     Double
  -> Model env es Bool
bernoulli' p = callDist' (Bernoulli p)

beta :: Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> Model env es Double
beta α β = callDist (Beta α β)

beta' ::
  -- | shape 1 (α)
     Double
  -- | shape 2 (β)
  -> Double
  -> Model env es Double
beta' α β = callDist' (Beta α β)

binomial :: Observable env x Int =>
     Int
  -> Double
  -> Var x
  -> Model env es Int
binomial n p = callDist (Binomial n p)

binomial' ::
  -- | number of trials
     Int
  -- | probability of successful trial
  -> Double
  -- | number of successful trials
  -> Model env es Int
binomial' n p = callDist' (Binomial n p)

gamma :: forall env es x. Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> Model env es Double
gamma k θ = callDist (Gamma k θ)

gamma' ::
  -- | shape (k)
     Double
  -- | scale (θ)
  -> Double
  -> Model env es Double
gamma' k θ = callDist' (Gamma k θ)

uniform :: Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> Model env es Double
uniform min max = callDist (Uniform min max)

uniform' ::
  -- | lower-bound
     Double
  -- | upper-bound
  -> Double
  -> Model env es Double
uniform' min max = callDist' (Uniform min max)

poisson :: Observable env x Int =>
     Double
  -> Var x
  -> Model env es Int
poisson λ = callDist (Poisson λ)

poisson' ::
  -- | rate (λ)
     Double
  -- | number of events
  -> Model env es Int
poisson' λ = callDist' (Poisson λ)

