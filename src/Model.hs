
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE TypeApplications #-}


{-# LANGUAGE ConstraintKinds #-}

{- | An algebraic effect embedding of probabilistic models.
-}

module Model (
    GenModel(..)
  , Model(..)
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
import Data.Type.Nat
import Effects.Dist ( handleDist, Dist(..), Observe, Sample)
import Effects.EnvRW
import Env
import PrimDist
import Comp ( call, Member, Comp, Members, LastMember )
import Vec
import Debug.Trace
import Sampler
import Data.Typeable

{- | Models are parameterised by:

    1) a model environment @env@ containing random variables that can be provided observed values

    2) an effect signature @es@ of the possible effects a model can invoke

    3) an output type @a@ of values that the model generates.

    A model initially consists of (at least) two effects: @Dist@ for calling primitive distributions
    and @EnvRW env@ for reading from @env@.
-}
newtype GenModel env es a =
  GenModel { runGenModel :: ( Member Dist es        -- models can call primitive distributions
                      , Member (EnvRW env) es -- models can read observed values from their environment
                      )
                   => Comp es a }
  deriving Functor

instance Applicative (GenModel env es) where
  pure x = GenModel $ pure x
  (<*>) = ap

instance Monad (GenModel env es) where
  return = pure
  GenModel f >>= x = GenModel $ do
    f' <- f
    runGenModel $ x f'

{- | The initial handler for models, specialising a model under a certain environment
     to produce a probabilistic program consisting of @Sample@ and @Observe@ operations.
-}
handleCore :: forall env es a. Env env -> GenModel env (EnvRW env : Dist : es) a
           -> Model (Observe : Sample : es) (a, Env env)
handleCore env_in m = Model $ (handleDist . handleEnvRW env_in) (runGenModel m)

{- | Probabilistic programs are those with effects for conditioning and sampling.
-}

newtype Model es a =
  Model { runModel :: Members [Observe, Sample] es
                   => Comp es a
  }

{- $Smart-Constructors

    Smart constructors for calling primitive distribution operations inside models,
    where each distribution comes with a primed and an unprimed variant.

    An unprimed distribution takes the standard distribution parameters as well as
    an observable variable. This lets one later provide observed values for that
    variable to be conditioned against:

    @
    exampleModel :: Observable env "b" Bool => GenModel env es Bool
    exampleModel = bernoulli 0.5 #b
    @

    A primed distribution takes no observable variable and so cannot be conditioned against;
    this will always representing sampling from that distribution:

    @
    exampleModel' :: GenModel env es Bool
    exampleModel' = bernoulli' 0.5
    @
-}

callDist :: forall env x d a es. (PrimDist d a,  Show a) => Observable env x a => d -> Var x -> GenModel env es a
callDist d field = GenModel $ do
  let tag =  Just $ varToStr field
  maybe_y <- call (Read @env field)
  y <- call (Dist d maybe_y tag)
  call (Write @env field y)
  pure y

callDist' :: (PrimDist d a) => d -> GenModel env es a
callDist' d = GenModel $ call (Dist d Nothing Nothing)

dirichlet :: (Observable env x (Vec n Double), TypeableSNatI n) =>
     Vec n Double
  -> Var x
  -> GenModel env es (Vec n Double)
dirichlet xs = callDist (mkDirichlet xs)

dirichlet' :: (TypeableSNatI n) =>
  -- | concentration parameters
     Vec n Double
  -> GenModel env es (Vec n Double)
dirichlet' xs = callDist' (mkDirichlet xs)

discrete :: (Typeable a, Eq a, Show a, Observable env x a) =>
     [(a, Double)]
  -> Var x
  -> GenModel env es a
discrete ps = callDist (mkDiscrete ps)

discrete' :: (Typeable a, Eq a, Show a) =>
  -- | primitive values and their probabilities
     [(a, Double)]
  -> GenModel env es a
discrete' ps = callDist' (mkDiscrete ps)

categorical :: (Observable env x Int)
  => [Double]
  -> Var x
  -> GenModel env es Int
categorical xs = callDist (mkCategorical xs)

categorical' ::
  -- | list of @n@ probabilities
     [Double]
  -- | integer index from @0@ to @n - 1@
  -> GenModel env es Int
categorical' xs = callDist' (mkCategorical xs)

normal :: Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> GenModel env es Double
normal mu sigma = callDist (mkNormal mu sigma)

normal'
  -- | mean
  :: Double
  -- | standard deviation
  -> Double
  -> GenModel env es Double
normal' mu sigma = callDist' (mkNormal mu sigma)

halfNormal :: Observable env x Double
  => Double
  -> Var x
  -> GenModel env es Double
halfNormal sigma = callDist (mkHalfNormal sigma)

halfNormal'
  -- | standard deviation
  :: Double
  -> GenModel env es Double
halfNormal' sigma = callDist' (mkHalfNormal sigma)

cauchy :: Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> GenModel env es Double
cauchy mu sigma = callDist (mkCauchy mu sigma)

cauchy'
  -- | location
  :: Double
  -- | scale
  -> Double
  -> GenModel env es Double
cauchy' mu sigma = callDist' (mkCauchy mu sigma)

halfCauchy :: Observable env x Double =>
     Double
  -> Var x
  -> GenModel env es Double
halfCauchy sigma = callDist (mkHalfCauchy sigma)

halfCauchy' ::
  -- | scale
     Double
  -> GenModel env es Double
halfCauchy' sigma = callDist' (mkHalfCauchy sigma)

bernoulli :: Observable env x Bool =>
     Double
  -> Var x
  -> GenModel env es Bool
bernoulli p = callDist (mkBernoulli p)

bernoulli' ::
  -- | probability of @True@
     Double
  -> GenModel env es Bool
bernoulli' p = callDist' (mkBernoulli p)

beta :: Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> GenModel env es Double
beta α β = callDist (mkBeta α β)

beta' ::
  -- | shape 1 (α)
     Double
  -- | shape 2 (β)
  -> Double
  -> GenModel env es Double
beta' α β = callDist' (mkBeta α β)

binomial :: Observable env x Int =>
     Int
  -> Double
  -> Var x
  -> GenModel env es Int
binomial n p = callDist (mkBinomial n p)

binomial' ::
  -- | number of trials
     Int
  -- | probability of successful trial
  -> Double
  -- | number of successful trials
  -> GenModel env es Int
binomial' n p = callDist' (mkBinomial n p)

gamma :: forall env es x. Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> GenModel env es Double
gamma k θ = callDist (mkGamma k θ)

gamma' ::
  -- | shape (k)
     Double
  -- | scale (θ)
  -> Double
  -> GenModel env es Double
gamma' k θ = callDist' (mkGamma k θ)

uniform :: Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> GenModel env es Double
uniform min max = callDist (mkUniform min max)

uniform' ::
  -- | lower-bound
     Double
  -- | upper-bound
  -> Double
  -> GenModel env es Double
uniform' min max = callDist' (mkUniform min max)

poisson :: Observable env x Int =>
     Double
  -> Var x
  -> GenModel env es Int
poisson λ = callDist (mkPoisson λ)

poisson' ::
  -- | rate (λ)
     Double
  -- | number of events
  -> GenModel env es Int
poisson' λ = callDist' (mkPoisson λ)

