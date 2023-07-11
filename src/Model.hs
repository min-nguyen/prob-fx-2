
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE IncoherentInstances #-}

{- | An algebraic effect embedding of probabilistic models.
-}

module Model (
    MulModel(..)
  , Model
  , liftHandler
  , liftCall
  , conditionWith
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
import Data.Type.Nat
import Effects.MulDist ( handleMulDist, MulDist(..), Observe, Sample)
import Effects.EnvRW
import Env
import Dist
import Comp ( call, Member, Comp, Members, LastMember, Handler )
import Vec
import Debug.Trace
import Data.Typeable

{- | Models are parameterised by:

    1) a model environment @env@ containing random variables that can be provided observed values

    2) an effect signature @es@ of the possible effects a model can invoke

    3) an output type @a@ of values that the model generates.

    A model initially consists of (at least) two effects: @MulDist@ for calling primitive distributions
    and @EnvRW env@ for reading from @env@.
-}
newtype MulModel env es a =
  MulModel { runModel :: ( Member MulDist es        -- models can call primitive distributions
                         , Member (EnvRW env) es -- models can read observed values from their environment
                      )
                   => Comp es a }
  deriving Functor

instance Applicative (MulModel env es) where
  pure x = MulModel $ pure x
  (<*>) = ap

instance Monad (MulModel env es) where
  return = pure
  MulModel f >>= x = MulModel $ do
    f' <- f
    runModel $ x f'

liftHandler :: Handler e es a b -> MulModel env (e:es) a -> MulModel env es b
liftHandler h (MulModel m) = MulModel (h m)

liftCall :: Member e es => e a -> MulModel env es a
liftCall op = MulModel (call op)

{- | Probabilistic programs are those with effects for conditioning and sampling.
-}

type Model es a = Comp (Observe : Sample : es) a

{- | The initial handler for models, specialising a model under a certain environment
     to produce a probabilistic program consisting of @Sample@ and @Observe@ operations.
-}
conditionWith :: Env env -> MulModel env (EnvRW env : MulDist : es) a
              -> Comp (Observe : Sample : es) (a, Env env)
conditionWith env_in m = (handleMulDist . handleEnvRW env_in) (runModel m)

{- $Smart-Constructors

    Smart constructors for calling primitive distribution operations inside models,
    where each distribution comes with a primed and an unprimed variant.

    An unprimed distribution takes the standard distribution parameters as well as
    an observable variable. This lets one later provide observed values for that
    variable to be conditioned against:

    @
    exampleModel :: Observable env "b" Bool => MulModel env es Bool
    exampleModel = bernoulli 0.5 #b
    @

    A primed distribution takes no observable variable and so cannot be conditioned against;
    this will always representing sampling from that distribution:

    @
    exampleModel' :: MulModel env es Bool
    exampleModel' = bernoulli' 0.5
    @
-}

callDist :: forall env x d a es. (Dist d a,  Show a) => Observable env x a => d -> Var x -> MulModel env es a
callDist d field = MulModel $ do
  let tag =  Just $ varToStr field
  maybe_y <- call (EnvRead @env field)
  y <- call (MulDist d maybe_y tag)
  call (EnvWrite @env field y)
  pure y

callDist' :: (Dist d a) => d -> MulModel env es a
callDist' d = MulModel $ call (MulDist d Nothing Nothing)

dirichlet :: (Observable env x (Vec n Double), TypeableSNatI n) =>
     Vec n Double
  -> Var x
  -> MulModel env es (Vec n Double)
dirichlet xs = callDist (mkDirichlet xs)

dirichlet' :: (TypeableSNatI n) =>
  -- | concentration parameters
     Vec n Double
  -> MulModel env es (Vec n Double)
dirichlet' xs = callDist' (mkDirichlet xs)

discrete :: (Typeable a, Eq a, Show a, Observable env x a) =>
     [(a, Double)]
  -> Var x
  -> MulModel env es a
discrete ps = callDist (mkDiscrete ps)

discrete' :: (Typeable a, Eq a, Show a) =>
  -- | primitive values and their probabilities
     [(a, Double)]
  -> MulModel env es a
discrete' ps = callDist' (mkDiscrete ps)

categorical :: (Observable env x Int)
  => [Double]
  -> Var x
  -> MulModel env es Int
categorical xs = callDist (mkCategorical xs)

categorical' ::
  -- | list of @n@ probabilities
     [Double]
  -- | integer index from @0@ to @n - 1@
  -> MulModel env es Int
categorical' xs = callDist' (mkCategorical xs)

normal :: Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> MulModel env es Double
normal mu sigma = callDist (mkNormal mu sigma)

normal'
  -- | mean
  :: Double
  -- | standard deviation
  -> Double
  -> MulModel env es Double
normal' mu sigma = callDist' (mkNormal mu sigma)

halfNormal :: Observable env x Double
  => Double
  -> Var x
  -> MulModel env es Double
halfNormal sigma = callDist (mkHalfNormal sigma)

halfNormal'
  -- | standard deviation
  :: Double
  -> MulModel env es Double
halfNormal' sigma = callDist' (mkHalfNormal sigma)

cauchy :: Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> MulModel env es Double
cauchy mu sigma = callDist (mkCauchy mu sigma)

cauchy'
  -- | location
  :: Double
  -- | scale
  -> Double
  -> MulModel env es Double
cauchy' mu sigma = callDist' (mkCauchy mu sigma)

deterministic :: (Typeable a, Eq a, Show a, Observable env x a)
  => a -> Var x -> MulModel env es a
deterministic x = callDist (mkDeterministic x)

deterministic' :: (Typeable a, Eq a, Show a)
  => a -> MulModel env es a
deterministic' x = callDist' (mkDeterministic x)

halfCauchy :: Observable env x Double =>
     Double
  -> Var x
  -> MulModel env es Double
halfCauchy sigma = callDist (mkHalfCauchy sigma)

halfCauchy' ::
  -- | scale
     Double
  -> MulModel env es Double
halfCauchy' sigma = callDist' (mkHalfCauchy sigma)

bernoulli :: Observable env x Bool =>
     Double
  -> Var x
  -> MulModel env es Bool
bernoulli p = callDist (mkBernoulli p)

bernoulli' ::
  -- | probability of @True@
     Double
  -> MulModel env es Bool
bernoulli' p = callDist' (mkBernoulli p)

beta :: Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> MulModel env es Double
beta α β = callDist (mkBeta α β)

beta' ::
  -- | shape 1 (α)
     Double
  -- | shape 2 (β)
  -> Double
  -> MulModel env es Double
beta' α β = callDist' (mkBeta α β)

binomial :: Observable env x Int =>
     Int
  -> Double
  -> Var x
  -> MulModel env es Int
binomial n p = callDist (mkBinomial n p)

binomial' ::
  -- | number of trials
     Int
  -- | probability of successful trial
  -> Double
  -- | number of successful trials
  -> MulModel env es Int
binomial' n p = callDist' (mkBinomial n p)

gamma :: forall env es x. Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> MulModel env es Double
gamma k θ = callDist (mkGamma k θ)

gamma' ::
  -- | shape (k)
     Double
  -- | scale (θ)
  -> Double
  -> MulModel env es Double
gamma' k θ = callDist' (mkGamma k θ)

uniform :: Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> MulModel env es Double
uniform min max = callDist (mkUniform min max)

uniform' ::
  -- | lower-bound
     Double
  -- | upper-bound
  -> Double
  -> MulModel env es Double
uniform' min max = callDist' (mkUniform min max)

poisson :: Observable env x Int =>
     Double
  -> Var x
  -> MulModel env es Int
poisson λ = callDist (mkPoisson λ)

poisson' ::
  -- | rate (λ)
     Double
  -- | number of events
  -> MulModel env es Int
poisson' λ = callDist' (mkPoisson λ)

