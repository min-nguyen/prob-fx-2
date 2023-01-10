
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE TypeApplications #-}


{-# LANGUAGE ConstraintKinds #-}

{- | An algebraic effect embedding of probabilistic models.
-}

module Model (
    Model(..)
  , Guide
  , ProbProg
  , ProbSig
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
import Effects.Dist ( handleDist, Dist(..), Observe, Sample, Param)
import Effects.EnvRW
import Env
import PrimDist
import Prog ( call, Member, Prog, Members, LastMember )
import Vec
import Debug.Trace
import Sampler
import Effects.Lift
import Data.Typeable

{- | Models are parameterised by:

    1) a model environment @env@ containing random variables that can be provided observed values

    2) an effect signature @es@ of the possible effects a model can invoke

    3) an output type @a@ of values that the model generates.

    A model initially consists of (at least) two effects: @Dist@ for calling primitive distributions
    and @EnvRW env@ for reading from @env@.
-}
newtype Model env es a =
  Model { runModel :: ( Member Dist es        -- models can call primitive distributions
                      , Member (EnvRW env) es -- models can read observed values from their environment
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
type Guide  a    = Prog [Param , Sample ] a
type ProbProg a = Prog [Observe, Sample] a
type ProbSig es = es ~ [Observe, Sample]



{- | The initial handler for models, specialising a model under a certain environment
     to produce a probabilistic program consisting of @Sample@ and @Observe@ operations.
-}
handleCore :: Env env -> Model env (EnvRW env : Dist : '[]) a -> ProbProg (a, Env env)
handleCore env_in m = (handleDist . handleEnvRW env_in) (runModel m)

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
  maybe_y <- call (Read @env field)
  y <- call (Dist d maybe_y tag)
  call (Write @env field y)
  pure y

callDist' :: (PrimDist d a) => d -> Model env es a
callDist' d = Model $ call (Dist d Nothing Nothing)

dirichlet :: (Observable env x (Vec n Double), TypeableSNatI n) =>
     Vec n Double
  -> Var x
  -> Model env es (Vec n Double)
dirichlet xs = callDist (mkDirichlet xs)

dirichlet' :: (TypeableSNatI n) =>
  -- | concentration parameters
     Vec n Double
  -> Model env es (Vec n Double)
dirichlet' xs = callDist' (mkDirichlet xs)

discrete :: (Typeable a, Eq a, Show a, Observable env x a) =>
     [(a, Double)]
  -> Var x
  -> Model env es a
discrete ps = callDist (mkDiscrete ps)

discrete' :: (Typeable a, Eq a, Show a) =>
  -- | primitive values and their probabilities
     [(a, Double)]
  -> Model env es a
discrete' ps = callDist' (mkDiscrete ps)

categorical :: (Observable env x Int)
  => [Double]
  -> Var x
  -> Model env es Int
categorical xs = callDist (mkCategorical xs)

categorical' ::
  -- | list of @n@ probabilities
     [Double]
  -- | integer index from @0@ to @n - 1@
  -> Model env es Int
categorical' xs = callDist' (mkCategorical xs)

normal :: Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> Model env es Double
normal mu sigma = callDist (mkNormal mu sigma)

normal'
  -- | mean
  :: Double
  -- | standard deviation
  -> Double
  -> Model env es Double
normal' mu sigma = callDist' (mkNormal mu sigma)

halfNormal :: Observable env x Double
  => Double
  -> Var x
  -> Model env es Double
halfNormal sigma = callDist (mkHalfNormal sigma)

halfNormal'
  -- | standard deviation
  :: Double
  -> Model env es Double
halfNormal' sigma = callDist' (mkHalfNormal sigma)

cauchy :: Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> Model env es Double
cauchy mu sigma = callDist (mkCauchy mu sigma)

cauchy'
  -- | location
  :: Double
  -- | scale
  -> Double
  -> Model env es Double
cauchy' mu sigma = callDist' (mkCauchy mu sigma)

halfCauchy :: Observable env x Double =>
     Double
  -> Var x
  -> Model env es Double
halfCauchy sigma = callDist (mkHalfCauchy sigma)

halfCauchy' ::
  -- | scale
     Double
  -> Model env es Double
halfCauchy' sigma = callDist' (mkHalfCauchy sigma)

bernoulli :: Observable env x Bool =>
     Double
  -> Var x
  -> Model env es Bool
bernoulli p = callDist (mkBernoulli p)

bernoulli' ::
  -- | probability of @True@
     Double
  -> Model env es Bool
bernoulli' p = callDist' (mkBernoulli p)

beta :: Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> Model env es Double
beta α β = callDist (mkBeta α β)

beta' ::
  -- | shape 1 (α)
     Double
  -- | shape 2 (β)
  -> Double
  -> Model env es Double
beta' α β = callDist' (mkBeta α β)

binomial :: Observable env x Int =>
     Int
  -> Double
  -> Var x
  -> Model env es Int
binomial n p = callDist (mkBinomial n p)

binomial' ::
  -- | number of trials
     Int
  -- | probability of successful trial
  -> Double
  -- | number of successful trials
  -> Model env es Int
binomial' n p = callDist' (mkBinomial n p)

gamma :: forall env es x. Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> Model env es Double
gamma k θ = callDist (mkGamma k θ)

gamma' ::
  -- | shape (k)
     Double
  -- | scale (θ)
  -> Double
  -> Model env es Double
gamma' k θ = callDist' (mkGamma k θ)

uniform :: Observable env x Double =>
     Double
  -> Double
  -> Var x
  -> Model env es Double
uniform min max = callDist (mkUniform min max)

uniform' ::
  -- | lower-bound
     Double
  -- | upper-bound
  -> Double
  -> Model env es Double
uniform' min max = callDist' (mkUniform min max)

poisson :: Observable env x Int =>
     Double
  -> Var x
  -> Model env es Int
poisson λ = callDist (mkPoisson λ)

poisson' ::
  -- | rate (λ)
     Double
  -- | number of events
  -> Model env es Int
poisson' λ = callDist' (mkPoisson λ)

