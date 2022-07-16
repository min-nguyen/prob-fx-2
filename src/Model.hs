{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Model ( 
    Model(..)
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
import Effects.Dist
import Effects.ObsReader
import Env
import OpenSum (OpenSum)
import PrimDist
import Prog
import qualified OpenSum


{- | Models are parameterised by:

1) a model environment @env@ containing random variables that can be provided observed values for, 

2) an effect signature @es@ of the possible effects a model can invoke, and 

3) an output type @a@ of values that the model generates. 

A model initially consists of (at least) two effects: @Dist@ for calling primitive distributions and @ObsReader env@ for reading from @env@. 
-}
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

{- | The initial handler for models, specialising a model under a certain 
environment to produce a probabilistic program consisting of @Sample@ and @Observe@ operations. -}
handleCore :: Env env -> Model env (ObsReader env : Dist : es) a -> Prog (Observe : Sample : es) a
handleCore env m = (handleDist . handleObsRead env) (runModel m)

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

A primed distribution takes no observable variable and so cannot be conditioned against; this will always representing sampling from that distribution:

@
exampleModel' :: Model env es Bool
exampleModel' = bernoulli' 0.5
@

-}

deterministic :: forall env es a x. (Eq a, Show a, OpenSum.Member a PrimVal, Observable env x a) => a    
  -> Var x 
  -> Model env es a
deterministic x field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Deterministic x) maybe_y tag)

deterministic' :: (Eq a, Show a, OpenSum.Member a PrimVal) => 
  -- | Value to be deterministically generated
     a 
  -> Model env es a
deterministic' x = Model $ do
  call (Dist (Deterministic x) Nothing Nothing)

dirichlet :: forall env es x. Observable env x [Double] => 
     [Double] 
  -> Var x
  -> Model env es [Double]
dirichlet xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Dirichlet xs) maybe_y tag)

dirichlet' :: 
  -- | Concentration parameters
     [Double] 
  -> Model env es [Double]
dirichlet' xs = Model $ do
  call (Dist (Dirichlet xs) Nothing Nothing)

discrete :: forall env es a x. (Eq a, Show a, OpenSum.Member a PrimVal, Observable env x a) => 
     [(a, Double)] 
  -> Var x
  -> Model env es a 
discrete ps field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Discrete ps) maybe_y tag)

discrete' :: (Eq a, Show a, OpenSum.Member a PrimVal) => 
  -- | Primitive values and their probabilities
     [(a, Double)] 
  -> Model env es a
discrete' ps = Model $ do
  call (Dist (Discrete ps) Nothing Nothing)

categorical :: forall env es x. Observable env x Int => 
     [Double] 
  -> Var x
  -> Model env es Int
categorical xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Categorical xs) maybe_y tag)

categorical' :: 
  -- | List of @n@ probabilities
     [Double] 
  -- | Integer index from @0@ to @n - 1@
  -> Model env es Int
categorical' xs = Model $ do
  call (Dist (Categorical xs) Nothing Nothing)

normal :: forall env es x. Observable env x Double => 
     Double
  -> Double 
  -> Var x
  -> Model env es Double
normal mu sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Normal mu sigma) maybe_y tag)

normal' :: 
  -- | Mean
     Double 
  -- | Standard deviation 
  -> Double 
  -> Model env es Double
normal' mu sigma = Model $ do
  call (Dist (Normal mu sigma) Nothing Nothing)

halfNormal :: forall env es x. Observable env x Double => 
     Double 
  -> Var x
  -> Model env es Double
halfNormal sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (HalfNormal sigma) maybe_y tag)

halfNormal' :: 
  -- | Standard deviation
     Double 
  -> Model env es Double
halfNormal' sigma = Model $ do
  call (Dist (HalfNormal sigma) Nothing Nothing)

cauchy :: forall env es x. Observable env x Double => 
     Double 
  -> Double 
  -> Var x
  -> Model env es Double
cauchy mu sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Cauchy mu sigma) maybe_y tag)

cauchy' :: 
  -- | Location
     Double 
  -- | Scale
  -> Double 
  -> Model env es Double
cauchy' mu sigma = Model $ do
  call (Dist (Cauchy mu sigma) Nothing Nothing)

halfCauchy :: forall env es x. Observable env x Double => 
     Double 
  -> Var x
  -> Model env es Double
halfCauchy sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (HalfCauchy sigma) maybe_y tag)

halfCauchy' :: 
  -- | Scale
     Double 
  -> Model env es Double
halfCauchy' sigma = Model $ do
  call (Dist (HalfCauchy sigma) Nothing Nothing)

bernoulli :: forall env es x. Observable env x Bool => 
     Double 
  -> Var x
  -> Model env es Bool
bernoulli p field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Bernoulli p) maybe_y tag)

bernoulli' :: 
  -- | Probability of @True@
     Double 
  -> Model env es Bool
bernoulli' p = Model $ do
  call (Dist (Bernoulli p) Nothing Nothing)

beta :: forall env es x. Observable env x Double => 
     Double 
  -> Double 
  -> Var x
  -> Model env es Double
beta α β field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Beta α β) maybe_y tag)

beta' :: 
  -- | Shape 1 (α)
     Double
  -- | Shape 2 (β)
  -> Double 
  -> Model env es Double
beta' α β = Model $ do
  call (Dist (Beta α β) Nothing Nothing)

binomial :: forall env es x. Observable env x Int => 
     Int 
  -> Double 
  -> Var x
  -> Model env es Int
binomial n p field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Binomial n p) maybe_y tag)

binomial' :: 
  -- | Number of trials   
     Int 
  -- | Probability of successful trial
  -> Double 
  -- | Number of successful trials
  -> Model env es Int
binomial' n p = Model $ do
  call (Dist (Binomial n p) Nothing Nothing)

gamma :: forall env es x. Observable env x Double => 
     Double 
  -> Double 
  -> Var x
  -> Model env es Double
gamma k θ field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Gamma k θ) maybe_y tag)

gamma' :: 
  -- | Shape (k)
     Double 
  -- | Scale (θ)
  -> Double 
  -> Model env es Double
gamma' k θ = Model $ do
  call (Dist (Gamma k θ) Nothing Nothing)

uniform :: forall env es x. Observable env x Double => 
     Double 
  -> Double 
  -> Var x
  -> Model env es Double
uniform min max field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Uniform min max) maybe_y tag)

uniform' ::
  -- | Lower-bound
     Double 
  -- | Upper-bound
  -> Double 
  -> Model env es Double
uniform' min max = Model $ do
  call (Dist (Uniform min max) Nothing Nothing)

poisson :: forall env es x. Observable env x Int => 
     Double 
  -> Var x
  -> Model env es Int
poisson λ field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  call (Dist (Poisson λ) maybe_y tag)

poisson' :: 
  -- | Rate (λ)
     Double 
  -- | Number of events
  -> Model env es Int
poisson' λ = Model $ do
  call (Dist (Poisson λ) Nothing Nothing)

