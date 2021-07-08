{-# LANGUAGE RankNTypes, GADTs, TypeApplications, FlexibleInstances, DerivingStrategies, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, PolyKinds, UndecidableSuperClasses, TemplateHaskell, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, OverloadedLabels, UndecidableInstances, FunctionalDependencies, TypeFamilyDependencies #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Extensible.Model where

import Util
import Extensible.Dist
import Extensible.Freer
-- import Extensible.Reader
import Extensible.RecordReader
import Extensible.Sampler
import Extensible.IO
import GHC.Generics
import GHC.Types
import GHC.TypeLits
import Data.Maybe
import Data.Kind
import Data.Profunctor.Unsafe
import Data.Extensible hiding (wrap, Head, Member)
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

mkField "m c b μ σ mu sigma y ys label yObs weight bias"

type family Maybes (as :: [k]) = (bs :: [k]) | bs -> as where
  Maybes ((f :> v) : as) = (f :> Maybe v) : Maybes as
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
class Lookup (AsList xs) k [v] => HasVar xs k v where

instance Lookup (AsList xs) k [v] => HasVar xs k v where

type MRec s = Record (Maybes s)

{-
Idea : we can use State instead of Reader for the environment of observable variables.
This lets us use affine types in a way. When a variable is consumed by being conditioned against, we replace its value in the environment with Nothing. When a variable corresponds to a list of values, conditioning against it means removing the head element from the list and writing back the tail of the list. When the tail is empty, we replace the variable's value with Nothing.
  or:
We convert all the environment values to lists if they aren't already.
  or:
We can use reader on the user end, but then transform this during inference to use the state effect.
-}

newtype Model env es a =
  Model { runModel :: (Member Dist es, Member (RecReader (AsList env)) es) => Freer es a }
  deriving Functor

instance Applicative (Model env es) where
  pure = Model . pure
  (<*>) = ap

instance Monad (Model env es) where
  return = pure
  Model f >>= k = Model $ do
    f' <- f
    runModel $ k f'

-- accessEnv :: forall s es . (Member (RecReader (AsList s)) es) =>
--    Freer es (LRec s)
-- accessEnv = do
--     env :: LRec s <- Free (inj Ask) Pure
--     return env

accessField :: forall s es a.
   Lens' (AsList s :& Field Identity) [a] ->
   Model s es (Maybe a)
accessField f = Model $ do
    env <- Free (inj $ Ask f) Pure
    return env

-- replicateMdl :: forall s es a.
--      Int
--   -> Getting [a] (AsList s :& Field Identity) [a]
--   -> (Getting a (AsList s :& Field Identity) a -> Model s es a)
--   -> Model s es [a]
-- replicateMdl n field dist = Model $ do
--   env <- Free (inj $ Ask field) Pure
--   let maybe_ys = env ^. field
--   -- replicateM n ()
--   undefined

normal :: Double -> Double -> Model s es Double
normal mu sigma = Model $ do
  send (NormalDist mu sigma Nothing)

normal' :: forall s es a . (a ~ Double)
  => Double -> Double -> Lens' (AsList s :& Field Identity) [a]
  -> Model s es Double
normal' mu sigma field = Model $ do
  maybe_y <- Free (inj $ Ask field) Pure
  send (NormalDist mu sigma maybe_y)

bernoulli :: Double -> Model s es Bool
bernoulli p = Model $ do
  send (BernoulliDist p Nothing)

bernoulli' :: forall s es a. (a ~ Bool)
  => Double -> Lens' (AsList s :& Field Identity) [a]
  -> Model s es Bool
bernoulli' p field = Model $ do
  maybe_y <- Free (inj $ Ask field) Pure
  send (BernoulliDist p maybe_y)

binomial :: Int -> Double -> Model s es Int
binomial n p = Model $ do
  send (BinomialDist n p Nothing)

binomial' :: forall s es a. (a ~  Int)
  => Int -> Double -> Lens' (AsList s :& Field Identity) [a]
  -> Model s es Int
binomial' n p field = Model $ do
  maybe_y <- Free (inj $ Ask field) Pure
  send (BinomialDist n p maybe_y)

gamma :: Double -> Double -> Model s es Double
gamma k θ = Model $ do
  send (GammaDist k θ Nothing)

gamma' :: forall s es a. (a ~ Double)
  => Double -> Double -> Lens' (AsList s :& Field Identity) [a]
  -> Model s es Double
gamma' k θ field = Model $ do
  maybe_y <- Free (inj $ Ask field) Pure
  send (GammaDist k θ maybe_y)

uniform :: Double -> Double -> Model s es Double
uniform min max = Model $ do
  send (UniformDist min max Nothing)

uniform' :: forall s es a. (a ~ [Double])
  => Double -> Double -> Lens' (AsList s :& Field Identity) a
  -> Model s es Double
uniform' min max field = Model $ do
  maybe_y <- Free (inj $ Ask field) Pure
  send (UniformDist min max maybe_y)
