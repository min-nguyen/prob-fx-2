{-# LANGUAGE RankNTypes, GADTs, TypeApplications, FlexibleInstances, DerivingStrategies, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, PolyKinds, UndecidableSuperClasses, TemplateHaskell, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, OverloadedLabels, UndecidableInstances, FunctionalDependencies, TypeFamilyDependencies #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Extensible.Model where

import Util
import Extensible.Dist
import Extensible.Freer
import Extensible.Reader
import Extensible.Sampler
import Extensible.IO
import GHC.Generics
import GHC.Types
import GHC.TypeLits
import Data.Maybe
import Data.Kind
import Data.Profunctor.Unsafe
import Data.Extensible hiding (wrap, Head, Member)
import Control.Lens ( Identity, (^.), review, Getting )
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

mkField "mu sigma y ys label yObs"

type family Maybes (as :: [k]) = (bs :: [k]) | bs -> as where
  Maybes ((f :> v) : as) = (f :> Maybe v) : Maybes as
  Maybes (a : as) = Maybe a : Maybes as
  Maybes '[] = '[]

-- HasVar: Lookup for maybe types
class Lookup (Maybes xs) k (Maybe v) => HasVar xs k v where

instance Lookup (Maybes xs) k (Maybe v) => HasVar xs k v where

type MRec s = Record (Maybes s)

-- type Model env es a =
--   forall ts. (Member Dist ts, Member (Reader (MRec env)) ts) => Freer ts a


newtype Model env es a =
  Model { runModel :: (Member Dist es, Member (Reader (MRec env)) es) => Freer es a }
  deriving (Functor)

instance Applicative (Model env es) where
  pure = Model . pure
  (<*>) = ap

instance Monad (Model env es) where
  return = pure
  Model f >>= k = Model $ do
    f' <- f
    runModel $ k f'

accessEnv :: forall s es . (Member (Reader (MRec s)) es) =>
   Freer es (MRec s)
accessEnv = do
    env :: MRec s <- Free (inj Ask) Pure
    return env

accessField :: forall s es a.
   Getting a (Maybes s :& Field Identity) a ->
   Model s es a
accessField f = Model $ do
    env :: MRec s <- Free (inj Ask) Pure
    return $ env ^. f

normal :: Double -> Double -> Model s es Double
normal mu sigma = Model $ do
  send (NormalDist mu sigma Nothing)

normal' :: forall s es a . (a ~ Maybe Double)
  => Double -> Double -> Getting a (Maybes s :& Field Identity) a
  -> Model s es Double
normal' mu sigma field = Model $ do
  env :: MRec s <- accessEnv
  let maybe_y = env ^. field
  send (NormalDist mu sigma maybe_y)



bernoulli :: Double -> Model s es Bool
bernoulli p = Model $ do
  send (BernoulliDist p Nothing)

bernoulli' :: forall s es a. (a ~ Maybe Bool)
  => Double -> Getting a (Maybes s :& Field Identity) a
  -> Model s es Bool
bernoulli' p field = Model $ do
  env :: MRec s <- accessEnv
  let maybe_y = env ^. field
  send (BernoulliDist p maybe_y)

gamma :: Double -> Double -> Model s es Double
gamma k θ = Model $ do
  send (GammaDist k θ Nothing)

gamma' :: forall s es a. (a ~ Maybe Double) =>
  (a ~ Maybe Double)
  => Double -> Double -> Getting a (Maybes s :& Field Identity) a
  -> Model s es Double
gamma' k θ field = Model $ do
  env :: MRec s <- accessEnv
  let maybe_y = env ^. field
  send (GammaDist k θ maybe_y)

uniform :: Double -> Double -> Model s es Double
uniform min max = Model $ do
  send (UniformDist min max Nothing)

uniform' :: forall s es a. (a ~ Maybe Double) =>
  (a ~ Maybe Double)
  => Double -> Double -> Getting a (Maybes s :& Field Identity) a
  -> Model s es Double
uniform' min max field = Model $ do
  env :: MRec s <- accessEnv
  let maybe_y = env ^. field
  send (UniformDist min max maybe_y)
