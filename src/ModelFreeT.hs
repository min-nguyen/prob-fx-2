
{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, DerivingStrategies, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, PolyKinds, UndecidableSuperClasses, TemplateHaskell, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, OverloadedLabels, UndecidableInstances, FunctionalDependencies, TypeFamilyDependencies #-}

module ModelFreeT where

import Dist
import FreeT
import Util
import GHC.Types
import GHC.TypeLits
import Data.Kind
import Data.Extensible hiding (wrap, Head)
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State

mkField "mu sigma y"

type Model s a = FreeT Dist (State (Record s)) a

data HList (l::[*]) :: * where
  HNil  :: HList '[]
  HCons :: e -> HList l -> HList (e ': l)

type family Maybes (as :: [k]) = (bs :: [k]) | bs -> as where
  Maybes ((f :> v) : as) = (f :> Maybe v) : Maybes as
  Maybes (a : as) = Maybe a : Maybes as
  Maybes '[] = '[]

-- HasVar: Lookup for maybe types
class Lookup xs k (Maybe v) => HasVar xs k v  where

instance Lookup xs k (Maybe v) => HasVar xs k v where

type Vars s = Record (Maybes s)

type X =
    '[  "mu"    ':>  Double
      , "sigma" ':>  Double
      , "y"     ':>  Double
     ]

exampleParams :: Vars X
exampleParams = mu @= Just 5 <: sigma @= Just 2 <: y @= Just 0 <: emptyRecord

access :: Getting a (s :& Field Identity) a
       -> FreeT Dist (State (Record s)) a
access f = do
    state <- lift get
    return (state ^. f)

normal :: (a ~ Maybe Double)
       => Double -> Double
       -> FreeT Dist (State (Record s)) Double
normal mu sigma  = do
  suspend (NormalDist mu sigma Nothing return)

normal' :: (a ~ Maybe Double)
       => Double -> Double
       -> Getting a (s :& Field Identity) a
       -> FreeT Dist (State (Record s)) Double
normal' mu sigma field = do
  y <- access field
  suspend (NormalDist mu sigma y return)

bernoulli :: (a ~ Maybe Bool)
          => Double
          -> FreeT Dist (State (Record s)) Bool
bernoulli p = do
  suspend (BernoulliDist p Nothing return)

bernoulli' :: (a ~ Maybe Bool)
          => Double -> Getting a (s :& Field Identity) a
          -> FreeT Dist (State (Record s)) Bool
bernoulli' p field = do
  y <- access field
  suspend (BernoulliDist p y return)

runModelFree :: Model s a -> State (Record s) a
runModelFree model = do
  let loop v = do
          x <- runFreeT v
          case x of FreeF (NormalDist mu sigma y f) -> loop (f 5)
                    Pure v -> return v
  loop model

runModel :: Model s a -> Record s -> (a, Record s)
runModel model = runState (runModelFree model)