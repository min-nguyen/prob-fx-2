
{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, DerivingStrategies, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, PolyKinds, UndecidableSuperClasses, TemplateHaskell, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, OverloadedLabels, UndecidableInstances, FunctionalDependencies, TypeFamilyDependencies #-}

module ModelFreeT where

import Dist
import FreeT
import Util
import GHC.Types
import GHC.TypeLits
import Data.Kind
import Data.Profunctor.Unsafe
import Data.Extensible hiding (wrap, Head)
import Control.Lens hiding ((:>))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State
import Control.Monad.Reader

mkField "mu sigma y ys"

type ModelT s m a = FreeT Dist (ReaderT (Record (Maybes s)) m) a

type Model s a = ModelT s Identity a

type family Fields (as :: [Assoc k v]) :: [v] where
  Fields ((f :> v) : as) = v : Fields as
  Fields '[] = '[]

{- Handling Accessible Model Variables -}
data HList (l::[*]) :: * where
  HNil  :: HList '[]
  HCons :: e -> HList l -> HList (e ': l)

type family Maybes (as :: [k]) = (bs :: [k]) | bs -> as where
  Maybes ((f :> v) : as) = (f :> Maybe v) : Maybes as
  Maybes (a : as) = Maybe a : Maybes as
  Maybes '[] = '[]

-- HasVar: Lookup for maybe types
class Lookup (Maybes xs) k (Maybe v) => HasVar xs k v  where

instance Lookup (Maybes xs) k (Maybe v) => HasVar xs k v where

type Vars s = Record (Maybes s)

type X =
    '[  "mu"    ':>  Double
      , "sigma" ':>  Double
      , "y"     ':>  Double
     ]

-- fields :: 
f = HCons (Just 5) (HCons True HNil)

g :: Wrapper h => FieldName k -> Repr h v -> Field h (k ':> v)
g k v = k @= v

h :: Field h kv -> h (TargetOf kv)
h (Field i) = i

me :: Wrapper h => FieldName k -> Repr h v -> Field h (k ':> v)
me _ = Field #. review _Wrapper

exampleParams :: Vars X
exampleParams = mu @= Just 5 <: sigma @= Just 2 <: y @= Just 0 <: emptyRecord

access :: Monad m
  => Getting a (s :& Field Identity) a
  -> FreeT Dist (ReaderT (Record s) m) a
access f = do
    env <- lift ask
    return (env ^. f)

{- Distribution functions -}
normal :: (a ~ Maybe Double, Monad m)
  => Double -> Double -> Maybe Double 
  -> FreeT Dist (ReaderT (Record s) m) Double
normal mu sigma maybe_y = do
  suspend (NormalDist mu sigma maybe_y return)

normal' :: (a ~ Maybe Double, Monad m)
  => Double -> Double -> Getting a (s :& Field Identity) a
  -> FreeT Dist (ReaderT (Record s) m) Double
normal' mu sigma field = do
  y <- access field
  suspend (NormalDist mu sigma y return)

bernoulli :: (a ~ Maybe Bool, Monad m)
  => Double -> Maybe Bool
  -> FreeT Dist (ReaderT (Record s) m) Bool
bernoulli p maybe_y = do
  suspend (BernoulliDist p maybe_y return)

bernoulli' :: (a ~ Maybe Bool, Monad m)
  => Double -> Getting a (s :& Field Identity) a
  -> FreeT Dist (ReaderT (Record s) m) Bool
bernoulli' p field = do
  y <- access field
  suspend (BernoulliDist p y return)

binomial :: (a ~ Maybe Int, Monad m)
  => Int -> Double -> Maybe Int
  -> FreeT Dist (ReaderT (Record s) m) Int
binomial n p maybe_y = do
  suspend (BinomialDist n p maybe_y return)

binomial' :: (a ~ Maybe Int, Monad m)
  => Int -> Double -> Getting a (s :& Field Identity) a
  -> FreeT Dist (ReaderT (Record s) m) Int
binomial' n p field = do
  y <- access field
  suspend (BinomialDist n p y return)

{- Executing Models -}
runModelFree :: Monad m => ModelT s m a -> ReaderT (Record (Maybes s)) m a
runModelFree model = do
  let loop v = do
          x <- runFreeT v
          case x of FreeF (NormalDist mu sigma y f) -> loop (f 5)
                    Pure v -> return v
  loop model

runModel :: Monad m => ModelT s m a -> Record (Maybes s) -> m a
runModel model = runReaderT (runModelFree model)