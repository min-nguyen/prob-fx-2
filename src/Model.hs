{-# LANGUAGE RankNTypes, GADTs, TypeApplications, FlexibleInstances, DerivingStrategies, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, PolyKinds, UndecidableSuperClasses, TemplateHaskell, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, OverloadedLabels, UndecidableInstances, FunctionalDependencies, TypeFamilyDependencies #-}

module Model where

import Dist
import FreeT
import Sampler
import Util
import GHC.Generics
import GHC.Types
import GHC.TypeLits
import Data.Kind
import Data.Profunctor.Unsafe
import Data.Extensible hiding (wrap, Head)
import Control.Lens ( Identity, (^.), review, Getting )
import Control.Monad
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Monad.Reader
import Control.Monad.Trans.Identity


mkField "mu sigma y ys"

type ModelT s t a = FreeT Dist (ReaderT (Record (Maybes s)) (t Sampler)) a

type Model s a = ModelT s IdentityT a

type family Keys (as :: [Assoc k v]) :: [k] where
  Keys ((k :> v) : as) = k : Keys as
  Keys '[] = '[]

type family Fields (as :: [Assoc k v]) :: [v] where
  Fields ((k :> v) : as) = v : Fields as
  Fields '[] = '[]

{- Handling Accessible Model Variables -}
data HList (l::[*]) :: * where
  HNil  :: HList '[]
  HCons :: e -> HList l -> HList (e ': l)

type family Maybes (as :: [k]) = (bs :: [k]) | bs -> as where
  Maybes ((f :> v) : as) = (f :> Maybe v) : Maybes as
  Maybes (a : as) = Maybe a : Maybes as
  Maybes '[] = '[]

type family Nothings (as :: [k]) :: [Assoc k v] where
  Nothings (k:ks) = (k :> Nothing) : Nothings ks
  Nothings '[] = '[]

-- HasVar: Lookup for maybe types
class Lookup (Maybes xs) k (Maybe v) => HasVar xs k v where

instance Lookup (Maybes xs) k (Maybe v) => HasVar xs k v where

type MRec s = Record (Maybes s)

type X =
    '[  "mu"    ':>  Double
      , "sigma" ':>  Double
      , "y"     ':>  Double
     ]

class AllNothing (a :: [Assoc Symbol *]) where
  allNothing :: MRec a

instance AllNothing '[] where
  allNothing = emptyRecord

instance AllNothing xs => AllNothing ((x ':> y) ': xs) where
  allNothing = xlb (Proxy @x) @= Nothing <: allNothing

nothingRecord = allNothing :: MRec X

g :: Wrapper h => FieldName k -> Repr h v -> Field h (k ':> v)
g k v = k @= v

h :: Field h kv -> h (TargetOf kv)
h (Field i) = i

me :: Wrapper h => FieldName k -> Repr h v -> Field h (k ':> v)
me _ = Field #. review _Wrapper

exampleParams :: MRec X
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

-- FreeT Dist (ReaderT (Record (Maybes s)) m) a
{- Executing Models -}
runModelFree :: MonadTrans t => ModelT s t a -> ReaderT (MRec s) (t Sampler) a
runModelFree model = do
  let loop v = do
          x <- runFreeT v
          case x of FreeF (NormalDist mu sigma y f) -> loop (f 5)
                    Pure v -> return v
  loop model

runModel :: MonadTrans t => ModelT s t a -> MRec s -> t (Sampler) a
runModel model = runReaderT (runModelFree model)