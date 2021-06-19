{-# LANGUAGE RankNTypes, GADTs, TypeApplications, FlexibleInstances, DerivingStrategies, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, PolyKinds, UndecidableSuperClasses, TemplateHaskell, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, OverloadedLabels, UndecidableInstances, FunctionalDependencies, TypeFamilyDependencies #-}
{-# LANGUAGE DeriveFunctor #-}

module ModelF where


-- import FreeT
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

data DistF a x
  = Pure a
  | NormalF Double Double (Maybe Double) (Double -> x)
  | BernoulliF Double (Maybe Bool) (Bool -> x)
  | BinomialF Int Double (Maybe Int) (Int -> x)

newtype DistT m a = DistT { runDistT :: m (DistF a (DistT m a)) }

suspend :: Monad m => DistF a (DistT m a) -> DistT m a
suspend fx = DistT (return fx)

instance Functor (DistF a) where
  fmap g (NormalF mu sigma y fx) = NormalF mu sigma y (fmap g fx)
  fmap g (BernoulliF p y fx) = BernoulliF p y (fmap g fx)
  fmap g (BinomialF n p y fx) = BinomialF n p y (fmap g fx)
  fmap g (Pure x)  = Pure x

instance (Monad m) => Functor (DistT m) where
    fmap = liftM

instance (Monad m) => Applicative (DistT m) where
    pure  = return
    (<*>) = ap

instance (Monad m) => Monad (DistT m) where
    return  = DistT . return . Pure
    m >>= f = DistT $ do
        x <- runDistT m
        runDistT $ case x of
            Pure a -> f a
            NormalF mu sigma y k -> DistT $ return $ NormalF mu sigma y (k >=> f)
            BernoulliF p y k -> DistT $ return $ BernoulliF p y (k >=> f)
            BinomialF n p y k -> DistT $ return $ BinomialF n p y (k >=> f)

instance MonadTrans DistT where
    lift = DistT . fmap Pure

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

class AllNothing (a :: [Assoc Symbol *]) where
  allNothing :: MRec a

instance AllNothing '[] where
  allNothing = emptyRecord

instance AllNothing xs => AllNothing ((x ':> y) ': xs) where
  allNothing = xlb (Proxy @x) @= Nothing <: allNothing

access :: Monad m
  => Getting a (s :& Field Identity) a
  -> DistT (ReaderT (Record s) m) a
access f = do
    env <- lift ask
    return (env ^. f)

{- Distribution functions -}
normal :: (a ~ Maybe Double, Monad m)
  => Double -> Double -> Maybe Double
  -> DistT (ReaderT (Record s) m) Double
normal mu sigma maybe_y = do
  suspend (NormalF mu sigma maybe_y return)

normal' :: (a ~ Maybe Double, Monad m)
  => Double -> Double -> Getting a (s :& Field Identity) a
  -> DistT (ReaderT (Record s) m) Double
normal' mu sigma field = do
  y <- access field
  suspend (NormalF mu sigma y return)

bernoulli :: (a ~ Maybe Bool, Monad m)
  => Double -> Maybe Bool
  -> DistT (ReaderT (Record s) m) Bool
bernoulli p maybe_y = do
  suspend (BernoulliF p maybe_y return)

bernoulli' :: (a ~ Maybe Bool, Monad m)
  => Double -> Getting a (s :& Field Identity) a
  -> DistT (ReaderT (Record s) m) Bool
bernoulli' p field = do
  y <- access field
  suspend (BernoulliF p y return)

binomial :: (a ~ Maybe Int, Monad m)
  => Int -> Double -> Maybe Int
  -> DistT (ReaderT (Record s) m) Int
binomial n p maybe_y = do
  suspend (BinomialF n p maybe_y return)

binomial' :: (a ~ Maybe Int, Monad m)
  => Int -> Double -> Getting a (s :& Field Identity) a
  -> DistT (ReaderT (Record s) m) Int
binomial' n p field = do
  y <- access field
  suspend (BinomialF n p y return)