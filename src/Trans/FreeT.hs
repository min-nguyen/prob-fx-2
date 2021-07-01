
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Trans.FreeT where

import Control.Monad
import Control.Monad.Trans.Class
import Data.Extensible hiding (wrap, Head)

data FreeF f a x = Pure a | FreeF (f x)

instance Functor f => Functor (FreeF f a) where
   fmap g (FreeF fx) = FreeF (fmap g fx)
   fmap g (Pure x)  = Pure x

newtype FreeT f m a = FreeT { runFreeT :: m (FreeF f a (FreeT f m a)) }

instance (Functor f, Monad m) => Functor (FreeT f m) where
    fmap = liftM

instance (Functor f, Monad m) => Applicative (FreeT f m) where
    pure  = return
    (<*>) = ap

instance (Functor f, Monad m) => Monad (FreeT f m) where
    return  = FreeT . return . Pure
    m >>= f = FreeT $ do
        x <- runFreeT m
        runFreeT $ case x of
            Pure a -> f a
            FreeF w -> wrap $ fmap (>>= f) w

instance MonadTrans (FreeT f) where
    lift = FreeT . fmap Pure

-- | Prepend one layer of the functor to the free monad
wrap :: (Monad m) => f (FreeT f m a) -> FreeT f m a
wrap = FreeT . return . FreeF

-- | Convert one layer of a functor into an operation in the free monad
liftF :: (Functor f, Monad m) => f a -> FreeT f m a
liftF x = wrap $ fmap return x

suspend :: (Monad m, Functor f) => f (FreeT f m a) -> FreeT f m a
suspend fx = FreeT (return (FreeF fx))