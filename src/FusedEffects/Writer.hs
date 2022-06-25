{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications, UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
{-# LANGUAGE TupleSections #-}

module FusedEffects.Writer where

import FusedEffects.Algebra ( Algebra(..), send )
import FusedEffects.Sum

{- A higher-order effect signature, sig m a, with underlying carrier type m. -}

data WriterEff w m a where
  Tell :: w -> WriterEff w m ()

tell :: (Member (WriterEff w) sig, Algebra sig m) => w -> m ()
tell = send . Tell

{- One of the computation carrier types 'm' that the effect as a singleton signature can be interpreted to -}

newtype Writer w a = Writer { runWriter :: (a, w) } 
  deriving Functor

instance Monoid w => Applicative (Writer w) where
  pure x = Writer (x, mempty)
  Writer (f, w) <*> Writer (a, v) = Writer (f a, w <> v)

instance Monoid w => Monad (Writer w) where
  return = pure
  Writer (x, w) >>= f = 
    let Writer (y, v) = f x
    in  Writer (y, w <> v)

type Handler ctx n m = forall x. ctx (n x) -> m (ctx x)

instance Monoid w => Algebra (WriterEff w)   -- sig
                             (Writer w)      -- m
                     where
  alg :: (Functor ctx) => 
         Handler ctx n (Writer w) -> (WriterEff w) n a -> ctx () -> Writer w (ctx a)
  alg hdl op ctx = case op of
    Tell w -> Writer (fmap (const ()) ctx, w) 


{- One of the computation carrier types 'm' that the effect in a larger signature can be interpreted to -}

newtype WriterT w n a = WriterT { runWriterT :: n (a, w) } 
  deriving Functor

liftA2 :: Applicative f => (a1 -> a2 -> b) -> f a1 -> f a2 -> f b
liftA2 f x = (fmap f x <*>)

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
  pure x  = WriterT (pure (x, mempty))
  WriterT mf <*> WriterT ma = WriterT (liftA2 k mf ma)
    where k (f, w) (a, v) = (f a, w <> v)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return   = pure
    m >>= k  = WriterT $ do
      (a, w) <- runWriterT m 
      (b, v) <- runWriterT (k a)
      pure (b, w <> v)

instance (Monoid w, Algebra es ms) => 
         Algebra (WriterEff w :+: es) (WriterT w ms) where
  alg :: (Functor ctx)
      => (forall x. ctx (n x) -> WriterT w ms (ctx x))
      -> (WriterEff w :+: es) n a
      -> ctx () 
      -> WriterT w ms (ctx a)
  alg hdl op ctx = case op of
    L (Tell w :: WriterEff w n a)
      -> WriterT (pure (fmap (const ()) ctx, w))
    R (op' :: es n a) 
      -> WriterT ((, mempty) 
                  <$> alg -- Calls a "smaller" algebra to interpret effect `es n a` to underlying computation `ms a`
                      ((fst <$>) . runWriterT . hdl) -- Calls a "smaller" handler of type `ctx (n x) -> ms (ctx x)`
                      op' 
                      ctx
                 )
