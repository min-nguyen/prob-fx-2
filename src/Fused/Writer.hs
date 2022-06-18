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

module Fused.Writer where

import Fused.Algebra ( Algebra(..), send )
import Fused.Sum

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
liftA2 f x = (<*>) (fmap f x)

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
  pure x  = WriterT (pure (x, mempty))
  WriterT mf <*> WriterT ma = WriterT (liftA2 k mf ma)
    where k (f, w) (a, v) = (f a, w <> v)

-- instance (Monad m) => Monad (ReaderT r m) where
--     return   = pure
--     m >>= k  = ReaderT $ \ r -> do
--         a <- runReaderT m r
--         runReaderT (k a) r

-- instance Algebra es ms => 
--          Algebra (ReaderEff env :+: es) (ReaderT env ms) where
--   alg :: (Functor ctx)
--       => (forall x. ctx (n x) -> ReaderT env ms (ctx x))
--       -> (ReaderEff env :+: es) n a
--       -> ctx () 
--       -> ReaderT env ms (ctx a)
--   alg hdl op ctx = case op of
--     L (Ask :: ReaderEff env n a)
--         -> ReaderT (\env -> pure $ fmap (const env) ctx) 
--     R (op' :: es n a) 
--         -> ReaderT (\env -> -- Calls a "smaller" algebra to interpret effect `es n a` to underlying computation `ms a`
--                             alg
--                             -- Calls a "smaller" handler of type `ctx (n x) -> ms (ctx x)`
--                             ((`runReaderT` env) . hdl)
--                             op' 
--                             ctx)


        