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

module Fused.Reader where

import Fused.Lift hiding (Handler)
import Fused.Algebra 
import Fused.Sum

{- 
  Higher-order signatures, sig m a, are a refinement of first-order signatures, sig k. In particular, the rest of the computation 'k' corresponds to a monadic computation 'm a'.

    data Sig (m :: * -> *) (a :: *) where
      Op :: arg -> Sig m a

  We understand this as: the syntax of 'Sig' has an underlying computation type 'm'.
-}

{- A higher-order effect signature, sig m a, with underlying carrier type m. -}

data ReaderEff env m a where
  Ask :: ReaderEff env m env

ask :: (Member (ReaderEff env) sig, Algebra sig m) => m env
ask = send Ask

{- One of the computation carrier types 'm' that the effect as a singleton signature can be interpreted to -}

newtype Reader env a = Reader { runReader :: env -> a } 
  deriving (Functor, Applicative, Monad)

instance Algebra (ReaderEff env)   -- sig
                 (Reader env)      -- m
                 where
  alg :: (Functor ctx) => 
         Handler ctx n (Reader env) -> (ReaderEff env) n a -> ctx () -> Reader env (ctx a)
  alg hdl op ctx = case op of
    Ask -> Reader (\env -> fmap (const env) ctx) 

{- One of the computation carrier types 'm' that the effect in a larger signature can be interpreted to -}

newtype ReaderT env n a = ReaderT { runReaderT :: env -> n a } 
  deriving (Functor)

instance (Applicative m) => Applicative (ReaderT r m) where
  pure x  = ReaderT (\env -> pure x)
  f <*> v = ReaderT $ \ r -> runReaderT f r <*> runReaderT v r

instance (Monad m) => Monad (ReaderT r m) where
    return   = pure
    m >>= k  = ReaderT $ \ r -> do
        a <- runReaderT m r
        runReaderT (k a) r

instance Algebra es ms => 
         Algebra (ReaderEff env :+: es) (ReaderT env ms) where
  alg :: (Functor ctx)
      => (forall x. ctx (n x) -> ReaderT env ms (ctx x))
      -> (ReaderEff env :+: es) n a
      -> ctx () 
      -> ReaderT env ms (ctx a)
  alg hdl op ctx = case op of
    L (Ask :: ReaderEff env n a)
        -> ReaderT (\env -> pure $ fmap (const env) ctx) 
    R (op' :: es n a) 
        -> ReaderT (\env -> -- Calls a "smaller" algebra to interpret effect `es n a` to underlying computation `ms a`
                            alg
                            -- Calls a "smaller" handler of type `ctx (n x) -> ms (ctx x)`
                            ((`runReaderT` env) . hdl)
                            op' 
                            ctx)


        