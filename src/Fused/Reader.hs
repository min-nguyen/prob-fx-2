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

import Fused.Algebra ( Algebra(..), send )
import Fused.Sum
import qualified Control.Monad.Trans.Reader as Reader

{- A higher-order effect signature, sig m a, with underlying carrier type m. -}

data ReaderEff env m a where
  Ask :: ReaderEff env m env

ask :: (Member (ReaderEff env) sig, Algebra sig m) => m env
ask = send Ask

{- One of the computation carrier types 'm' that the signature can be interpreted to -}

newtype Reader env a = Reader { runReader :: env -> a } 
  deriving (Functor, Applicative, Monad)

type Handler ctx n m = forall x. ctx (n x) -> m (ctx x)

instance Algebra (ReaderEff env)   -- sig
                 (Reader env)      -- m
                 where
  alg :: (Functor ctx) => 
         Handler ctx n (Reader env) -> (ReaderEff env) n a -> ctx () -> Reader env (ctx a)
  alg hdl op ctx = case op of
    Ask -> Reader (\env -> fmap (const env) ctx) 

{- -}

newtype ReaderT env n a = ReaderT { runReaderT :: Reader.ReaderT env n a } 
  deriving (Functor, Applicative, Monad)

instance Algebra es ms => 
         Algebra (ReaderEff env :+: es) (ReaderT env ms) where
  alg :: (m ~ ReaderT env ms, sig ~ (ReaderEff env :+: es))=>
         (Algebra sig m, Functor ctx) => Handler ctx n m -> sig n a -> ctx () -> m (ctx a)
  alg _ _ _ = undefined