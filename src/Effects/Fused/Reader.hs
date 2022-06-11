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

module Effects.Fused.Reader where

import Control.Algebra
import Control.Effect.Sum

{- A higher-order effect signature, sig m a, with underlying carrier type m. -}

data Reader env m a where
  Ask :: Reader env m env

{- One of the computation carrier types 'm' that the signature can be interpreted to -}

newtype ReaderC r a = ReaderC { runReaderC :: r -> a } 
  deriving (Functor, Applicative, Monad)

ask :: (Member (Reader env) sig, Algebra sig m) => m env
ask = send Ask

-- type Handler ctx n m = forall x. ctx (n x) -> m (ctx x)

instance Algebra (Reader env)   -- sig
                 (ReaderC env)  -- m
                 where
  alg :: (m ~ ReaderC env, sig ~ Reader env) =>
         (Algebra sig m, Functor ctx) => Handler ctx n m -> sig n a -> ctx () -> m (ctx a)
  alg hdl op ctx = case op of
    Ask -> ReaderC undefined 