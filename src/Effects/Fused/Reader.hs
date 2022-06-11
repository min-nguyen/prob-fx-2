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

data ReaderE r m k where
  Ask :: ReaderE r m r

newtype ReaderC r a = ReaderC { runReaderC :: r -> a } deriving (Functor, Applicative, Monad)

ask :: (Member (ReaderE r) sig, Algebra sig m) => m r
ask = send Ask

-- handleReader :: forall env es a. env -> Prog (Reader env ': es) a -> Prog es a
-- handleReader env = loop where
--   loop :: Prog (Reader env ': es) a -> Prog es a
--   loop (Val x) = return x
--   loop (Op u k) = case discharge u of
--     Right Ask -> loop (k env)
--     Left  u'  -> Op u' (loop . k)

instance Algebra (ReaderE r)  -- sig
                 (ReaderC r)  -- m
                 where
  alg :: (Algebra sig m, Functor ctx) => Handler ctx n m -> sig n a -> ctx () -> m (ctx a)
  alg h = undefined