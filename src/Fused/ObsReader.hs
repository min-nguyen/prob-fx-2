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
{-# LANGUAGE InstanceSigs #-}


module Fused.ObsReader where

import Fused.Algebra
import Fused.Sum
import Env
import Util

-- | Effect
data ObsReader env m (a :: *) where
  Ask :: Observable env x a => ObsVar x -> ObsReader env m (Maybe a)

ask :: forall sig env es x m a. (Member (ObsReader env) sig, Algebra sig m) => Observable env x a => ObsVar x -> m (Maybe a)
ask k = send (Ask k :: ObsReader env m (Maybe a))

-- | Carrier
newtype ObsReaderC env m a = ObsReaderC {runObsReaderC :: Env env -> m (Env env, a)}
  deriving Functor

instance Monad m => Applicative (ObsReaderC env m) where
  pure a = ObsReaderC (\ s -> pure (s, a))

  ObsReaderC f <*> ObsReaderC a = ObsReaderC $ \ s -> do
    (s', f') <- f s
    (s'', a') <- a s'
    pure (s'', f' a')

instance Monad m => Monad (ObsReaderC env m) where
  ObsReaderC m >>= f = ObsReaderC $ \ s -> do
    (s', a) <- m s
    runObsReaderC (f a) s'

-- | Algebra
instance Algebra sig m => Algebra (ObsReader env :+: sig) (ObsReaderC env m) where
  alg :: (Functor ctx)
      => (forall x. ctx (n x) -> ObsReaderC env m (ctx x))
      -> (ObsReader env :+: sig) n a
      -> ctx () 
      -> ObsReaderC env m (ctx a)
  alg hdl sig ctx = ObsReaderC $ \env -> case sig of
    L (Ask x) -> do
      let ys = get x env
          y  = maybeHead ys
          env' = set x (safeTail ys) env
      pure (env', y <$ ctx)
    R other   -> thread ((uncurry runObsReaderC . swap) ~<~ hdl) other (env, ctx)
  {-# INLINE alg #-}
