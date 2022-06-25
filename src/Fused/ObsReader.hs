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


module Fused.ObsReader where

import Fused.Algebra
import Fused.Sum
import Env
import Util

-- | Effect
data ObsReader env m (a :: *) where
  Ask :: Observable env x a => ObsVar x -> ObsReader env m (Maybe a)
  -- Get :: ObsReader env m (Env env)
  -- Put :: Env env -> ObsReader env m ()

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
  alg hdl sig ctx = ObsReaderC $ \ s -> case sig of
    -- L Ask
    -- L Get     -> pure (s, s <$ ctx)
    -- L (Put s) -> pure (s, ctx)
    R other   -> thread ((uncurry runObsReaderC . swap) ~<~ hdl) other (s, ctx)
  {-# INLINE alg #-}



-- handleObsRead :: forall env es a.
--   Env env -> Prog (ObsReader env ': es) a -> Prog es a
-- handleObsRead env (Val x) = return x
-- handleObsRead env (Op (AskPatt key) k) = do
--     let ys = get key env
--         y  = maybeHead ys
--         env' = set key (safeTail ys) env
--     handleObsRead env' (k y)
-- handleObsRead env (Op (Other u) k) = Op u (handleObsRead env . k)
