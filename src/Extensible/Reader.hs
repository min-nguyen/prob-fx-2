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


module Extensible.Reader where

import Extensible.Freer
import Data.Extensible hiding (Member)

data Reader env a where
  Ask :: Reader env env

ask :: (Member (Reader env) ts) => Freer ts env
ask = Free (inj Ask) Pure

runReader :: forall env ts a. env -> Freer (Reader env ': ts) a -> Freer ts a
runReader env = loop where
  loop :: Freer (Reader env ': ts) a -> Freer ts a
  loop (Pure x) = return x
  loop (Free u k) = case decomp u of
    Right Ask -> loop (k env)
    Left  u'  -> Free u' (loop . k)

runReader' :: env -> Freer (Reader env ': ts) a -> Freer ts a
runReader' env = handleRelay return (\Ask k -> k env)

-- runReader'' :: forall ts env a.
--   LastMember IO ts =>
--   Member (Reader env) ts => env -> Freer ts a -> Freer ts a
-- runReader'' env =
--   interpose @(Reader env) return
--   (\Ask k -> do sendM (print "hi")
--                 k env)
