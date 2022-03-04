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

ask :: (Member (Reader env) ts) => Prog ts env
ask = Op (inj Ask) Val

runReader :: forall env ts a. env -> Prog (Reader env ': ts) a -> Prog ts a
runReader env = loop where
  loop :: Prog (Reader env ': ts) a -> Prog ts a
  loop (Val x) = return x
  loop (Op u k) = case decomp u of
    Right Ask -> loop (k env)
    Left  u'  -> Op u' (loop . k)

runReader' :: env -> Prog (Reader env ': ts) a -> Prog ts a
runReader' env = handleRelay return (\Ask k -> k env)

runReader'' :: forall ts env a.
  LastMember IO ts =>
  Member (Reader env) ts => env -> Prog ts a -> Prog ts a
runReader'' env =
  interpose @(Reader env) return
  (\Ask k -> do sendM (print "hi")
                k env)
