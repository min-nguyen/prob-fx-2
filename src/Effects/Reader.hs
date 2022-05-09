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


module Effects.Reader where

import Prog
import Data.Extensible hiding (Member)

data Reader env a where
  Ask :: Reader env env

ask :: (Member (Reader env) es) => Prog es env
ask = Op (inj Ask) Val

runReader :: forall env es a. env -> Prog (Reader env ': es) a -> Prog es a
runReader env = loop where
  loop :: Prog (Reader env ': es) a -> Prog es a
  loop (Val x) = return x
  loop (Op u k) = case discharge u of
    Right Ask -> loop (k env)
    Left  u'  -> Op u' (loop . k)

runReader' :: env -> Prog (Reader env ': es) a -> Prog es a
runReader' env = handleRelay return (\Ask k -> k env)

runReader'' :: forall es env a.
  LastMember IO es =>
  Member (Reader env) es => env -> Prog es a -> Prog es a
runReader'' env =
  interpose @(Reader env) return
  (\Ask k -> do sendM (print "hi")
                k env)
