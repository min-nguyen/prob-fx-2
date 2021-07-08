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
{-# LANGUAGE TypeFamilyDependencies #-}

module Extensible.RecordReader where

import Extensible.State
import Extensible.Freer
import Data.Extensible hiding (Member)
-- import Extensible.Model
import Control.Lens hiding ((:>))
import Util

type family AsList (as :: [k]) = (bs :: [k]) | bs -> as where
  AsList ((f :> a) : as)   = ((f :> [a]) : AsList as)
  AsList '[] = '[]

type LRec s = Record (AsList s)

-- A Reader for which it is possible to access fields with type [a].
data RecReader env a where
  Ask :: Lens' (Record env) [a] -> RecReader env (Maybe a)

ask :: (Member (RecReader env) rs) =>
  Lens' (Record env) [a] -> Freer rs (Maybe a)
ask field = Free (inj $ Ask field) Pure

runRecReader :: forall env rs a.
  (Member (State (LRec env)) rs) =>
  LRec env -> Freer (RecReader (AsList env) ': rs) a -> Freer rs a
runRecReader env = loop where
  loop :: Freer (RecReader (AsList env) ': rs) a -> Freer rs a
  loop (Pure x) = return x
  loop (Free u k) = case decomp u of
    Right (Ask f) -> do
      let ys = env ^. f
          y  = maybeHead ys
      modify (set f (safeTail ys))
      loop (k y)
    Left  u'  -> Free u' (loop . k)
