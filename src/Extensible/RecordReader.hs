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

import Extensible.Freer
import Data.Extensible hiding (Member)
-- import Extensible.Model
import Control.Lens ( Identity, (^.), Getting )
import Util

type family AsList (as :: [k]) = (bs :: [k]) | bs -> as where
  -- AsList ((f :> [a]) : as) = ((f :> [a]) : AsList as)
  AsList ((f :> a) : as)   = ((f :> [a]) : AsList as)
  AsList '[] = '[]

data RecReader env a where
  Ask :: Getting [a] (Record env) [a] -> RecReader env (Maybe a)

ask :: (Member (RecReader env) rs) =>
  Getting [a] (env :& Field Identity) [a] -> Freer rs (Maybe a)
ask field = Free (inj $ Ask field) Pure

runReader :: forall env rs a . Record (AsList env) -> Freer (RecReader (AsList env) ': rs) a -> Freer rs a
runReader env = loop where
  loop :: Freer (RecReader (AsList env) ': rs) a -> Freer rs a
  -- At this point, all Reader requests have been handled
  loop (Pure x) = return x
  -- Handle if Reader request, else ignore and go through the rest of the tree (by leaving the request's continuation k there to handle it, but also composing this with 'loop' so that the reader handler can then carry on afterwards).
  loop (Free u k) = case decomp u of
    Right (Ask f) -> let ys = env ^. f
                         y  = maybeHead ys
                     in  loop (k y)
    Left  u'      -> Free u' (loop . k)
