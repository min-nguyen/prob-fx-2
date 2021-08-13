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

-- ask' :: (Member (Reader env) ts) => Freer (ts ++ '[Reader env]) env
-- ask' = Free (inj Ask) Pure

runReader :: forall env ts a. env -> Freer (Reader env ': ts) a -> Freer ts a
runReader env m = loop m where
  loop :: Freer (Reader env ': ts) a -> Freer ts a
  -- At this point, all Reader requests have been handled
  loop (Pure x) = return x
  -- Handle if Reader request, else ignore and go through the rest of the tree (by leaving the request's continuation k there to handle it, but also composing this with 'loop' so that the reader handler can then carry on afterwards).
  loop (Free u k) = case decomp u of
    Right Ask -> loop (k env)
    Left  u'  -> Free u' (loop . k)
