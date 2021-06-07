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


module Extensible.State where

import Extensible.Freer

data State env a where
  Get :: State env env
  Put :: env -> State env ()

get :: (Member (State env) rs) => Freer rs env
get = Free (inj Get) Pure

put :: (Member (State env) rs) => env -> Freer rs ()
put s = Free (inj $ Put s) Pure
 
runState :: forall env rs a. env -> Freer (State env ': rs) a -> Freer rs a
runState env m = loop env m where
  loop :: env -> Freer (State env ': rs) a -> Freer rs a
  -- At this point, all Reader requests have been handled
  loop env (Pure x) = return x
  -- Handle if Reader request, else ignore and go through the rest of the tree (by leaving the request's continuation k there to handle it, but also composing this with 'loop' so that the reader handler can then carry on afterwards).
  loop env (Free u k) = case decomp u of 
    Right Get        -> loop env (k env)
    Right (Put env') -> loop env' (k ())
    Left  u'         -> Free u' (loop env . k)
