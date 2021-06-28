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

data State s a where
  Get :: State s s
  Put :: s -> State s ()

get :: (Member (State s) rs) => Freer rs s
get = Free (inj Get) Pure

put :: (Member (State s) rs) => s -> Freer rs ()
put s = Free (inj $ Put s) Pure

modify :: Member (State s) rs => (s -> s) -> Freer rs ()
modify f = get >>= put . f

runState :: forall s rs a. s -> Freer (State s ': rs) a -> Freer rs (a, s)
runState s m = loop s m where
  loop :: s -> Freer (State s ': rs) a -> Freer rs (a, s)
  -- At this point, all Reader requests have been handled
  loop s (Pure x) = return (x, s)
  -- Handle if Reader request, else ignore and go through the rest of the tree (by leaving the request's continuation k there to handle it, but also composing this with 'loop' so that the reader handler can then carry on afterwards).
  loop s (Free u k) = case decomp u of
    Right Get      -> loop s (k s)
    Right (Put s') -> loop s' (k ())
    Left  u'       -> Free u' (loop s . k)
