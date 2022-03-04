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

get :: (Member (State s) ts) => Prog ts s
get = Op (inj Get) Val

put :: (Member (State s) ts) => s -> Prog ts ()
put s = Op (inj $ Put s) Val

modify :: Member (State s) ts => (s -> s) -> Prog ts ()
modify f = get >>= put . f

runState :: forall s ts a. s -> Prog (State s ': ts) a -> Prog ts (a, s)
runState s m = loop s m where
  loop :: s -> Prog (State s ': ts) a -> Prog ts (a, s)
  -- At this point, all Reader requests have been handled
  loop s (Val x) = return (x, s)
  -- Handle if Reader request, else ignore and go through the rest of the tree (by leaving the request's continuation k there to handle it, but also composing this with 'loop' so that the reader handler can then carry on afterwards).
  loop s (Op u k) = case decomp u of
    Right Get      -> loop s (k s)
    Right (Put s') -> loop s' (k ())
    Left  u'       -> Op u' (loop s . k)

runState' :: s -> Prog (State s ': ts) a -> Prog ts (a, s)
runState' s0  = handleRelaySt s0
  (\s x -> return (x, s))
  (\s tx k -> case
      tx of Get    -> k s s
            Put s' -> k s' ())

-- installIO :: forall s ts a b.
--   Member (State s) ts => s ->
--   Prog ts a -> Prog ts b
-- installIO s = install ret h
--   where
--    ret = (\x -> return x)
--    h   = (\tx k -> case
--               tx of Get -> k)