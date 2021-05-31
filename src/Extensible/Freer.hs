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


module Extensible.Freer where

import Control.Monad
import Unsafe.Coerce
import Data.Kind (Constraint)
import GHC.TypeLits
import Data.Typeable

{- Extensible effects without Typeable in Union, using Freer monad -}

{- Unions -}
data Union (rs :: [* -> *]) x where
  Union :: Int -> t x -> Union rs x

-- instance Functor (Union rs) where
--   fmap f (Union i tx) = Union i (fmap f tx)
  
newtype P t rs = P {unP :: Int}

class FindElem (t :: * -> *) r where
  findElem :: P t r

instance FindElem t (t ': r) where
  findElem = P 0
instance {-# OVERLAPPABLE #-} FindElem t r => FindElem t (t' ': r) where
  findElem = P $ 1 + (unP $ (findElem :: P t r))
instance TypeError ('Text "Cannot unify effect types." ':$$:
                    'Text "Unhandled effect: " ':<>: 'ShowType t ':$$:
                    'Text "Perhaps check the type of effectful computation and the sequence of handlers for concordance?")
  => FindElem t '[] where
  findElem = error "unreachable"

class (FindElem t rs) => Member (t :: * -> *) (rs :: [* -> *]) where
  inj ::  t x -> Union rs x
  prj ::  Union rs x -> Maybe (t x)

instance (t ~ s) => Member t '[s] where
   inj x          = Union 0 x
   prj (Union _ x) = Just (unsafeCoerce x)

instance (FindElem t rs) => Member t rs where
  inj = inj' (unP (findElem :: P t rs))
  prj = prj' (unP (findElem :: P t rs))

inj' :: Int -> t v -> Union r v
inj' = Union

prj' :: Int -> Union r v -> Maybe (t v)
prj' n (Union n' x) | n == n'   = Just (unsafeCoerce x)
                    | otherwise = Nothing

{- We want to handle a request of type t, where we state that t must be at the front of the list of requests (we know that the index is 0). If the request tv is indeed of type t (its index is 0), then we can unsafe coerce the tv to be of type 't v'. Otherwise, we return rv which is a request of a different type, and we can safely remove the request 't' from the front of the union at _this_ level of the free monad.  -}
decomp :: Union (t ': r) v -> Either (Union r v) (t v)
decomp (Union 0 tv) = Right $ unsafeCoerce tv
decomp (Union n rv) = Left  $ Union (n-1) rv

{- Eff and VE -}
-- data Free f a = Pure a |  Free (Union f (Free f a))

data Freer f a where
  Pure :: a -> Freer f a
  Free :: Union f x -> (x -> Freer f a) -> Freer f a

instance Functor (Freer f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free fx k) = Free fx (fmap f . k)

instance Applicative (Freer f) where
  pure = Pure
  Pure f <*> x = fmap f x
  (Free fx k) <*> x = Free fx ((<*> x) . k)

instance Monad (Freer f) where
  return            = Pure
  Pure a >>= f      = f a
  Free fx k >>= f = Free fx (k >=> f)

run :: Freer '[] a -> a 
run (Pure x) = x

data Reader env a where
  Get :: Reader env env

get :: (Member (Reader env) rs) => Freer rs env
get = Free (inj Get) Pure

runReader :: forall env rs a. env -> Freer (Reader env ': rs) a -> Freer rs a
runReader env m = loop m where
  loop :: Freer (Reader env ': rs) a -> Freer rs a
  -- At this point, all Reader requests have been handled
  loop (Pure x) = return x
  -- Handle if Reader request, else ignore and go through the rest of the tree (by leaving the request's continuation k there to handle it, but also composing this with 'loop' so that the reader handler can then carry on afterwards).
  loop (Free u k) = case decomp u of 
    Right Get -> loop (k env)
    Left  u'  -> Free u' (loop . k)

data Writer w a where 
  Put :: w -> Writer w ()

put :: Member (Writer w) rs => w -> Freer rs ()
put w = Free (inj $ Put w) Pure

runWriter :: forall w rs a . Monoid w => Freer (Writer w ': rs) a -> Freer rs (a, w)
runWriter m = loop mempty m where
  loop ::  w -> Freer (Writer w ': rs) a -> Freer rs (a, w)
  -- At this point, all Reader requests have been handled
  loop w (Pure x) = return (x, w)
  -- Handle if Writer request, else ignore and go through the rest of the tree
  loop w (Free u k) = case decomp u of
    Right (Put w') -> loop (w `mappend` w') (k ())
    Left u'        -> Free u' (loop w . k)

example :: (Member (Reader Int) rs, Member (Writer String) rs) 
        => Freer rs Int
example = do
  put "hi"
  x :: Int <- get
  put "hi"
  return 5

prog :: (Member (Reader Int) rs, Member (Writer String) rs) 
        => Freer rs Int
prog = Free (inj $ Put "hi") Pure >>= \() -> 
         Free (inj Get) Pure >>= \(x :: Int) -> 
           Free (inj $ Put (show x)) Pure >>= \() -> 
             Pure 5

prog' :: (Member (Reader Int) rs, Member (Writer String) rs) => Freer rs Int
prog' = Free (inj $ Put "hi") (Pure >=> \() -> 
          Free (inj Get) (Pure >=> \(x :: Int) -> 
            Free (inj $ Put (show x)) (Pure >=> \() -> 
              Pure 5)))

prog'' :: (Member (Reader Int) rs, Member (Writer String) rs) => Freer rs Int
prog'' = Free (inj $ Put "hi") (\() -> 
          Free (inj Get) (\(x :: Int) -> 
            Free (inj $ Put (show x)) (\() -> 
              Pure 5)))

example' :: forall env. Freer '[Reader env, Writer String] Int
example' = do
  put "hi"
  x :: env <- get
  return 5

exampleR :: forall rs . (Member (Reader Int) rs) 
        => Freer rs Int
exampleR = do
  x :: Int <- get
  return 5

exampleW :: forall rs. (Member (Writer String) rs) 
        => Freer rs Int
exampleW = do
  put "hi"
  return 5

runEx :: (Int, String)
runEx = (run . runWriter . runReader (5 :: Int)) example