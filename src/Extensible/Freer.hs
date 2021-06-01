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
{-# LANGUAGE IncoherentInstances #-}

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

instance {-# INCOHERENT #-} FindElem t (t ': r) where
  findElem = P 0
instance {-# INCOHERENT #-} FindElem t r => FindElem t (t' ': r) where
  findElem = P $ 1 + (unP $ (findElem :: P t r))
instance TypeError ('Text "Cannot unify effect types." ':$$:
                    'Text "Unhandled effect: " ':<>: 'ShowType t ':$$:
                    'Text "Perhaps check the type of effectful computation and the sequence of handlers for concordance?")
  => FindElem t '[] where
  findElem = error "unreachable"

class (FindElem t rs) => Member (t :: * -> *) (rs :: [* -> *]) where
  inj ::  t x -> Union rs x
  prj ::  Union rs x -> Maybe (t x)

instance {-# OVERLAPPABLE #-} (t ~ s) => Member t '[s] where
   inj x          = Union 0 x
   prj (Union _ x) = Just (unsafeCoerce x)
 
instance {-# INCOHERENT #-} (FindElem t rs) => Member t rs where
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

class EQU (a :: k) (b :: k) p | a b -> p
instance EQU a a 'True
instance (p ~ 'False) => EQU a b p

class Member t r => SetMember (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance (EQU t1 t2 p, MemberU' p tag t1 (t2 ': r)) => SetMember tag t1 (t2 ': r)

class Member t r =>
      MemberU' (f::Bool) (tag :: k -> * -> *) (t :: * -> *) r | tag r -> t
instance MemberU' 'True tag (tag e) (tag e ': r)
instance (Member t (t' ': r), SetMember tag t r) =>
           MemberU' 'False tag t (t' ': r)

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

send :: Member t rs => t x -> Freer rs x
send t = Free (inj t) Pure
