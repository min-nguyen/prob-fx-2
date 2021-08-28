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
    ( TypeError, ErrorMessage(Text, (:<>:), (:$$:), ShowType) )
import Data.Typeable
import qualified Extensible.OpenSum as OpenSum
{- Extensible effects without Typeable in Union, using Freer monad -}

{- Unions -}
data Union (ts :: [* -> *]) x where
  Union :: Int -> t x -> Union ts x

-- instance Functor (Union ts) where
--   fmap f (Union i tx) = Union i (fmap f tx)

newtype P t ts = P {unP :: Int}

class FindElem (t :: * -> *) r where
  findElem :: P t r

instance {-# INCOHERENT  #-} FindElem t (t ': r) where
  findElem = P 0
instance {-# OVERLAPPABLE #-} FindElem t r => FindElem t (t' ': r) where
  findElem = P $ 1 + (unP $ (findElem :: P t r))
instance TypeError ('Text "Cannot unify effect types." ':$$:
                    'Text "Unhandled effect: " ':<>: 'ShowType t ':$$:
                    'Text "Perhaps check the type of effectful computation and the sequence of handlers for concordance?")
  => FindElem t '[] where
  findElem = error "unreachable"

class (FindElem t ts) => Member (t :: * -> *) (ts :: [* -> *]) where
  inj ::  t x -> Union ts x
  prj ::  Union ts x -> Maybe (t x)

instance {-# INCOHERENT #-} (t ~ t') => Member t '[t'] where
   inj x          = Union 0 x
   prj (Union _ x) = Just (unsafeCoerce x)

instance (FindElem t ts) => Member t ts where
  inj = inj' (unP (findElem :: P t ts))
  prj = prj' (unP (findElem :: P t ts))

inj' :: Int -> t v -> Union r v
inj' = Union

prj' :: Int -> Union r v -> Maybe (t v)
prj' n (Union n' x) | n == n'   = Just (unsafeCoerce x)
                    | otherwise = Nothing

{- We want to handle a request of type t, where we state that t must be at the front of the list of requests (we know that the index is 0). If the request tv is indeed of type t (its index is 0), then we can unsafe coerce the tv to be of type 't v'. Otherwise, we return rv which is a request of a different type, and we can safely remove the request 't' from the front of the union at _this_ level of the free monad.  -}
decomp :: Union (t ': r) v -> Either (Union r v) (t v)
decomp (Union 0 tv) = Right $ unsafeCoerce tv
decomp (Union n rv) = Left  $ Union (n-1) rv

-- | Unique Member
type family UMember (b :: Bool) (t :: * -> *) (ts :: [* -> *]) :: Bool where
  UMember 'True t (t ': ts)   = 'False
  UMember 'True t (t' ': ts)  = UMember 'True t ts
  UMember 'True t '[]         = 'True
  UMember 'False t (t ': ts)  = UMember 'True t ts
  UMember 'False t (t' ': ts) = UMember 'False t ts
  UMember 'False t '[]        = 'False

class    (UMember 'False t ts ~ True) => UniqueMember t ts
instance (UMember 'False t ts ~ True) => UniqueMember t ts

-- | Last effect in effect list
class Member m effs => LastMember m effs | effs -> m
instance {-# OVERLAPPABLE #-} LastMember m effs => LastMember m (eff ': effs)
instance LastMember m (m ': '[])

-- | Freer monad
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

handleRelay ::
     (a -> Freer ts b)
  -> (forall x. t x -> (x -> Freer ts b) -> Freer ts b)
  -> Freer (t ': ts) a
  -> Freer ts b
handleRelay ret _ (Pure x) = ret x
handleRelay ret h (Free u k) =
  case decomp u of
    Right x  -> h x (handleRelay ret h . k)
    Left  u' -> Free u' (handleRelay ret h . k)

handleRelaySt ::
     s
  -> (s -> a -> Freer ts b)
  -> (forall x. s -> t x -> (s -> x -> Freer ts b) -> Freer effs b)
handleRelaySt = undefined

run :: Freer '[] a -> a
run (Pure x) = x
run _ = error "'run' isn't defined for non-pure computations"

runM :: forall m a ts. (Monad m, LastMember m ts) => Freer ts a -> m a
runM (Pure x) = return x
runM (Free u k) =
  let x = prj @m u
  in case x of
      Just mb -> mb >>= runM . k
      Nothing -> error "Impossible: Nothing cannot occur"

send :: Member t ts => t x -> Freer ts x
send t = Free (inj t) Pure

sendM :: (Monad m, LastMember m ts) => m a -> Freer ts a
sendM = send
