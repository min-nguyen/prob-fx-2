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

newtype P t ts = P {unP :: Int}

class FindElem (t :: * -> *) r where
  findElem :: P t r

instance {-# INCOHERENT  #-} FindElem t (t ': r) where
  findElem = P 0
instance {-# OVERLAPPABLE #-} FindElem t r => FindElem t (t' ': r) where
  findElem = P $ 1 + unP (findElem :: P t r)
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

-- class Members t (tss :: [* -> *])
-- instance (Member t tss, Members ts tss) => Members (t ': ts) tss
-- instance Members '[] ts

type family Members (ts :: [* -> *]) (tss :: [* -> *]) = (cs :: Constraint) | cs -> ts where
  Members (t ': ts) tss = (Member t tss, Members ts tss)
  Members '[] tss       = ()

inj' :: Int -> t v -> Union r v
inj' = Union

prj' :: Int -> Union r v -> Maybe (t v)
prj' n (Union n' x) | n == n'   = Just (unsafeCoerce x)
                    | otherwise = Nothing

{- We want to handle a request of type t, where we state that t must be at the front of the list of requests (we know that the index is 0). If the request tv is indeed of type t (its index is 0), then we can unsafe coerce the tv to be of type 't v'. Otherwise, we return rv which is a request of a different type, and we can safely remove the request 't' from the front of the union at _this_ level of the free monad.  -}
decomp :: Union (t ': r) v -> Either (Union r v) (t v)
decomp (Union 0 tv) = Right $ unsafeCoerce tv
decomp (Union n rv) = Left  $ Union (n-1) rv

-- Prepend new effect type at front
weaken :: Union ts a -> Union (any ': ts) a
weaken (Union n ta) = Union (n + 1) ta

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

-- | Given request, handle or relay it, and discharge it from the list of effects
handleRelay ::
     (a -> Freer ts b)                                  -- Return handler
  -> (forall x. t x -> (x -> Freer ts b) -> Freer ts b) -- Request handler
  -> Freer (t ': ts) a
  -> Freer ts b
handleRelay ret _ (Pure x) = ret x
handleRelay ret h (Free u k) =
  case decomp u of
    Right x  -> h x (handleRelay ret h . k)
    Left  u' -> Free u' (handleRelay ret h . k)

-- | Parameterised version of handleRelay to allow a state to be managed
handleRelaySt ::
     s                                                            -- State
  -> (s -> a -> Freer ts b)                                       -- Return handler
  -> (forall x. s -> t x -> (s -> x -> Freer ts b) -> Freer ts b) -- Request handler
  -> Freer (t ': ts) a
  -> Freer ts b
handleRelaySt s ret _ (Pure x) = ret s x
handleRelaySt s ret h (Free u k) =
  case decomp u of
    Right tx -> h s tx (\s' x -> handleRelaySt s' ret h $ k x)
    Left u' -> Free u' (handleRelaySt s ret h . k)

-- | Intercept and handle requests from program, but do not discharge effect from type-level
--   Requires type application on usage to specify which effect is being intercepted, e.g. "interpose @(Reader Int)"
interpose :: Member t ts =>
     (a -> Freer ts b)
  -> (forall x. t x -> (x -> Freer ts b) -> Freer ts b)
  -> Freer ts a
  -> Freer ts b
interpose ret h (Pure x ) = ret x
interpose ret h (Free u k) =
  case prj u  of
    Just tx -> h tx (interpose ret h . k)
    Nothing -> Free u (interpose ret h . k)

interposeSt :: Member t ts =>
     s
  -> (s -> a -> Freer ts b)
  -> (forall x. s -> t x -> (s -> x -> Freer ts b) -> Freer ts b)
  -> Freer ts a
  -> Freer ts b
interposeSt s ret h (Pure x ) = ret s x
interposeSt s ret h (Free u k) =
  case prj u  of
    Just tx -> h s tx (\s' x -> interposeSt s' ret h $ k x)
    Nothing -> Free u (interposeSt s ret h . k)

-- | Replace the effect t at the front of the list of effects with a new effect v.
replaceRelay ::
      (a -> Freer (v ': ts) b)
  ->  (forall x. t x -> (x -> Freer (v ': ts) b) -> Freer (v ': ts) b)
  ->  Freer (t ': ts) a
  ->  Freer (v ': ts) b
replaceRelay ret h (Pure x) = ret x
replaceRelay ret h (Free u k) = case decomp u of
  Right tx -> h tx (replaceRelay ret h . k)
  Left  u' -> Free (weaken u') (replaceRelay ret h . k)

replaceRelaySt ::
      s
  ->  (s -> a -> Freer (v ': ts) b)
  ->  (forall x. s -> t x -> (s -> x -> Freer (v ': ts) b) -> Freer (v ': ts) b)
  ->  Freer (t ': ts) a
  ->  Freer (v ': ts) b
replaceRelaySt s ret h (Pure x) = ret s x
replaceRelaySt s ret h (Free u k) = case decomp u of
  Right tx -> h s tx (\s' x -> replaceRelaySt s' ret h $ k x)
  Left  u' -> Free (weaken u') (replaceRelaySt s ret h . k)

data WriterE w a where
  TellE :: w -> WriterE w ()

data ReaderE env a where
  AskE :: ReaderE env env

-- | Get effect t from (t ': ts), leave it unhandled, and install new effect t' after every request of t. This adds t' to the front of (t ': ts).
installFront ::
     (a -> Freer (t' ': t ': ts) a)
  -> (forall x. x -> t x -> (x -> Freer (t' ': t ': ts) a) -> Freer (t' ': t ': ts) a)
  -> Freer (t ': ts) a
  -> Freer (t' ': t ': ts) a
installFront ret h (Pure x )  = ret x
installFront ret h (Free u k) =
  case prj u  of
    Just tx -> Free (weaken u) (\x -> h x tx (installFront ret h . k))
    Nothing -> Free (weaken u) (installFront ret h . k)

runRWFront ::
  Freer (ReaderE Int ': ts) a -> Freer (WriterE [Int] ': ReaderE Int ': ts) a
runRWFront = installFront return
  (\x tx k ->
      case tx of AskE -> do send (TellE [x])
                            k x)

-- | Find some existing effect t in ts, leave it unhandled, and install new effect t' after every request of t. This adds the effect t' to the front of ts.
-- Requires type application on usage to specify which effect is being intercepted, e.g. "installPrepend @(Reader Int)"
installPrepend :: Member t ts =>
     (a -> Freer (t' ': ts) a)
  -> (forall x. x -> t x -> (x -> Freer (t' ': ts) a) -> Freer (t' ': ts) a)
  -> Freer ts a
  -> Freer (t' ': ts) a
installPrepend ret h (Pure x )  = ret x
installPrepend ret h (Free u k) =
  case prj u  of
    Just tx -> Free (weaken u) (\x -> h x tx (installPrepend ret h . k))
    Nothing -> Free (weaken u) (installPrepend ret h . k)

runRWPrepend :: forall ts a . Member (ReaderE Int) ts =>
  Freer ts a -> Freer (WriterE [Int] ': ts) a
runRWPrepend = installPrepend @(ReaderE Int) return
  (\x tx k ->
      case tx of AskE -> do send (TellE [x])
                            k x)

-- | Find some effect t in ts, leave it unhandled, and inject operation for another existing effect t' in ts.
-- Requires type application on usage to specify which effect is being intercepted and which is being inserted, e.g. "installPrepend @(Reader Int) @(Writer [Int])"
install :: forall t t' ts a b. (Member t ts, Member t' ts) =>
     (a -> Freer ts b)
  -> (forall x. x -> t x -> (x -> Freer ts b) -> Freer ts b)
  -> Freer ts a
  -> Freer ts b
install ret h (Pure x )  = ret x
install ret h (Free u k) =
  case prj u  of
    Just tx -> Free u (\x -> h x tx (install @t @t' ret h . k))
    Nothing -> Free u (install @t @t' ret h . k)

runRW :: Members '[ReaderE Int, WriterE [Int]] ts =>
  Freer ts a -> Freer ts a
runRW = install @(ReaderE Int) @(WriterE [Int]) return
  (\x tx k ->
      case tx of AskE -> do send (TellE [x])
                            k x)