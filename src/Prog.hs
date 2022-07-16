{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

{- | An encoding for algebraic effects, based on the @freer@ monad.
-}

module Prog (
  -- * Effectful program
    Prog(..)
  , EffectSum
  , Member(..)
  , UniqueMember
  , LastMember
  -- * Auxiliary functions
  , run
  , call
  , discharge
  , weaken
  , install) where

import Control.Monad ( (>=>) )
import Data.Kind (Constraint)
import FindElem ( Idx(unIdx), FindElem(..) )
import GHC.TypeLits ( TypeError, ErrorMessage(Text, (:<>:), (:$$:), ShowType) )
import Unsafe.Coerce ( unsafeCoerce )

-- | A program that returns a value of type @a@ and can call operations that belong to some effect @e@ in signature @es@; this represents a syntax tree whose nodes are operations and leaves are pure values.
data Prog es a where
  Val
    :: a                -- ^ pure value
    -> Prog es a
  Op
    :: EffectSum es x   -- ^ an operation belonging to some effect in @es@
    -> (x -> Prog es a) -- ^ a continuation from the result of the operation
    -> Prog es a

instance Functor (Prog es) where
  fmap f (Val a) = Val (f a)
  fmap f (Op fx k) = Op fx (fmap f . k)

instance Applicative (Prog es) where
  pure = Val
  Val f <*> x = fmap f x
  (Op fx k) <*> x = Op fx ((<*> x) . k)

instance Monad (Prog es) where
  return            = Val
  Val a >>= f      = f a
  Op fx k >>= f = Op fx (k >=> f)

-- | An open sum for an effect signature @es@, containing an operation @e x@ where @e@ is in @es@
data EffectSum (es :: [* -> *]) (x :: *) :: * where
  EffectSum :: Int -> e x -> EffectSum es x

-- | Membership of an effect @e@ in @es@
class (FindElem e es) => Member (e :: * -> *) (es :: [* -> *]) where
  -- | Inject an operation of type @e x@ into an effect sum
  inj ::  e x -> EffectSum es x
  -- | Attempt to project an operation of type @e x@ out from an effect sum
  prj ::  EffectSum es x -> Maybe (e x)

instance {-# INCOHERENT #-} (e ~ e') => Member e '[e'] where
   inj x  = EffectSum 0 x
   prj (EffectSum _ x) = Just (unsafeCoerce x)

instance (FindElem e es) => Member e es where
  inj = EffectSum (unIdx (findElem :: Idx e es))
  prj = prj' (unIdx (findElem :: Idx e es))
    where prj' n (EffectSum n' x)
            | n == n'   = Just (unsafeCoerce x)
            | otherwise = Nothing

-- | Membership of many effects @es@ in @ess@
type family Members (es :: [* -> *]) (tss :: [* -> *]) = (cs :: Constraint) | cs -> es where
  Members (e ': es) tss = (Member e tss, Members es tss)
  Members '[] tss       = ()

-- | Unique effect
class    (UMember 'False e es ~ True, Member e es) => UniqueMember e es
instance (UMember 'False e es ~ True, Member e es) => UniqueMember e es

type family UMember (b :: Bool) (e :: * -> *) (es :: [* -> *]) :: Bool where
  UMember 'True e (e ': es)   = 'False
  UMember 'True e (e' ': es)  = UMember 'True e es
  UMember 'True e '[]         = 'True
  UMember 'False e (e ': es)  = UMember 'True e es
  UMember 'False e (e' ': es) = UMember 'False e es
  UMember 'False e '[]        = 'False

-- | Last effect
class Member m effs => LastMember m effs | effs -> m
instance {-# OVERLAPPABLE #-} LastMember m effs => LastMember m (eff ': effs)
instance LastMember m (m ': '[])

-- | Run a pure computation
run :: Prog '[] a -> a
run (Val x) = x
run _ = error "'run' isn't defined for non-pure computations"

-- | Call an operation of type @e x@ in a computation
call :: (Member e es) => e x -> Prog es x
call e = Op (inj e) Val

-- | Discharges an effect @e@ from the front of an effect signature @es@
discharge :: EffectSum (e ': es) x -> Either (EffectSum es x) (e x)
discharge (EffectSum 0 tv) = Right $ unsafeCoerce tv
discharge (EffectSum n rv) = Left  $ EffectSum (n-1) rv

-- | Prepend an effect @e@ to the front of an effect signature @es@
weaken :: EffectSum es a -> EffectSum (e ': es) a
weaken (EffectSum n ta) = EffectSum (n + 1) ta

-- | Find some existing effect e in es, leave it unhandled, and install new effect e' after every request of e. This adds the effect e' to the front of es. Requires type application on usage to specify which effect is being intercepted, e.g. "installPrepend @(Reader Int)"
install :: Member e es =>
     (a -> Prog (e' ': es) a)
  -> (forall x. x -> e x -> (x -> Prog (e' ': es) a) -> Prog (e' ': es) a)
  -> Prog es a
  -> Prog (e' ': es) a
install ret h (Val x )  = ret x
install ret h (Op u k) =
  case prj u  of
    Just tx -> Op (weaken u) (\x -> h x tx (install ret h . k))
    Nothing -> Op (weaken u) (install ret h . k)

-- | For pattern-matching against operations that belong in the tail of an effect signature
pattern Other :: EffectSum es x -> EffectSum  (e ': es) x
pattern Other u <- (discharge -> Left u)

