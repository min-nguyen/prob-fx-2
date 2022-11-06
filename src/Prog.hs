{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}

{- | An encoding for algebraic effects, based on the @freer@ monad.
-}

module Prog (
  -- * Effectful program
    Prog(..)
  , EffectSum
  , Member(..)
  , Members
  , UniqueMember
  , LastMember
  , Append
  -- * Auxiliary functions
  , run
  , call
  , discharge
  , discharge1
  , weaken
  , weakenProg
  , install) where

import Control.Monad ( (>=>) )
import Data.Kind (Constraint)
import TyCompare ( Idx(unIdx), FindElem(..) )
import GHC.TypeLits ( TypeError, ErrorMessage(Text, (:<>:), (:$$:), ShowType) )
import Unsafe.Coerce ( unsafeCoerce )

{- | A program that returns a value of type @a@ and can call operations that belong to some effect
     @e@ in signature @es@; this represents a syntax tree whose nodes are operations and leaves are pure values.
-}
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
  return           = Val
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
   inj = EffectSum 0
   prj (EffectSum _ x) = Just (unsafeCoerce x)

instance (FindElem e es) => Member e es where
  inj = EffectSum (unIdx (findElem :: Idx e es))
  prj = prj' (unIdx (findElem :: Idx e es))
    where prj' n (EffectSum n' x)
            | n == n'   = Just (unsafeCoerce x)
            | otherwise = Nothing

-- | Specifies that @es@ is a subset of @ess@
type family Members (es :: [* -> *]) (ess :: [* -> *]) = (cs :: Constraint) | cs -> es where
  Members (e ': es) ess = (Member e ess, Members es ess)
  Members '[] ess       = ()

-- | Specifies that @e@ is a unique effect in @es@
class    (UMember 'False e es ~ True, Member e es) => UniqueMember e es
instance (UMember 'False e es ~ True, Member e es) => UniqueMember e es

type Append :: forall a. [a] -> [a] -> [a]  -- kind signature
type family Append xs ys where              -- header
  Append '[]    ys = ys                     -- clause 1
  Append (x:xs) ys = x : Append xs ys

type family UMember (b :: Bool) (e :: * -> *) (es :: [* -> *]) :: Bool where
  UMember 'True e (e ': es)   = 'False
  UMember 'True e (e' ': es)  = UMember 'True e es
  UMember 'True e '[]         = 'True
  UMember 'False e (e ': es)  = UMember 'True e es
  UMember 'False e (e' ': es) = UMember 'False e es
  UMember 'False e '[]        = 'False

-- | Specifies that @e@ is the last effect in @es@
class Member e es => LastMember e es | es -> e
instance {-# OVERLAPPABLE #-} LastMember e es => LastMember e (e' ': es)
instance LastMember e (e ': '[])

-- | Run a pure computation
run :: Prog '[] a -> a
run (Val x) = x
run _ = error "Prog.run isn't defined for non-pure computations"

-- | Call an operation in a computation
call :: Member e es => e x -> Prog es x
call e = Op (inj e) Val

-- | Discharge an effect from the front of an effect sum
discharge :: EffectSum (e ': es) x -> Either (EffectSum es x) (e x)
discharge (EffectSum 0 tv) = Right $ unsafeCoerce tv
discharge (EffectSum n rv) = Left  $ EffectSum (n-1) rv

-- | Discharge the only effect from an effect sum
discharge1 :: EffectSum '[e] x -> e x
discharge1 (EffectSum 0 tv) = unsafeCoerce tv
discharge1 _ = error "Prog.discharge1: impossible"

-- | Prepend an effect to the front of an effect sum
weaken :: EffectSum es a -> EffectSum (e ': es) a
weaken (EffectSum n ta) = EffectSum (n + 1) ta

-- | Prepend an effect to the front of a computation's effect signature
weakenProg :: forall e es a. Prog es a -> Prog (e : es) a
weakenProg (Val x)   = Val x
weakenProg (Op op k) = Op (weaken op) (weakenProg . k)

{- | Find some existing effect @e@ in @es@, leave it unhandled, and call a new
     effect @e'@ after every request of @e@. This prepends @e'@ to @es@, and requires
     type application on usage to specify which effect is being intercepted.
-}
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

