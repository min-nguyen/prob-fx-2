{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Auxiliary definitions for finding a type in a type-level list.
-}

module TyCompare (
    FindElem(..)
  , Idx(..)
  , TyEq(..)
  , HeteroOrd(..)
  , TrueOrdering(..)) where

import Type.Reflection
    ( Typeable, type (:~~:)(HRefl), eqTypeRep, typeRep )
import GHC.TypeLits ( TypeError, ErrorMessage(Text, (:<>:), (:$$:), ShowType) )

{- | Proof that @x@ is an element of the type-level list @xs@
-}
class FindElem x xs where
  findElem :: Idx x xs

-- | The integer index of @x@ in @xs@
newtype Idx x xs = Idx {unIdx :: Int}

instance FindElem x (x ': xs) where
  findElem = Idx 0

instance {-# OVERLAPPABLE #-} FindElem x xs => FindElem x (x' : xs) where
  findElem = Idx $ 1 + unIdx (findElem :: Idx x xs)

instance TypeError ('Text "Cannot unify effect types." ':$$:
                    'Text "Unhandled effect: " ':<>: 'ShowType x ':$$:
                    'Text "Perhaps check the type of effectful computation and the sequence of handlers for concordance?")
  => FindElem x '[] where
  findElem = error "unreachable"

{- | TyEq: Test equality between two different types 'a' and 'b'.
-}
class (Typeable c, Typeable b) => TyEq c b where
  tyEq :: c -> b -> Maybe (c :~~: b)
  eq   :: Eq c => c -> b -> Bool
  eq x y = case eqTypeRep (typeRep @c) (typeRep @b) of
            Just HRefl -> x == y
            Nothing -> False

instance (Typeable a, Typeable b) => TyEq a b where
  tyEq x y = eqTypeRep (typeRep @a) (typeRep @b)

{- TrueOrd:  Compare two different types 'a' and 'b' for equality (using TyEq), then
             compare two different values 'x : a' and 'y : a' for ordering
-}
data TrueOrdering a b = TrueEQ (a :~~: b) | TrueNEQ | TrueLT | TrueGT

class HeteroOrd (k :: k' -> *) where
  hCompare :: k a -> k b -> Ordering
