{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

{- | Auxiliary definitions for finding a type in a type-level list.
-}

module TyCompare (
    FindElem(..)
  , Idx(..)
  , HeteroOrd(..)
  , TrueOrdering(..)
  , TrueOrd(..)
  , asProxy) where

import           Data.Typeable ( Typeable, Proxy(..), type (:~~:) )
import           GHC.TypeLits ( TypeError, ErrorMessage(Text, (:<>:), (:$$:), ShowType) )
import           Data.Proxy ( Proxy(..) )

{- | Proof that @x@ is an element of the type-level list @xs@.
-}
class FindElem x xs where
  findElem :: Idx x xs

-- | The integer index of @x@ in @xs@
newtype Idx x xs = Idx {unIdx :: Int}

instance FindElem x (x ': xs) where
  findElem = Idx 0

-- | Note: This is INCOHERENT rather than OVERLAPPABLE in order to support the `Model.liftHandler` function
instance {-# INCOHERENT #-} FindElem x xs => FindElem x (x' : xs) where
  findElem = Idx $ 1 + unIdx (findElem :: Idx x xs)

instance TypeError ('Text "Cannot unify effect types." ':$$:
                    'Text "Unhandled effect: " ':<>: 'ShowType x ':$$:
                    'Text "Perhaps check the type of effectful computation and the sequence of handlers for concordance?")
  => FindElem x '[] where
  findElem = error "unreachable"

{- TrueOrd:  Compare two different types 'a' and 'b' for equality (using TyEq), then
             compare two different values 'x : a' and 'y : a' for ordering
-}
data TrueOrdering a b = TrueEQ (a :~~: b) | TrueLT | TrueGT

class HeteroOrd (k :: k' -> *) where
  hCompare :: k a -> k b -> Ordering

class (HeteroOrd k, Typeable a, Typeable b) => TrueOrd k a b where
  trueCompare :: k a -> k b -> TrueOrdering a b

asProxy :: forall a. a -> Proxy a
asProxy _ = Proxy @a

