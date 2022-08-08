{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}

{- | An open sum implementation for value types.
-}
module OpenSum (
    OpenSum(..)
  , Member(..)) where

import Data.Kind (Type, Constraint)
import Data.Typeable ( Typeable )
import FindElem ( Idx(..), FindElem(..) )
import GHC.TypeLits (Nat, KnownNat, natVal, TypeError, ErrorMessage (Text, (:$$:), (:<>:), ShowType))
import qualified GHC.TypeLits as TL
import Unsafe.Coerce ( unsafeCoerce )

-- | Open sum of value types @as@
data OpenSum (as :: [*]) where
  UnsafeOpenSum :: Int -> a -> OpenSum as

instance Eq (OpenSum '[]) where
  x == _ = case x of {}

instance forall a as. (Eq a, Eq (OpenSum as)) => Eq (OpenSum (a : as)) where
  UnsafeOpenSum i _ == UnsafeOpenSum j _ | i /= j = False
  UnsafeOpenSum 0 x == UnsafeOpenSum 0 y =
    unsafeCoerce x == (unsafeCoerce y :: a)
  UnsafeOpenSum i x == UnsafeOpenSum j y =
    UnsafeOpenSum (i - 1) x == (UnsafeOpenSum (j - 1) y :: OpenSum as)

instance forall a as. (Show a, Show (OpenSum as)) => Show (OpenSum (a : as)) where
  show (UnsafeOpenSum i a)    =
    if i == 0
    then show (unsafeCoerce a :: a)
    else show (UnsafeOpenSum (i - 1) a :: OpenSum as)

instance {-# OVERLAPPING #-} Show a => Show (OpenSum '[a]) where
  show (UnsafeOpenSum i a) = show (unsafeCoerce a :: a)

-- | Membership of type @a@ in a type-level list @as@
class (FindElem a as) => Member (a :: *) (as :: [*]) where
  -- | Inject a value of type @A@ into an open sum
  inj ::  a -> OpenSum as
  -- | Attempt to project an open sum to a value of type @a@
  prj ::  OpenSum as  -> Maybe a

instance (Typeable a, a ~ a') => Member a '[a'] where
   inj = UnsafeOpenSum 0
   prj (UnsafeOpenSum _ x) = Just (unsafeCoerce x)

instance (FindElem a as) => Member a as where
  inj = UnsafeOpenSum (unIdx (findElem :: Idx a as))
  prj = prj' (unIdx (findElem :: Idx a as))
    where prj' n (UnsafeOpenSum n' x)
            | n == n'   = Just (unsafeCoerce x)
            | otherwise = Nothing
