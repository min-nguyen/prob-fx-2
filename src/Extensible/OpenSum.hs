{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts, FlexibleInstances, GADTs, PolyKinds, RankNTypes, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances, EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Extensible.OpenSum where

import Data.Typeable
import Data.Kind (Type, Constraint)
import Data.Proxy
-- import Fcf
import GHC.Natural
import GHC.TypeLits (Nat, KnownNat, natVal, TypeError, ErrorMessage (Text, (:$$:), (:<>:), ShowType))
import qualified GHC.TypeLits as TL
import Unsafe.Coerce
import Extensible.FindElem

data OpenSum (as :: [k]) where
  UnsafeOpenSum :: Int -> a -> OpenSum as

instance Eq (OpenSum '[]) where
  x == _ = case x of {}

testOpenSum :: Bool
testOpenSum =
  let os1 = inj (5 :: Int) :: OpenSum '[Int, Bool]
      os2 = inj (True :: Bool) :: OpenSum '[Int, Bool]
  in  os1 == os2

instance forall a as. (Eq a, Eq (OpenSum as)) => Eq (OpenSum (a ': as)) where
  -- If their types aren'a the same, return false
  UnsafeOpenSum i _ == UnsafeOpenSum j _ | i /= j = False
  -- If their types are both a, then unsafe coerce both and compare
  UnsafeOpenSum 0 x == UnsafeOpenSum 0 y =
    unsafeCoerce x == (unsafeCoerce y :: a)
  -- If their types are still to be determined (in as), then compare the same values but with the type OpenSum as
  UnsafeOpenSum i x == UnsafeOpenSum j y =
    UnsafeOpenSum (i - 1) x == (UnsafeOpenSum (j - 1) y :: OpenSum as)

instance forall a as. (Show a, Show (OpenSum as)) => Show (OpenSum (a ': as)) where
  show (UnsafeOpenSum i a) =
    if i == 0
    then show (unsafeCoerce a :: a)
    else show (UnsafeOpenSum (i - 1) a :: OpenSum as)

instance {-# OVERLAPPING #-} Show a => Show (OpenSum '[a]) where
  show (UnsafeOpenSum i a) = show (unsafeCoerce a :: a)

class (FindElem a as) => Member (a :: *) (as :: [*]) where
  inj ::  a -> OpenSum as
  prj ::  OpenSum as  -> Maybe a

instance (Typeable a, a ~ a') => Member a '[a'] where
   inj x          = UnsafeOpenSum 0 x
   prj (UnsafeOpenSum _ x) = Just (unsafeCoerce x)

instance (FindElem a as) => Member a as where
  inj = inj' (unP (findElem :: P a as))
  prj = prj' (unP (findElem :: P a as))

-- | Not possible to implement "Members" as a type class.
-- class Members a (tss :: [* -> *])
-- instance (Member a tss, Members as tss) => Members (a ': as) tss
-- instance Members '[] as

type family Members (as :: [* ]) (tss :: [* ]) = (cs :: Constraint) | cs -> as where
  Members (a ': as) tss = (Member a tss, Members as tss)
  Members '[] tss       = ()

inj' :: Int -> a  -> OpenSum as
inj' = UnsafeOpenSum

prj' :: Int -> OpenSum as  -> Maybe a
prj' n (UnsafeOpenSum n' x) | n == n'   = Just (unsafeCoerce x)
                    | otherwise = Nothing

-- type Exp a = a -> Type
-- type family Eval (e :: Exp a) :: a
-- -- ## Exp as type-level monad
-- data Pure :: a -> Exp a
-- data (>>=) :: Exp a -> (a -> Exp b) -> Exp b
-- data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
-- infixl 0 >>=
-- infixl 1 <=<
-- type instance Eval (Pure a) = a
-- type instance Eval (mx >>= mf) = Eval (mf (Eval mx))
-- type instance Eval ((mf <=< mg) a) = Eval (mf (Eval (mg a)))
-- -- ## A type family for equality between types.
-- type family TyEqImpl (a :: k) (b :: k) :: Bool where
--   TyEqImpl a a = 'True
--   TyEqImpl a b = 'False
-- -- A defunctionalised label for type equality
-- data TyEq :: a -> b -> Exp Bool
-- -- Evaluating TyEq a b is equivalent to the TyEqImpl a b
-- type instance Eval (TyEq a b) = TyEqImpl a b
-- -- ## FMap
-- data FMap :: (a -> Exp b) -> f a -> Exp (f b)
-- type instance Eval (FMap f '[]) = '[]
-- type instance Eval (FMap f (x ': xs)) = Eval (f x) ': Eval (FMap f xs)
-- type instance Eval (FMap f 'Nothing)  = 'Nothing
-- type instance Eval (FMap f ('Just a)) = 'Just (Eval (f a))
-- -- ## Addition
-- data (+) :: Nat -> Nat -> Exp Nat
-- type instance Eval ((+) a b) = a TL.+ b
-- -- ## If
-- data If :: Bool -> a -> a -> Exp a
-- type instance Eval (If 'True a1 a2) = a1
-- type instance Eval (If 'False a1 a2) = a2
-- -- ## FromMaybe
-- data FromMaybe :: a -> Maybe a -> Exp a
-- type instance Eval (FromMaybe a' ('Just a)) = a
-- type instance Eval (FromMaybe a 'Nothing)   = a
-- -- ## Stuck - equivalent to a type-level undefined
-- type family Stuck :: a
-- -- ## FindIndex
-- data FindIndex :: (a -> Exp Bool) -> [a] -> Exp (Maybe Nat)
-- type instance Eval (FindIndex f '[]) = 'Nothing
-- type instance Eval (FindIndex f (x ': xs))
--   = Eval (If (Eval (f x))
--              (Eval (Pure ('Just 0)))
--              (Eval (FindIndex f xs >>= FMap ((+) 1))))
-- -- ## FindElem
-- type FindElem (key :: k) (as :: [k]) =
--   FindIndex (TyEq key) as >>= FromMaybe Stuck

-- type Member a as = KnownNat (Eval (FindElem a as))

-- findElem :: forall a as. Member a as => Int
-- findElem = fromIntegral $ natVal (Proxy @(Eval (FindElem a as)))

-- inj :: forall  a as. Member a as => a -> OpenSum as
-- inj ft = UnsafeOpenSum (findElem @a @as) ft

-- prj :: forall  a as. Member a as => OpenSum as -> Maybe a
-- prj (UnsafeOpenSum i ft) =
--   if i == findElem @a @as
--   then Just $ unsafeCoerce ft
--   else Nothing