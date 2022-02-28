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
import Extensible.Member

data OpenSum (ts :: [k]) where
  UnsafeOpenSum :: Int -> t -> OpenSum ts

instance Eq (OpenSum '[]) where
  x == _ = case x of {}

testOpenSum :: Bool
testOpenSum =
  let os1 = inj (5 :: Int) :: OpenSum '[Int, Bool]
      os2 = inj (True :: Bool) :: OpenSum '[Int, Bool]
  in  os1 == os2

instance forall t ts. (Eq t, Eq (OpenSum ts)) => Eq (OpenSum (t ': ts)) where
  -- If their types aren't the same, return false
  UnsafeOpenSum i _ == UnsafeOpenSum j _ | i /= j = False
  -- If their types are both t, then unsafe coerce both and compare
  UnsafeOpenSum 0 x == UnsafeOpenSum 0 y =
    unsafeCoerce x == (unsafeCoerce y :: t)
  -- If their types are still to be determined (in ts), then compare the same values but with the type OpenSum ts
  UnsafeOpenSum i x == UnsafeOpenSum j y =
    UnsafeOpenSum (i - 1) x == (UnsafeOpenSum (j - 1) y :: OpenSum ts)

instance forall t ts. (Show t, Show (OpenSum ts)) => Show (OpenSum (t ': ts)) where
  show (UnsafeOpenSum i t) =
    if i == 0
    then show (unsafeCoerce t :: t)
    else show (UnsafeOpenSum (i - 1) t :: OpenSum ts)

instance {-# OVERLAPPING #-} Show t => Show (OpenSum '[t]) where
  show (UnsafeOpenSum i t) = show (unsafeCoerce t :: t)

class (FindElem t ts) => Member (t :: *) (ts :: [*]) where
  inj ::  t -> OpenSum ts
  prj ::  OpenSum ts  -> Maybe t

instance (Typeable t, t ~ t') => Member t '[t'] where
   inj x          = UnsafeOpenSum 0 x
   prj (UnsafeOpenSum _ x) = Just (unsafeCoerce x)

instance (FindElem t ts) => Member t ts where
  inj = inj' (unP (findElem :: P t ts))
  prj = prj' (unP (findElem :: P t ts))

-- | Not possible to implement "Members" as a type class.
-- class Members t (tss :: [* -> *])
-- instance (Member t tss, Members ts tss) => Members (t ': ts) tss
-- instance Members '[] ts

type family Members (ts :: [* ]) (tss :: [* ]) = (cs :: Constraint) | cs -> ts where
  Members (t ': ts) tss = (Member t tss, Members ts tss)
  Members '[] tss       = ()

inj' :: Int -> t  -> OpenSum ts
inj' = UnsafeOpenSum

prj' :: Int -> OpenSum ts  -> Maybe t
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
-- type FindElem (key :: k) (ts :: [k]) =
--   FindIndex (TyEq key) ts >>= FromMaybe Stuck

-- type Member t ts = KnownNat (Eval (FindElem t ts))

-- findElem :: forall t ts. Member t ts => Int
-- findElem = fromIntegral $ natVal (Proxy @(Eval (FindElem t ts)))

-- inj :: forall  t ts. Member t ts => t -> OpenSum ts
-- inj ft = UnsafeOpenSum (findElem @t @ts) ft

-- prj :: forall  t ts. Member t ts => OpenSum ts -> Maybe t
-- prj (UnsafeOpenSum i ft) =
--   if i == findElem @t @ts
--   then Just $ unsafeCoerce ft
--   else Nothing