{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts, FlexibleInstances, GADTs, PolyKinds, RankNTypes, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Extensible.OpenSum where

import Data.Kind (Type)
import Data.Proxy
-- import Fcf
import GHC.Natural
import GHC.TypeLits (Nat, KnownNat, natVal)
import qualified GHC.TypeLits as TL
import Unsafe.Coerce


data OpenSum (ts :: [k]) where
  UnsafeOpenSum :: Int -> t -> OpenSum ts

instance {-# INCOHERENT #-} Show t => Show (OpenSum '[t]) where
  show (UnsafeOpenSum i t) = show (unsafeCoerce t :: t)

instance forall t ts. (Show t, Show (OpenSum ts)) => Show (OpenSum (t ': ts)) where
  show (UnsafeOpenSum i t) =
    if i == 0
    then show (unsafeCoerce t :: t)
    else show (UnsafeOpenSum (i - 1) t :: OpenSum ts)

type Exp a = a -> Type
type family Eval (e :: Exp a) :: a

-- ## Exp as type-level monad
data Pure :: a -> Exp a
data (>>=) :: Exp a -> (a -> Exp b) -> Exp b
data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
infixl 0 >>=
infixl 1 <=<
type instance Eval (Pure a) = a
type instance Eval (mx >>= mf) = Eval (mf (Eval mx))
type instance Eval ((mf <=< mg) a) = Eval (mf (Eval (mg a)))

-- ## A type family for equality between types.
type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False
-- A defunctionalised label for type equality
data TyEq :: a -> b -> Exp Bool
-- Evaluating TyEq a b is equivalent to the TyEqImpl a b
type instance Eval (TyEq a b) = TyEqImpl a b
-- ## FMap
data FMap :: (a -> Exp b) -> f a -> Exp (f b)
type instance Eval (FMap f '[]) = '[]
type instance Eval (FMap f (x ': xs)) = Eval (f x) ': Eval (FMap f xs)
type instance Eval (FMap f 'Nothing)  = 'Nothing
type instance Eval (FMap f ('Just a)) = 'Just (Eval (f a))
-- ## Addition
data (+) :: Nat -> Nat -> Exp Nat
type instance Eval ((+) a b) = a TL.+ b
-- ## If
data If :: Bool -> a -> a -> Exp a
type instance Eval (If 'True a1 a2) = a1
type instance Eval (If 'False a1 a2) = a2
-- ## FromMaybe
data FromMaybe :: a -> Maybe a -> Exp a
type instance Eval (FromMaybe a' ('Just a)) = a
type instance Eval (FromMaybe a 'Nothing)   = a
-- ## Stuck - equivalent to a type-level undefined
type family Stuck :: a
-- ## FindIndex
data FindIndex :: (a -> Exp Bool) -> [a] -> Exp (Maybe Nat)
type instance Eval (FindIndex f '[]) = 'Nothing
type instance Eval (FindIndex f (x ': xs))
  = Eval (If (Eval (f x))
             (Eval (Pure ('Just 0)))
             (Eval (FindIndex f xs >>= FMap ((+) 1))))

-- ## FindElem
type FindElem (key :: k) (ts :: [k]) =
  FindIndex (TyEq key) ts >>= FromMaybe Stuck

type Member t ts = KnownNat (Eval (FindElem t ts))

findElem :: forall t ts. Member t ts => Int
findElem = fromIntegral $ natVal (Proxy @(Eval (FindElem t ts)))

inj :: forall  t ts. Member t ts => t -> OpenSum ts
inj ft = UnsafeOpenSum (findElem @t @ts) ft

inj' :: forall  t ts. Member t ts => t -> OpenSum ts
inj' ft = UnsafeOpenSum (findElem @t @ts) ft


prj :: forall  t ts. Member t ts => OpenSum ts -> Maybe t
prj (UnsafeOpenSum i ft) =
  if i == findElem @t @ts
  then Just $ unsafeCoerce ft
  else Nothing