{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}

module DMap where

import Type.Reflection
    ( Typeable, type (:~~:)(HRefl), eqTypeRep, typeRep )
import PrimDist

{- TyEq: Test equality between two different types 'a' and 'b' -}
class (Typeable c, Typeable b) => TyEq c b where
  tyEq :: c -> b -> Maybe (c :~~: b)
  eq   :: Eq c => c -> b -> Bool
  eq x y = case eqTypeRep (typeRep @c) (typeRep @b) of
            Just HRefl -> x == y
            Nothing -> False

instance (Typeable a, Typeable b) => TyEq a b where
  tyEq x y = eqTypeRep (typeRep @a) (typeRep @b)

{- TrueOrd:  Compare two different types 'a' and 'b' for equality (using TyEq), then
             compare two different values 'x : a' and 'y : a' for ordering -}
data TrueOrdering a b = TrueEQ (a :~~: b) | TrueNEQ | TrueLT | TrueGT

class HeteroOrd (k :: k' -> *) where
  hCompare :: k a -> k b -> Ordering

instance Ord s => HeteroOrd (Key s) where
  hCompare (Key s1) (Key s2) = compare s1 s2

class (HeteroOrd k, Typeable a, Typeable b) => TrueOrd k a b where
  trueCompare :: k a -> k b -> TrueOrdering a b

instance (HeteroOrd k, Typeable a, Typeable b) => TrueOrd k a b where
  trueCompare a b = case hCompare a b of
    EQ -> case eqTypeRep (typeRep @a) (typeRep @b) of
              Just HRefl -> TrueEQ HRefl
              Nothing    -> TrueNEQ
    LT -> TrueLT
    GT -> TrueGT

{- Dist Key -}
type DKey = Key String

data Key s a where
  Key :: forall s a. (Ord s, Typeable a) => s -> Key s a
  deriving (Typeable)

instance Show s => Show (Key s a) where
  show (Key s) = show s

instance Eq (Key s a) where
  Key a == Key b = a == b

instance Ord (Key s a) where
  compare :: forall k s (a :: k). Key s a -> Key s a -> Ordering
  compare (Key a) (Key b) = compare a b

{- Dist map where we compare values of keys before comparing types of keys, by using HeteroOrd -}

data DMap where
  Leaf :: DMap
  Node :: Typeable v
       => DKey v           -- key
       -> PrimDist v    -- value
       -> DMap         -- left
       -> DMap         -- right
       -> DMap

empty :: DMap
empty = Leaf

singleton :: (Typeable v) => DKey v -> PrimDist v -> DMap
singleton k x = Node k x Leaf Leaf

lookup :: forall a b. (Eq a, Typeable a)
  => DKey a -> DMap  -> Maybe (PrimDist a)
lookup k = go
  where
    go :: DMap -> Maybe (PrimDist a)
    go Leaf = Nothing
    go (Node k' x l r) = case trueCompare k k'
        of TrueEQ HRefl -> if k' == k then Just x else Nothing
           TrueNEQ      -> Nothing
           TrueLT       -> go l
           TrueGT       -> go r

insert :: forall v. (Typeable v) => DKey v -> PrimDist v -> DMap  -> DMap
insert kx x = go
  where
    go :: DMap -> DMap
    go Leaf = singleton kx x
    go (Node ky y l r) =
      case trueCompare kx ky of
        TrueEQ HRefl -> Node kx x l r
        TrueNEQ      -> Node ky y l r
        TrueLT       -> Node ky y (go l) r
        TrueGT       -> Node ky y l (go r)

foldrWithKey :: Typeable k => (forall v. (Typeable v) => DKey v -> PrimDist v -> b -> b) -> b -> DMap  -> b
foldrWithKey f = go
  where
    go z Leaf             = z
    go z (Node  kx x l r) = go (f kx x (go z r)) l

key1 :: DKey Int
key1 = Key "s"

key2 :: DKey Double
key2 = Key "s"

cmp1 :: TrueOrdering Int Double
cmp1 = trueCompare key1 key2

map1 :: DMap
map1 = insert key1 (Deterministic 5) empty

{- Dependent map from random variables to primitive distributions -}

-- exampleDistMap :: DistMap
exampleDistMap :: DMap
exampleDistMap = insert (Key "s" :: DKey Double) (Normal 0 5) empty