
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE FlexibleContexts #-}


{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{- | For recording samples and log-probabilities during model execution.
-}

module DepMap (
  -- * Addr
    Tag
  , Addr(..)
  -- * Sample trace
  , Trace(..)
  , filterTrace
  -- * Value trace
  , ValueTrace
  -- * Log-probability trace
  , LPTrace
  -- , traceLogProbs
  -- * Gradient trace
  , ΔGuides
  -- * MulDist trace
  , Guides
  , Key(..)
  , keys
  , Some(..)
  , empty
  , insert
  , lookup
  , lookupBy
  , lookupWithDefault
  , lookupOrInsert
  , map
  -- , dlookupOrInsert
  , intersectLeftWith
  -- , dmap
  ) where

import           Data.Map (Map)
import           Data.Maybe ( fromJust, fromMaybe )
import           Data.Proxy ( Proxy(..) )
import           GHC.TypeLits ( KnownSymbol )
import           LogP ( LogP )
import           Comp ( Member, Comp(..), weaken, install )
import           Dist
import           Util
import           TyCompare
import           Vec (Vec)
import           Type.Reflection
import           Data.Kind (Constraint)
import           Prelude hiding (lookup, map)
import Control.Applicative ((<|>))
import Unsafe.Coerce


{- $Address
   Identifiers for probabilistic operations.
-}

-- | A compile-time identifier: observable variable name assigned to a primitive distribution
type Tag  = String
-- | A run-time identifier: global run-time occurrence + observable variable name + occurrence with respect to that variable name
data Addr = Addr { tag :: Tag, local :: Int }
  deriving (Eq, Ord, Show)

instance {-# OVERLAPPING #-} Show (String, Int) where
  show :: (String, Int) -> String
  show (x, n) = "⟨" ++ x ++ "," ++ show n ++ "⟩"

-- | Retrieve the values for the specified observable variable names
filterTrace ::
  -- | observable variable names
    [Tag]
  -- | trace
  -> Map Addr a
  -- | filtered trace
  -> Map Addr a
filterTrace tags = filterByKey (\(Addr tag _)  -> tag `elem` tags)

{- | The type of inverse sample traces, mapping addresses of Sample operations
     to the random values between 0 and 1 passed to their inverse CDF functions.
-}
type Trace = Map Addr Double

{- | The type of value traces, mapping addresses of Sample operations to their actual values
-}
type ValueTrace = Map Addr (OpenSum [Double, Int])

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
  show (UnsafeOpenSum i a)
    | i == 0    = show (unsafeCoerce a :: a)
    | otherwise = show (UnsafeOpenSum (i - 1) a :: OpenSum as)

instance {-# OVERLAPPING #-} Show a => Show (OpenSum '[a]) where
  show (UnsafeOpenSum i a) = show (unsafeCoerce a :: a)

class (FindElem a as) => VMember (a :: *) (as :: [*]) where
  inj ::  a -> OpenSum as
  prj ::  OpenSum as  -> Maybe a

instance (Typeable a, a ~ a') => VMember a '[a'] where
   inj = UnsafeOpenSum 0
   prj (UnsafeOpenSum _ x) = Just (unsafeCoerce x)

instance (FindElem a as) => VMember a as where
  inj = UnsafeOpenSum (unIdx (findElem :: Idx a as))
  prj = prj' (unIdx (findElem :: Idx a as))
    where prj' n (UnsafeOpenSum n' x)
            | n == n'   = Just (unsafeCoerce x)
            | otherwise = Nothing

{- | The type of log-probability traces, mapping addresses of sample/observe operations
     to their log probabilities.
-}
type LPTrace = Map Addr LogP

{- | Dependent map. -}
data DepMap c where
  Leaf  :: DepMap c
  Node  :: (DiffDistribution d)
        => Key d        -- key
        -> Assoc c d    -- distribution
        -> DepMap c  -- left
        -> DepMap c  -- right
        -> DepMap c

-- | A workaround for passing type families/functional dependencies as type parameters
class Id d
class Grad d
type family Assoc (c :: * -> Constraint) (a :: *) = (b :: *) where
  Assoc Id d   = d
  Assoc Grad d = Vec (Arity d) Double

-- | The type of differentiable distribution traces
type Guides = DepMap Id

-- | The type of gradient traces
type ΔGuides = DepMap Grad

instance {-# OVERLAPPING #-} Show [Guides] where
  show (x:xs) = show x ++ "\n" ++ show xs
  show []     = ""

instance Show Guides where
  show :: Guides -> String
  show Leaf = ""
  show (Node (Key var) d l r) = "(" ++ show var ++ ", " ++ show d ++ ") "
                                 ++ show l
                                 ++ show r
    where showNewline Leaf  = ""
          showNewline node  = "\n" ++ show node

instance Show ΔGuides where
  show :: ΔGuides -> String
  show Leaf = ""
  show (Node (Key var) d l r) = "(" ++ show var ++ ", " ++ show d ++ ") "
                                 ++ show l
                                 ++ show r
    where showNewline Leaf  = ""
          showNewline node  = "\n" ++ show node

{- Keys -}
data Key a where
  Key :: forall a. (Typeable a) => Addr -> Key a
  deriving (Typeable)

instance Show (Key a) where
  show :: Key a -> String
  show (Key s) = show s

instance Eq (Key  a) where
  (==) :: Key a -> Key a -> Bool
  Key a == Key b = a == b

instance Ord (Key a) where
  compare :: Key a -> Key a -> Ordering
  compare (Key s1) (Key s2) = compare s1 s2

instance HeteroOrd Key where
  hCompare :: Key a -> Key b -> Ordering
  hCompare (Key s1) (Key s2) = compare s1 s2

instance (HeteroOrd k, Typeable a, Typeable b) => TrueOrd k (a :: *) b where
  trueCompare :: k a -> k b -> TrueOrdering a b
  trueCompare ka kb = case (hCompare ka kb, compare (show (typeRep @a)) (show (typeRep @b))) of
    (LT, _)  -> TrueLT
    (GT, _)  -> TrueGT
    (EQ, EQ) -> case eqTypeRep (typeRep @a) (typeRep @b) of
                  Just HRefl -> TrueEQ HRefl
                  Nothing    -> error "Should not happen."
    (EQ, LT) -> TrueLT
    (EQ, GT) -> TrueGT

data Some c k where
  Some :: c d => k d -> Some c k

-- | Fetch the keys
keys :: DepMap c -> [Some DiffDistribution Key]
keys = go
  where
    go :: DepMap c -> [Some DiffDistribution Key]
    go Leaf = []
    go (Node ky y l r) = Some ky : go l ++ go r

-- | Construct an empty trace
empty :: DepMap c
empty = Leaf

-- | Construct a single node
singleton :: (DiffDistribution a) => Key a -> Assoc c a -> DepMap c
singleton k x = Node k x Leaf Leaf

-- | Lookup an entry
lookup :: Typeable a => Key a -> DepMap c -> Maybe (Assoc c a)
lookup kx = go where
  go Leaf = Nothing
  go (Node ky y l r) = case trueCompare kx ky
      of TrueEQ HRefl -> Just y
         TrueLT       -> go l
         TrueGT       -> go r

-- | Lookup an entry
lookupWithDefault :: Typeable a => Assoc c a -> Key a -> DepMap c -> Assoc c a
lookupWithDefault q k = fromMaybe q . lookup k

-- | Lookup an entry
lookupOrInsert :: DiffDistribution a => Key a -> Assoc c a -> DepMap c -> (Assoc c a, DepMap c)
lookupOrInsert kx q m = go m where
  go Leaf = (q, insert kx q m)
  go (Node ky q' l r) = case trueCompare kx ky
      of TrueEQ HRefl -> (q', m)
         TrueLT       -> go l
         TrueGT       -> go r

-- | Lookup an entry
lookupBy :: forall a c. Typeable a => (Addr -> Bool) -> DepMap c -> Maybe (Assoc c a)
lookupBy f = go where
  go Leaf = Nothing
  go (Node ((Key ky) :: Key b) y l r)
    | Just HRefl <- eqTypeRep (typeRep @a) (typeRep @b),
        True <- f ky = Just y
    | otherwise = go l <|> go r

-- | Insert a new entry
insert :: DiffDistribution a => Key a -> Assoc c a -> DepMap c -> DepMap c
insert kx d = go where
  go Leaf = singleton kx d
  go (Node ky y l r) = case trueCompare kx ky
      of  TrueEQ HRefl -> Node kx d l r
          TrueLT       -> Node ky y (go l) r
          TrueGT       -> Node ky y l (go r)

-- | Map over a tree
map ::  (forall a. Key a -> Assoc c a -> Assoc c a) -> DepMap c -> DepMap c
map f = go where
  go Leaf            = Leaf
  go (Node kx x l r) = Node kx (f kx x) (go l) (go r)

-- | Combine the entries of two traces with an operation when their keys match,
--   returning elements of the left trace that do not exist in the second trace.
intersectLeftWith :: (forall d. DiffDistribution d => d -> Vec (Arity d) Double -> d) -> Guides -> ΔGuides -> Guides
intersectLeftWith _ t1 Leaf  = t1
intersectLeftWith _ Leaf t2  = Leaf
intersectLeftWith f (Node k1 x1 l1 r1) t2 =
  case maybe_x2 of
      Nothing -> Node k1 x1 l1 r1
      Just x2 -> Node k1 (f x1 x2) l1l2 r1r2
    where (l2, maybe_x2, r2) = dsplitLookup k1 t2
          !l1l2 = intersectLeftWith f l1 l2
          !r1r2 = intersectLeftWith f r1 r2
          -- | Split-lookup without rebalancing tree
          dsplitLookup :: Typeable d => Key d -> ΔGuides -> (ΔGuides, Maybe (Vec (Arity d) Double), ΔGuides)
          dsplitLookup k = go where
            go Leaf            = (Leaf, Nothing, Leaf)
            go (Node kx x l r) = case trueCompare k kx of
              TrueLT       -> let (lt, z, gt) = go l in (lt, z, Node kx x gt r) -- As we know that `keys of gt` < `keys of r`
              TrueGT       -> let (lt, z, gt) = go r in (Node kx x l lt, z, gt)
              TrueEQ HRefl -> (l, Just x, r)