{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{- | For recording samples and log-probabilities during model execution.
-}

module Trace (
  -- * Sample trace
    STrace
  , filterTrace
  -- * Log-probability trace
  , LPTrace
  -- , traceLogProbs
  -- * Gradient trace
  , GTrace
  -- * Dist trace
  , DTrace
  , Key(..)
  , keys
  , Some(..)
  , empty
  , insert
  , lookup
  , map
  -- , dlookupOrInsert
  , intersectLeftWith
  -- , dmap
  ) where

import           Data.Map (Map)
import           Data.Maybe ( fromJust, fromMaybe )
import           Data.Proxy ( Proxy(..) )
import           Effects.Dist ( Tag, Addr, Observe, Sample(..), pattern ObsPrj, pattern SampPrj )
import           Env ( enil, varToStr, UniqueVar, Var(..), Env(ECons), Assign((:=)) )
import           GHC.TypeLits ( KnownSymbol )
import           LogP ( LogP )
import           Prog ( Member, Prog(..), weaken, install )
import           PrimDist
import           Util
import           TyCompare
import           Vec (Vec)
import           Type.Reflection
import           Data.Kind (Constraint)
import           Prelude hiding (lookup, map)

{- | The type of generic traces, mapping addresses of probabilistic operations
     to some data.
-}
type Trace a = Map Addr a

-- | Retrieve the values for the specified observable variable names
filterTrace ::
  -- | observable variable names
    [Tag]
  -- | trace
  -> Map Addr a
  -- | filtered trace
  -> Map Addr a
filterTrace tags = filterByKey (\(tag, idx)  -> tag `elem` tags)

{- | The type of inverse sample traces, mapping addresses of Sample operations
     to the random values between 0 and 1 passed to their inverse CDF functions.
-}
type STrace = Trace Double

{- | The type of log-probability traces, mapping addresses of sample/observe operations
     to their log probabilities.
-}
type LPTrace = Trace LogP

{- | Dependent map. -}
data DistTrace c where
  Leaf  :: DistTrace c
  Node  :: (DiffDistribution a)
        => Key a        -- key
        -> Assoc c a    -- distribution
        -> DistTrace c  -- left
        -> DistTrace c  -- right
        -> DistTrace c

-- | A workaround for passing type families/functional dependencies as type parameters
class Id d
class Grad d
type family Assoc (c :: * -> Constraint) (a :: *) = (b :: *) where
  Assoc Id d   = d
  Assoc Grad d = Vec (Arity d) Double

-- | The type of differentiable distribution traces
type DTrace = DistTrace Id

instance {-# OVERLAPPING #-} Show [DTrace] where
  show (x:xs) = show x ++ "\n" ++ show xs
  show []     = ""

instance Show DTrace where
  show :: DTrace -> String
  show Leaf = ""
  show (Node (Key var) d l r) = "(" ++ show var ++ ", " ++ show d ++ ") "
                                 ++ show l
                                 ++ show r
    where showNewline Leaf  = ""
          showNewline node  = "\n" ++ show node

-- | The type of gradient traces
type GTrace = DistTrace Grad

instance Show GTrace where
  show :: GTrace -> String
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
keys :: DistTrace c -> [Some DiffDistribution Key]
keys = go
  where
    go :: DistTrace c -> [Some DiffDistribution Key]
    go Leaf = []
    go (Node ky y l r) = Some ky : go l ++ go r

-- | Construct an empty trace
empty :: DistTrace c
empty = Leaf

-- | Construct a single node
singleton :: (DiffDistribution a) => Key a -> Assoc c a -> DistTrace c
singleton k x = Node k x Leaf Leaf

-- | Lookup an entry
lookup :: Typeable a => Key a -> DistTrace c -> Maybe (Assoc c a)
lookup kx = go where
  go Leaf = Nothing
  go (Node ky y l r) = case (trueCompare kx ky, ())
      of (TrueEQ HRefl, _) -> Just y
         (TrueLT, _)       -> go l
         (TrueGT, _)       -> go r

-- | Insert a new entry
insert :: DiffDistribution a => Key a -> Assoc c a -> DistTrace c -> DistTrace c
insert kx d = go where
  go Leaf = singleton kx d
  go (Node ky y l r) = case trueCompare kx ky
      of  TrueEQ HRefl -> Node kx d l r
          TrueLT       -> Node ky y (go l) r
          TrueGT       -> Node ky y l (go r)

-- | Map over a tree
map ::  (forall a. Key a -> Assoc c a -> Assoc c a) -> DistTrace c -> DistTrace c
map f = go where
  go Leaf            = Leaf
  go (Node kx x l r) = Node kx (f kx x) (go l) (go r)

-- | Combine the entries of two traces with an operation when their keys match,
--   returning elements of the left trace that do not exist in the second trace.
intersectLeftWith :: (forall d. DiffDistribution d => d -> Vec (Arity d) Double -> d) -> DTrace -> GTrace -> DTrace
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
          dsplitLookup :: Typeable d => Key d -> GTrace -> (GTrace, Maybe (Vec (Arity d) Double), GTrace)
          dsplitLookup k = go where
            go Leaf            = (Leaf, Nothing, Leaf)
            go (Node kx x l r) = case trueCompare k kx of
              TrueLT       -> let (lt, z, gt) = go l in (lt, z, Node kx x gt r) -- As we know that `keys of gt` < `keys of r`
              TrueGT       -> let (lt, z, gt) = go r in (Node kx x l lt, z, gt)
              TrueEQ HRefl -> (l, Just x, r)

-- -- | Apply a function to an entry if it exists
-- dupdate :: forall c a b. (c a b, Typeable a, Typeable b) => Key a -> (b -> b) -> DistTrace c -> DistTrace c
-- dupdate kx f = go where
--   go Leaf = Leaf
--   go (Node ky y l r) = case (trueCompare kx ky, tyEq (Proxy @b) (asProxy y))
--       of (TrueEQ HRefl, Just HRefl) -> Node kx (f y) l r
--          (TrueEQ HRefl, Nothing)    -> error "Trace.dupdate: Should not happen if 'a implies b' in the constraint 'c a b' "
--          (TrueLT, _)       -> Node ky y (go l) r
--          (TrueGT, _)       -> Node ky y l (go r)

-- -- | Return the entry if it exists, otherwise insert a new entry
-- dlookupOrInsert :: forall c a b. (c a b, Typeable a, Typeable b) => Key a -> b -> DistTrace c  -> (b, DistTrace c )
-- dlookupOrInsert kx dx = go where
--   go :: DistTrace c  -> (b, DistTrace c )
--   go Leaf             = (dx, dsingleton kx dx)
--   go (Node ky dy l r) =
--     case (trueCompare kx ky, tyEq (Proxy @b) (asProxy dy)) of
--       (TrueEQ HRefl, Just HRefl) -> (dy, Node ky dy l r)
--       (TrueEQ HRefl, Nothing)    -> error "Trace.dlookupOrInsert: Should not happen if 'a implies b' in the constraint 'c a b' "
--       (TrueLT, _)    -> let (d, l') = go l in (d, Node ky dy l' r)
--       (TrueGT, _)       -> let (d, r') = go r in (d, Node ky dy l r')

-- -- | Fold over a tree
-- dfoldr :: (forall a b r. c a b => Key a -> b -> r -> r) -> r -> DistTrace c -> r
-- dfoldr f = go where
--   go z Leaf             = z
--   go z (Node  kx x l r) = go (f kx x (go z r)) l
