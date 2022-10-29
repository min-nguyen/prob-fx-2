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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

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
  , Some(..)
  , dempty
  , dkeys
  , dinsert
  , dlookup
  , dlookupDefault
  , dlookupOrInsert
  , dintersectLeftWith
  -- , dmap
  ) where

import Type.Reflection
    ( Typeable, type (:~~:)(HRefl), eqTypeRep, typeRep )
import Data.Map (Map)
import Data.Maybe ( fromJust, fromMaybe )
import Data.Proxy ( Proxy(..) )
import Effects.Dist ( Tag, Addr, Observe, Sample(..), Score(..), pattern ObsPrj, pattern SampPrj )
import Env ( enil, varToStr, UniqueVar, Var(..), Env(ECons), Assign((:=)) )
import GHC.TypeLits ( KnownSymbol )
import OpenSum (OpenSum)
import LogP ( LogP )
import qualified Data.Map as Map
import qualified OpenSum
import Prog ( Member, Prog(..), weaken, install )
import PrimDist
import Effects.State ( State, modify, handleState )
import Util
import TyCompare
import Prog (Member(..))
import Vec (Vec)
import Data.Coerce (coerce)
import Data.Type.Coercion (coerceWith)

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

data CTrace c where
  Leaf :: CTrace c
  Node  :: (c a b, Typeable a, Typeable b)
        => Key a         -- key
        -> b            -- distribution
        -> CTrace c        -- left
        -> CTrace c        -- right
        -> CTrace c

class (DiffDistribution d, d ~ d') => DiffDistR d d' where

instance (DiffDistribution d, d ~ d') => DiffDistR d d' where

{- | The type of differentiable distribution traces. -}
type DTrace = CTrace DiffDistR

-- data DTrace where
--   Leaf :: DTrace
--   Node :: (DiffDistribution d)
--        => Key d         -- key
--        -> d              -- distribution
--        -> DTrace         -- left
--        -> DTrace         -- right
--        -> DTrace

class (DiffDistribution d, Typeable v, v ~ Vec (Arity d) Double) => GradR d v where

instance (DiffDistribution d,  Typeable v, v ~ Vec (Arity d) Double) => GradR d v where

{- | The type of gradient traces. -}
type GTrace = CTrace GradR

-- data GTrace where
--   Leaf :: CTrace c t
--   Node  :: (c d, a ~ t d)
--         => Key d         -- key
--         -> a            -- distribution
--         -> CTrace c t       -- left
--         -> CTrace c t       -- right
--         -> CTrace c t

type family Id a where
  Id a = a

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

{-
-}
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

instance (HeteroOrd k, Typeable a, Typeable b) => TrueOrd k a b where
  trueCompare :: k a -> k b -> TrueOrdering a b
  trueCompare ka kb = case (hCompare ka kb, compare (show (typeRep @a)) (show (typeRep @a))) of
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
dkeys :: DTrace -> [Some DiffDistribution Key]
dkeys = go
  where
    go :: DTrace -> [Some DiffDistribution Key]
    go Leaf = []
    go (Node ky y l r) = Some ky : go l ++ go r

-- | Construct an empty trace
dempty :: DTrace
dempty = Leaf

-- | Construct a single node
dsingleton :: (Typeable a, Typeable b, c a b) => Key a -> b -> CTrace c
dsingleton k x = Node k x Leaf Leaf

-- | Lookup an entry
dlookup :: forall c a b. c a b => (Typeable a, Typeable b) => Key a -> CTrace c -> Maybe b
dlookup kx = go where
  go Leaf = Nothing
  go (Node ky y l r) = case (trueCompare kx ky, tyEq (Proxy @b) (asProxy y))
      of (TrueEQ HRefl, Just HRefl) -> Just y
         (TrueEQ HRefl, Nothing)    -> error "Trace.dlookup: Should not happen if 'a implies b' in the constraint 'c a b' "
         (TrueLT, _)       -> go l
         (TrueGT, _)       -> go r

dlookupDefault :: (c a b, Typeable a, Typeable b) => Key a -> CTrace c -> b -> b
dlookupDefault kx dmap def = fromMaybe def (dlookup kx dmap)

-- | Insert a new entry
dinsert :: forall c a b. (Typeable a, Typeable b) => c a b => Key a -> b -> CTrace c -> CTrace c
dinsert kx d = go where
  go Leaf = dsingleton kx d
  go (Node ky y l r) = case trueCompare kx ky
      of  TrueEQ HRefl -> Node kx d l r
          TrueLT       -> Node ky y (go l) r
          TrueGT       -> Node ky y l (go r)

-- | Apply a function to an entry if it exists
dupdate :: forall c a b. (c a b, Typeable a, Typeable b) => Key a -> (b -> b) -> CTrace c -> CTrace c
dupdate kx f = go where
  go Leaf = Leaf
  go (Node ky y l r) = case (trueCompare kx ky, tyEq (Proxy @b) (asProxy y))
      of (TrueEQ HRefl, Just HRefl) -> Node kx (f y) l r
         (TrueEQ HRefl, Nothing)    -> error "Trace.dupdate: Should not happen if 'a implies b' in the constraint 'c a b' "
         (TrueLT, _)       -> Node ky y (go l) r
         (TrueGT, _)       -> Node ky y l (go r)

-- | Return the entry if it exists, otherwise insert a new entry
dlookupOrInsert :: forall c a b. (c a b, Typeable a, Typeable b) => Key a -> b -> CTrace c  -> (b, CTrace c )
dlookupOrInsert kx dx = go where
  go :: CTrace c  -> (b, CTrace c )
  go Leaf             = (dx, dsingleton kx dx)
  go (Node ky dy l r) =
    case (trueCompare kx ky, tyEq (Proxy @b) (asProxy dy)) of
      (TrueEQ HRefl, Just HRefl) -> (dy, Node ky dy l r)
      (TrueEQ HRefl, Nothing)    -> error "Trace.dlookupOrInsert: Should not happen if 'a implies b' in the constraint 'c a b' "
      (TrueLT, _)    -> let (d, l') = go l in (d, Node ky dy l' r)
      (TrueGT, _)       -> let (d, r') = go r in (d, Node ky dy l r')

-- | Fold over a tree
dfoldr :: (forall a b r. c a b => Key a -> b -> r -> r) -> r -> CTrace c -> r
dfoldr f = go where
  go z Leaf             = z
  go z (Node  kx x l r) = go (f kx x (go z r)) l

-- -- | Map over a tree
-- dmap :: (forall a. (c a b, Typeable b) => b -> b) -> CTrace c -> CTrace c
-- dmap f = go where
--   go Leaf            = Leaf
--   go (Node kx x l r) = Node kx (f x) (go l) (go r)

-- | Combine the entries of two traces with an operation when their keys match,
--   returning elements of the left trace that do not exist in the second trace.
dintersectLeftWith :: (forall d. DiffDistribution d => d -> Vec (Arity d) Double -> d) -> DTrace -> GTrace -> DTrace
dintersectLeftWith _ t1 Leaf  = t1
dintersectLeftWith _ Leaf t2  = Leaf
dintersectLeftWith f (Node k1 x1 l1 r1) t2 =
  case maybe_x2 of
      Nothing -> Node k1 x1 l1 r1
      Just x2 -> Node k1 (f x1 x2) l1l2 r1r2
    where (l2, maybe_x2, r2) =  dsplitLookup k1 t2
          !l1l2 = dintersectLeftWith f l1 l2
          !r1r2 = dintersectLeftWith f r1 r2

-- | Split-lookup without rebalancing tree
dsplitLookup :: Typeable d => Key d -> GTrace -> (GTrace, Maybe (Vec (Arity d) Double), GTrace)
dsplitLookup k = go where
  go Leaf            = (Leaf, Nothing, Leaf)
  go (Node kx x l r) = case trueCompare k kx of
    TrueLT       -> let (lt, z, gt) = go l in (lt, z, Node kx x gt r) -- As we know that `keys of gt` < `keys of r`
    TrueGT       -> let (lt, z, gt) = go r in (Node kx x l lt, z, gt)
    TrueEQ HRefl -> (l, Just x, r)
