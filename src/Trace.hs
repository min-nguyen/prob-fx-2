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

{- | For recording samples and log-probabilities during model execution.
-}

module Trace (
  -- * Sample trace
    STrace
  , filterTrace
  -- * Log-probability trace
  , LPTrace
  , traceLogProbs
  -- * Gradient trace
  , GTrace
  -- * Dist trace
  , DTrace
  , DKey
  , dempty
  , dkeys
  , dinsert
  , dlookup
  , dlookupOrInsert
  , dintersectLeftWith
  , dmap
  , Key(..)
  , Some(..)) where

import Type.Reflection
    ( Typeable, type (:~~:)(HRefl), eqTypeRep, typeRep )
import Data.Map (Map)
import Data.Maybe ( fromJust )
import Data.Proxy ( Proxy(..) )
import Effects.Dist ( Tag, Addr, Observe, Sample(..), pattern ObsPrj, pattern SampPrj )
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

-- | Update a sample trace at an address
updateTrace :: (Member (State (Map Addr v)) es) =>
  -- | address of sample site
     Addr
  -- | primitive distribution at address
  -- | sampled value
  -> v
  -> Prog es ()
updateTrace α v = modify (Map.insert α v)

{- | The type of inverse sample traces, mapping addresses of Sample operations
     to the random values between 0 and 1 passed to their inverse CDF functions.
-}
type STrace = Trace Double

{- | The type of log-probability traces, mapping addresses of sample/observe operations
     to their log probabilities.
-}
type LPTrace = Trace LogP

-- | Insert stateful operations for recording the log-probabilities at each @Sample@ or @Observe@ operation
traceLogProbs :: (Member Sample es, Member Observe es) => Prog es a -> Prog es (a, LPTrace)
traceLogProbs = handleState Map.empty . storeLPs
  where storeLPs :: (Member Sample es, Member Observe es) => Prog es a -> Prog (State LPTrace: es) a
        storeLPs (Val x) = pure x
        storeLPs (Op u k) = do
          case u of
            SampPrj d α
              -> Op (weaken u) (\x -> updateTrace α (logProb d x) >> storeLPs (k x))
            ObsPrj d y α
              -> Op (weaken u) (\x -> updateTrace α (logProb d x) >> storeLPs (k x))
            _ -> Op (weaken u) (storeLPs . k)


{- | The type of gradient traces.
-}
type GTrace = DTrace

{- | The type of differentiable primitive distribution traces.
-}
data DTrace where
  Leaf :: DTrace
  Node :: (DiffDistribution d)
       => DKey d         -- key
       -> d              -- distribution
       -> DTrace         -- left
       -> DTrace         -- right
       -> DTrace

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

type DKey = Key Addr

data Key s a where
  Key :: forall s a. (Ord s, Typeable a) => s -> Key s a
  deriving (Typeable)

instance Show s => Show (Key s a) where
  show :: Key s a -> String
  show (Key s) = show s

instance Eq (Key s a) where
  (==) :: Key s a -> Key s a -> Bool
  Key a == Key b = a == b

instance Ord (Key s a) where
  compare :: Key s a -> Key s a -> Ordering
  compare (Key s1) (Key s2) = compare s1 s2

instance Ord s => HeteroOrd (Key s) where
  hCompare :: Key s a -> Key s b -> Ordering
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

data Some k where
  Some :: DiffDistribution d => k d -> Some k

dkeys :: DTrace -> [Some DKey]
dkeys = go
  where
    go Leaf = []
    go (Node ky y l r) = Some ky : go l ++ go r

-- | Construct an empty trace
dempty :: DTrace
dempty = Leaf

-- | Construct a single node
dsingleton :: DiffDistribution d => DKey d -> d -> DTrace
dsingleton k x = Node k x Leaf Leaf

-- | Lookup an entry
dlookup :: Typeable d => DKey d -> DTrace -> Maybe d
dlookup kx = go
  where
    go Leaf = Nothing
    go (Node ky y l r) = case trueCompare kx ky
        of TrueEQ HRefl -> Just y
           TrueLT       -> go l
           TrueGT       -> go r

-- | Insert a new entry
dinsert :: DiffDistribution d => DKey d -> d -> DTrace  -> DTrace
dinsert kx d = go
  where
    go :: DTrace -> DTrace
    go Leaf = dsingleton kx d
    go (Node ky y l r) = case trueCompare kx ky
        of  TrueEQ HRefl -> Node kx d l r
            TrueLT       -> Node ky y (go l) r
            TrueGT       -> Node ky y l (go r)

-- | Apply a function to an entry if it exists
dupdate :: DiffDistribution d => DKey d -> (d -> d) -> DTrace -> DTrace
dupdate kx f = go
  where
    go :: DTrace -> DTrace
    go Leaf = Leaf
    go (Node ky y l r) = case trueCompare kx ky
        of TrueEQ HRefl -> Node kx (f y) l r
           TrueLT       -> Node ky y (go l) r
           TrueGT       -> Node ky y l (go r)

-- | Return the entry if it exists, otherwise insert a new entry
dlookupOrInsert :: forall d a. DiffDistribution d => DKey d -> d -> DTrace -> (d, DTrace)
dlookupOrInsert kx dx  = go
  where
    go :: DTrace -> (d, DTrace)
    go Leaf             = (dx, dsingleton kx dx)
    go (Node ky dy l r) =
      case trueCompare kx ky of
        TrueEQ HRefl -> (dy, Node ky dy l r)
        TrueLT       -> let (d, l') = go l in (d, Node ky dy l' r)
        TrueGT       -> let (d, r') = go r in (d, Node ky dy l r')

-- | Combine the entries of two traces with an operation when their keys match,
--   returning elements of the left trace that do not exist in the second trace.
dintersectLeftWith :: (forall d a. PrimDist d a => d -> d -> d) -> DTrace -> GTrace -> DTrace
dintersectLeftWith _ t1 Leaf  = t1
dintersectLeftWith _ Leaf t2  = Leaf
dintersectLeftWith f (Node k1 x1 l1 r1) t2 =
  case maybe_x2 of
      Nothing -> Node k1 x1 l1 r1
      Just x2 -> Node k1 (f undefined undefined) l1l2 r1r2
    where (l2, maybe_x2, r2) =  dsplitLookup k1 t2
          !l1l2 = dintersectLeftWith f l1 l2
          !r1r2 = dintersectLeftWith f r1 r2

-- | Split-lookup without rebalancing tree
dsplitLookup :: PrimDist d a => DKey d -> DTrace -> (DTrace, Maybe d, DTrace)
dsplitLookup k = go
  where
    go Leaf            = (Leaf, Nothing, Leaf)
    go (Node kx x l r) = case trueCompare k kx of
      TrueLT -> let (lt, z, gt) = go l in (lt, z, Node kx x gt r) -- As we know that `keys of gt` < `keys of r`
      TrueGT -> let (lt, z, gt) = go r in (Node kx x l lt, z, gt)
      TrueEQ HRefl -> (l, Just x, r)

-- | Fold over a tree
dfoldr :: (forall d a. PrimDist d a => DKey d -> d -> b -> b) -> b -> DTrace -> b
dfoldr f = go
  where
    go z Leaf             = z
    go z (Node  kx x l r) = go (f kx x (go z r)) l

-- | Fold over a tree
dmap :: (forall d a. DiffDistribution d => d -> d) -> DTrace -> DTrace
dmap f = go
  where
    go  Leaf             = Leaf
    go  (Node  kx x l r) = Node kx (f x) (go l) (go r)