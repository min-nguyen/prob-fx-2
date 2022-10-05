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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}

{- | For recording samples and log-probabilities during model execution.
-}

module Trace (
  -- * Sample trace
    InvSTrace
  , filterTrace
  -- * Log-probability trace
  , LPTrace
  , traceLogProbs
  -- * Gradient trace
  , GTrace
  -- * Dist trace
  , DTrace
  , lookupOrInsert
  , Key(..)) where

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
              -> Op (weaken u) (\x -> updateTrace α (PrimDist.logProb d x) >> storeLPs (k x))
            ObsPrj d y α
              -> Op (weaken u) (\x -> updateTrace α (PrimDist.logProb d x) >> storeLPs (k x))
            _ -> Op (weaken u) (storeLPs . k)


{- | The type of inverse sample traces, mapping addresses of sample operations
     to the random values between 0 and 1 passed to their inverse CDF functions.
-}
type InvSTrace = Trace Double


-- {- | The type of sample traces, mapping addresses of sample operations
--      to their primitive distributions and sampled values. This can be used
--      to record the sampled values for the exact addresses of all sampling
--      operations, regardless of whether or not they are associated with an
--      observable variable in a model environment.
-- -}
type STrace = Trace (ErasedPrimDist, OpenSum PrimVal)

-- | Forget the type of values generated by a primitive distribution
data ErasedPrimDist where
  ErasedPrimDist :: PrimDist a -> ErasedPrimDist

instance Show ErasedPrimDist where
  show (ErasedPrimDist d) = show d

-- | For converting sample traces to model environments
class FromSTrace a where
  -- | Convert a sample trace to a model environment
  fromSTrace :: STrace -> Env a

instance FromSTrace '[] where
  fromSTrace _ = enil

instance (UniqueVar x env ~ 'True, KnownSymbol x, Eq a, OpenSum.Member a PrimVal, FromSTrace env) => FromSTrace ((x := a) : env) where
  fromSTrace sMap = ECons (extractSTrace (Var @x, Proxy @a) sMap) (fromSTrace sMap)
    where extractSTrace ::  forall a x. (Eq a, OpenSum.Member a PrimVal) => (Var x, Proxy a) -> STrace -> [a]
          extractSTrace (x, typ)  =
              map (fromJust . OpenSum.prj @a . snd . snd)
            . Map.toList
            . Map.filterWithKey (\(tag, idx) _ -> tag == varToStr x)

-- | Insert stateful operations for recording the sampled values at each @Sample@ operation
traceSamples :: (Member Sample es) => Prog es a -> Prog es (a, STrace)
traceSamples = handleState Map.empty . storeSamples
  where storeSamples :: (Member Sample es) => Prog es a -> Prog (State STrace ': es) a
        storeSamples = install pure
          (\x tx k -> case tx of
              Sample (PrimDistPrf d) α -> do updateTrace α (ErasedPrimDist d, OpenSum.inj x :: OpenSum PrimVal)
                                             k x
          )

{- | The type of gradient traces.
-}
type GTrace = Trace Double

{- | The type of primitive distribution traces.
-}

data DTrace where
  Leaf :: DTrace
  Node :: Typeable v
       => DKey v         -- key
       -> PrimDist v     -- value
       -> DTrace         -- left
       -> DTrace         -- right
       -> DTrace

type DKey = Key Addr

data Key s a where
  Key :: forall s a. (Ord s, Typeable a) => s -> Key s a
  deriving (Typeable)

instance Show s => Show (Key s a) where
  show (Key s) = show s

instance Eq (Key s a) where
  Key a == Key b = a == b

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

instance Ord (Key s a) where
  compare :: forall k s (a :: k). Key s a -> Key s a -> Ordering
  compare (Key a) (Key b) = compare a b

empty :: DTrace
empty = Leaf

singleton :: (Typeable v) => DKey v -> PrimDist v -> DTrace
singleton k x = Node k x Leaf Leaf

lookup :: forall a b. (Eq a, Typeable a)
  => DKey a -> DTrace  -> Maybe (PrimDist a)
lookup k = go
  where
    go :: DTrace -> Maybe (PrimDist a)
    go Leaf = Nothing
    go (Node k' x l r) = case trueCompare k k'
        of TrueEQ HRefl -> if k' == k then Just x else Nothing
           TrueNEQ      -> Nothing
           TrueLT       -> go l
           TrueGT       -> go r

insert :: forall v. (Typeable v) => DKey v -> PrimDist v -> DTrace  -> DTrace
insert kx d = go
  where
    go :: DTrace -> DTrace
    go Leaf = singleton kx d
    go (Node ky y l r) =
      case trueCompare kx ky of
        TrueEQ HRefl -> Node kx d l r
        TrueNEQ      -> Node ky y l r
        TrueLT       -> Node ky y (go l) r
        TrueGT       -> Node ky y l (go r)

lookupOrInsert :: forall v. (Typeable v) => DKey v -> PrimDist v -> DTrace -> (PrimDist v, DTrace)
lookupOrInsert kx dx  = go
  where
    go :: DTrace -> (PrimDist v, DTrace)
    go Leaf             = (dx, singleton kx dx)
    go (Node ky dy l r) =
      case trueCompare kx ky of
        TrueEQ HRefl -> (dy, Node ky dy l r)
        TrueNEQ      -> (dx, Node kx dx l r) -- ? replace key
        TrueLT       -> let (d, l') = go l in (d, Node ky dy l' r)
        TrueGT       -> let (d, r') = go r in (d, Node ky dy l r')
