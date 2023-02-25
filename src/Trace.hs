
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

module Trace (
  -- * Addr
    Tag
  , Addr(..)
  -- * Sample trace
  , Trace(..)
  , filterTrace
  -- * Log-probability trace
  , LPTrace
  -- , traceLogProbs
  -- * Gradient trace
  , GradTrace
  , VecFor(..)
  , unVecFor
  -- * Dist trace
  , GuideTrace
  , Key(..)
  -- , lookupBy
  -- , lookupWithDefault
  -- , lookupOrInsert
  -- , map
  -- , dlookupOrInsert
  , intersectWithAdd
  , module Data.Dependent.Map
  , module Data.Functor.Identity
  -- , dmap
  ) where

import           Data.Dependent.Map
import           Data.Map (Map)
import           Data.Maybe ( fromJust, fromMaybe )
import           Data.Proxy ( Proxy(..) )
import           GHC.TypeLits ( KnownSymbol )
import           LogP ( LogP )
import           Comp ( Member, Comp(..), weaken, install )
import           PrimDist
import           Util
import           TyCompare
import           Vec (Vec)
import           Type.Reflection
import           Data.Kind (Constraint)
import           Prelude hiding (lookup, map)
import Control.Applicative ((<|>))
import Unsafe.Coerce
import Data.Functor.Identity
import Data.GADT.Compare
import Data.Typeable


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

data VecFor q where
  VecFor :: DiffDist q a => Vec (Arity q) Double -> VecFor q

unVecFor :: DiffDist q a => VecFor q -> Vec (Arity q) Double
unVecFor (VecFor q) = q

data Key q where
  Key :: (DiffDist q a, Typeable q) => Addr -> Key q
  deriving Typeable

instance Show (Key a) where
  show :: Key a -> String
  show (Key s) = show s

instance Eq (Key  a) where
  (==) :: Key a -> Key a -> Bool
  Key a == Key b = a == b

instance Ord (Key a) where
  compare :: Key a -> Key a -> Ordering
  compare (Key s1) (Key s2) = compare s1 s2

instance GEq Key where
  geq (Key a :: Key q1) (Key b :: Key q2) =
    if a == b then eqT @q1 @q2 else Nothing

instance GCompare Key where
  gcompare k1@(Key a :: Key q1) k2@(Key b :: Key q2) =
    case (compare a b, compare (show (Type.Reflection.typeRep @q1)) (show (Type.Reflection.typeRep @q2))) of
      (LT, _) -> GLT
      (GT, _) -> GGT
      (_, LT) -> GLT
      (_, GT) -> GGT
      (EQ, EQ) -> case geq k1 k2 of Just Refl -> GEQ
                                    Nothing   -> error "shouldn't happen"

type GuideTrace = DMap Key Identity
type GradTrace  = DMap Key VecFor

lookupGuide :: DiffDist q a => Key q  -> GuideTrace -> Maybe (Identity q)
lookupGuide (Key a) guides = lookup (Key a) guides

insertGrad :: DiffDist q a => Key q  -> Vec (Arity q) Double -> GradTrace -> GradTrace
insertGrad k vec grads = insert k (VecFor vec) grads

intersectWithAdd :: GuideTrace -> GradTrace -> GuideTrace
intersectWithAdd guides grads = intersectionWithKey f guides grads
  where f :: Key q -> Identity q -> VecFor q -> Identity q
        f _ (Identity q) (VecFor v) = Identity (safeAddGrad q v)
