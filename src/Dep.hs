
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

module Dep where

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
import Data.Dependent.Map
import Data.Functor.Identity (Identity)
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

data GetVec q where
  GetVec :: DiffDist q a => Vec (Arity q) a -> GetVec q

data Key q where
  Key :: Typeable q => Addr -> Key q
  deriving Typeable

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
type GradTrace  = DMap Key GetVec

lookupGuide :: DiffDist q a => Key q  -> GuideTrace -> Maybe (Identity q)
lookupGuide (Key a) guides = lookup (Key a) guides