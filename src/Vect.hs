{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeOperators #-}

module Vect (module Vect, module Data.Vector) where

import           Data.Maybe
import           Data.Proxy
import           Data.Type.Nat
import           Data.Typeable
import qualified Data.Vector as Vector
import           Data.Vector (Vector, (!))

class (SNatI n, Typeable n) => TyNat n

instance (SNatI n, Typeable n) => TyNat n

newtype Vect (n :: Nat) a = UnsafeMkVec { getVector :: Vector a }
    deriving (Show, Foldable)

(!!) :: Vect n a -> Int ->  a
(!!) (UnsafeMkVec xs) n = xs ! n

fromVector :: forall n a. TyNat n => Vector a -> Maybe (Vect n a)
fromVector v | length v == l = Just (UnsafeMkVec v)
             | otherwise     = error ("Vect.hs: length is actually " ++ show (length v) ++ " and not " ++ show l) Nothing
  where
    l = reflectToNum (Proxy @n)

unsafeFromVector :: forall n a. TyNat n => Vector a -> Vect n a
unsafeFromVector = fromJust . fromVector

fromList :: TyNat n => [a] -> Maybe (Vect n a)
fromList = fromVector . Vector.fromList

unsafeFromList :: TyNat n => [a] -> Vect n a
unsafeFromList = fromJust . fromList

toList :: Vect n a -> [a]
toList (UnsafeMkVec xs) = Vector.toList xs

toList2d :: (TyNat n, TyNat m) => Vect m (Vect n a) -> [[a]]
toList2d (UnsafeMkVec xs) = Vector.toList $ Vector.map toList xs

zipWith :: (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
zipWith f (UnsafeMkVec xs) (UnsafeMkVec ys) = UnsafeMkVec (Vector.zipWith f xs ys)

map :: (a -> b) -> Vect n a -> Vect n b
map f (UnsafeMkVec xs) = UnsafeMkVec (Vector.map f xs)

replicate :: forall n a. TyNat n => SNat n -> a -> Vect n a
replicate n a = UnsafeMkVec (Vector.replicate (reflectToNum (Proxy @n)) a)

replicateM :: forall m n a . Monad m => TyNat n => SNat n -> m a -> m (Vect n a)
replicateM n a = fmap UnsafeMkVec (Vector.replicateM (reflectToNum (Proxy @n)) a)