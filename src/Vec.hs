{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFoldable #-}

module Vec (module Vec, module GHC.TypeNats, module Data.Vector) where

import           Data.Proxy
import           GHC.TypeNats
import qualified Data.Vector as Vector
import           Data.Vector (Vector)


newtype Vec (n :: Nat) a = UnsafeMkVec { getVector :: Vector a }
    deriving (Show, Foldable)



mkVec :: forall n a. KnownNat n => Vector a -> Maybe (Vec n a)
mkVec v | length v == l = Just (UnsafeMkVec v)
        | otherwise     = Nothing
  where
    l = fromIntegral (natVal (Proxy @n))

fromList :: forall n a. KnownNat n => [a] -> Maybe (Vec n a)
fromList = mkVec . Vector.fromList

toList :: forall n a. KnownNat n => Vec n a -> [a]
toList (UnsafeMkVec xs) = Vector.toList xs

zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith f (UnsafeMkVec xs) (UnsafeMkVec ys) = UnsafeMkVec (Vector.zipWith f xs ys)

map :: (a -> b) -> Vec n a -> Vec n b
map f (UnsafeMkVec xs) = UnsafeMkVec (Vector.map f xs)

replicate :: KnownNat n => Int -> a -> Maybe (Vec n a)
replicate n a = mkVec (Vector.replicate n a)

replicateNat :: forall n a . KnownNat n => Proxy n -> a -> Vec n a
replicateNat n a = UnsafeMkVec (Vector.replicate (fromIntegral $ natVal (Proxy @n)) a)

replicateMNat :: forall m n a . Monad m => KnownNat n => Proxy n -> m a -> m (Vec n a)
replicateMNat n a = fmap UnsafeMkVec (Vector.replicateM (fromIntegral $ natVal (Proxy @n)) a)