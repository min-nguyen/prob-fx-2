{- | Some small utility functions.
-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Util (
    boolToInt
  , safeHead
  , safeTail
  , accumulate
  , findIndexes
  , roundUp16
  , roundPrecision
  , assocR
  , uncurry3
  , fst3
  , Util.unzip3
  , unzip4
  , mapT2
  , mapT3
  , filterByKey
  , mean
  , variance
  , covariance) where

import qualified Data.Map as Map
import           Data.Foldable
import           Data.Bifunctor (Bifunctor(first))

{- | List utility functions.
-}
-- | Safely attempt to return the head of a list
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

-- | Return the tail of a list, behaving as the identity function upon an empty list
safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

accumulate :: Monad m => (a -> m a) -> ([a] -> m [a])
accumulate f as = do
  a' <- f (head as)
  pure (a':as)

-- | Return all the positions that a value occurs within a list
findIndexes :: Eq a => [a] -> a -> [Int]
findIndexes xs a = reverse $ go xs 0 []
  where
  go (x:xs) i inds = if x == a then go xs (i + 1) (i:inds)
                     else go xs (i + 1) inds
  go [] i inds = inds

-- | Return True -> 1, False -> 0
boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

{- | Tuple utility functions.
-}
assocR :: ((a, b), c) -> (a, (b, c))
assocR ((x, y), z) = (x, (y, z))

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c) = f a b c

fst3 :: (a, b, c) -> a
fst3 (x,_, _) = x

mapT2 :: (a -> b) -> (a, a) -> (b, b)
mapT2 f (x, y) = (f x, f y)

mapT3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapT3 f (x, y, z) = (f x, f y, f z)

unzip3 :: [((a, b), c)] -> (([a], [b]), [c])
unzip3 = first unzip . unzip

unzip4 :: [(((a, b), c), d)] -> ((([a], [b]), [c]), [d])
unzip4 = first (first unzip . unzip) . unzip

{- Map utility functions.
-}
filterByKey ::  Ord k => (k -> Bool) -> Map.Map k a -> Map.Map k a
filterByKey f = Map.filterWithKey (\k _ -> f k)

{- | Numeric utility functions.
-}
-- | Round @n@ up to multiple of 16
roundUp16 :: Int -> Int
roundUp16 n = n + (16 - (n `mod` 16))

-- | Round @x@ to @n@ decimal places
roundPrecision :: Int -> Double -> Double
roundPrecision n x  = fromIntegral (floor (x * t)) / t
    where t = 10^n

{- | Statistical utility functions.
-}
mean :: Fractional a => [a] -> a
mean x = fst $ foldl' addElement (0,0) x
    where
      addElement (!m,!n) x = (m + (x-m)/(n+1), n+1)

covariance :: [Double] -> [Double] -> Double
covariance xs ys
  | n <= 1    = error "Util.covariance: List length must be > 1 "
  | otherwise = sum (zipWith (*) (map f1 xs) (map f2 ys)) / (n-1)
  where
    n = fromIntegral $ length xs
    m1 = mean xs
    m2 = mean ys
    f1 x = x - m1
    f2 x = x - m2

variance :: [Double] -> Double
variance xs
  | n <= 1    = error "Util.variance: List length must be > 1 "
  | otherwise = var' 0 0 0 xs / (n - 1)
  where
    n = fromIntegral $ length xs
    var' _ _ s [] = s
    var' m n s (x:xs) = var' nm (n + 1) (s + delta * (x - nm)) xs
      where
        delta = x - m
        nm = m + delta/(n + 1)