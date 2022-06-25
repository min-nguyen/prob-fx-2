{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module Util where

import Control.Monad
import Control.Applicative
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List

data Proxy p = Proxy


replicateM2 :: Applicative m => Int -> Int -> m a -> m [[a]]
replicateM2 n m = replicateM n . replicateM m

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldr (\x seen -> if x `elem` seen then seen else x : seen) []

findIndexes :: Eq a => [a] -> a -> [Int]
findIndexes xs a = reverse $ go xs 0 []
  where
  go (x:xs) i inds = if x == a then go xs (i + 1) (i:inds)
                     else go xs (i + 1) inds
  go [] i inds = inds

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

maybeHead :: [a] -> Maybe a
maybeHead []     = Nothing
maybeHead (x:xs) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

dotProd :: [[Double]] -> [[Double]] -> [[Double]]
dotProd xss yss =
  let ys_T = transpose yss
      dot xs1 xs2 = sum $ zipWith (*) xs1 xs2
  in  [map (dot xs) ys_T | xs <- xss]

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

mapfst3 :: (a -> d) -> (a, b, c) -> (d, b, c)
mapfst3 f (a, c, d) = (f a, c, d)

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

mapsnd3 :: (b -> d) -> (a, b, c) -> (a, d, c)
mapsnd3 f (a, b, c) = (a, f b, c)

thrd3 :: (a, b, c) -> c
thrd3 (_, _, c) = c

mapthrd3 :: (c -> d) -> (a, b, c) -> (a, b, d)
mapthrd3 f (a, b, c) = (a, b, f c)

fstsnd3 :: (a, b, c) -> (a, b)
fstsnd3 (a, b, c) = (a, b)

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 = map . map

ones :: Int -> Int -> [[Int]]
ones m n = replicate m $ replicate n 1

untuple3 :: [((a, b), c)] -> [(a, b, c)]
untuple3 = map (\((a, b), c) -> (a, b, c))

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

data Address =
  Address { -- The address of the previous sample statement
            prevSamp  :: String,
            -- Mapping of sample addresses to their number of occurrences
            samples   :: Map String Int,
            -- Mapping of observe addresses to their number of occurrences
            observes  :: Map String Int } deriving Show
