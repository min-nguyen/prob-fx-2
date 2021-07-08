{-# LANGUAGE GADTs #-}

module Util where

import qualified Data.Map as Map
import Data.Map (Map)

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

maybeHead :: [a] -> Maybe a
maybeHead []     = Nothing
maybeHead (x:xs) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

data Address =
  Address { -- The address of the previous sample statement
            prevSamp  :: String,
            -- Mapping of sample addresses to their number of occurrences
            samples   :: Map String Int,
            -- Mapping of observe addresses to their number of occurrences
            observes  :: Map String Int } deriving Show
