{-# LANGUAGE GADTs #-}

module Util where

import qualified Data.Map as Map
import Data.Map (Map)

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

data Address = 
  Address { -- The address of the previous sample statement
            prevSamp  :: String, 
            -- Mapping of sample addresses to their number of occurrences
            samples   :: Map String Int,
            -- Mapping of observe addresses to their number of occurrences 
            observes  :: Map String Int } deriving Show
