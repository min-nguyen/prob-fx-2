{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

{- | The effects for primitive distributions, sampling, and observing.
-}

module Effects.Dist (
  -- ** Address
  -- $Address
    Tag
  , Addr(..)
  -- ** Dist effect
  , Dist(..)
  , handleDist
  , module Effects.Sample
  , module Effects.Observe
  , module Effects.Param
  ) where

import           Data.Maybe ( fromMaybe )
import qualified Data.Map as Map
import           PrimDist
import           Trace
import           Comp ( call, discharge, weaken, Member(..), Comp(..), EffectSum )
import           Util
import           Effects.Sample
import           Effects.Observe
import           Effects.Param

-- | The effect @Dist@ for primitive distributions
data Dist a where
  Dist :: (Distribution d, a ~ Base d)
        => { getPrimDist :: d         -- ^ primitive distribution
           , getObs :: Maybe a        -- ^ optional observed value
           , getTag :: Maybe Tag      -- ^ optional observable variable name
           }
        -> Dist a

-- | Handle the @Dist@ effect to a @Sample@ or @Observe@ effect and assign an address
handleDist :: Comp (Dist : es) a -> Comp (Observe : Sample : es) a
handleDist = loop "" 0 Map.empty
  where
  loop :: String              -- ^ the tag of the previous @Dist@ operation
       -> Int                 -- ^ a counter for giving tags to unnamed @Dist@ operations
       -> Map.Map Tag Int     -- ^ a mapping from tags to their run-time occurrence
       -> Comp (Dist : es) a
       -> Comp (Observe : Sample : es) a
  loop _ _ _ (Val x) = return x
  loop prevTag counter tagMap (Op u k) = case discharge u of
    Right (Dist d maybe_y maybe_tag) ->
         case maybe_y of
              Just y  -> do call (Observe d y α) >>= k'
              Nothing -> do call (Sample d α)    >>= k'
          where α       = Addr tag tagIdx
                tag     = fromMaybe (show counter) maybe_tag
                tagIdx  = case Map.lookup tag tagMap of
                            Just currentIdx -> if tag /= prevTag then roundUp16 currentIdx else currentIdx
                            Nothing         -> 0
                tagMap' = Map.insert tag (tagIdx + 1) tagMap
                k'      = loop tag (counter + 1) tagMap' . k
    Left  u'  -> Op (weaken (weaken u')) (loop prevTag counter tagMap . k)

