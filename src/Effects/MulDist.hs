{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

{- | The effects for primitive distributions, sampling, and observing.
-}

module Effects.MulDist (
  -- ** Address
  -- $Address
    Tag
  , Addr(..)
  -- ** MulDist effect
  , MulDist(..)
  , handleMulDist
  , module Effects.Sample
  , module Effects.Observe
  ) where

import           Data.Maybe ( fromMaybe )
import qualified Data.Map as Map
import           PrimDist
import           Trace
import           Comp ( call, discharge, weaken, Member(..), Comp(..), EffectSum )
import           Util
import           Effects.Sample
import           Effects.Observe

-- | The effect @MulDist@ for primitive distributions
data MulDist a where
  MulDist :: (Distribution d, a ~ Base d)
        => { getPrimDist :: d         -- ^ primitive distribution
           , getObs :: Maybe a        -- ^ optional observed value
           , getTag :: Maybe Tag      -- ^ optional observable variable name
           }
        -> MulDist a

-- | Handle the @MulDist@ effect to a @Sample@ or @Observe@ effect and assign an address
handleMulDist :: Comp (MulDist : es) a -> Comp (Observe : Sample : es) a
handleMulDist = loop "" 0 Map.empty
  where
  loop :: String              -- ^ the tag of the previous @MulDist@ operation
       -> Int                 -- ^ a counter for giving tags to unnamed @MulDist@ operations
       -> Map.Map Tag Int     -- ^ a mapping from tags to their run-time occurrence
       -> Comp (MulDist : es) a
       -> Comp (Observe : Sample : es) a
  loop _ _ _ (Val x) = return x
  loop prevTag counter tagMap (Op u k) = case discharge u of
    Right (MulDist d maybe_y maybe_tag) ->
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

