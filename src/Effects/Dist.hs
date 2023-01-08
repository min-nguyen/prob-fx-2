{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
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
  -- ** Sample effect
  , Sample(..)
  , pattern SampPrj
  , pattern SampDis
  -- ** Observe effect
  , Observe(..)
  , pattern ObsPrj
  , pattern ObsDis
  -- ** Param effect
  , Param(..)
  , pattern ParamPrj
  , setParamDist
  -- ** Param effect
  , Score(..)
  , pattern ScorePrj
  , module Effects.Sample
  , module Effects.Observe
  ) where

import           Data.Maybe ( fromMaybe )
import qualified Data.Map as Map
import           PrimDist
import           Trace
import           Prog ( call, discharge, weaken, Member(..), Prog(..), EffectSum )
import           Util
import           Effects.Sample
import           Effects.Observe

-- | The effect @Dist@ for primitive distributions
data Dist a where
  Dist :: (Distribution d, a ~ Base d)
        => { getPrimDist :: d         -- ^ primitive distribution
           , getObs :: Maybe a        -- ^ optional observed value
           , getTag :: Maybe Tag      -- ^ optional observable variable name
           }
        -> Dist a

-- | The effect @Param@ for distributions with support for gradient log-pdfs
data Param a where
  Param :: (DiffDistribution d, a ~ Base d)
         => d              -- ^ proposal distribution
         -> Addr           -- ^ address of operation
         -> Param a        -- ^ observed point

setParamDist :: (DiffDistribution d, a ~ Base d) => Param a -> d -> Param a
setParamDist (Param q α) q' = Param q' α

-- | For projecting and then successfully pattern matching against @Param@
pattern ParamPrj :: (Member Param es) => (DiffDistribution d, x ~ Base d) => d -> Addr -> EffectSum es x
pattern ParamPrj q α <- (prj -> Just (Param q α))

-- | The effect @Score@ is like @Param@ but retains the original distribution to be optimised
data Score a where
  Score  :: (DiffDistribution d, a ~ Base d)
         => d              -- ^ original distribution
         -> d              -- ^ proposal distribution
         -> Addr           -- ^ address of operation
         -> Score a        -- ^ observed point

-- | For projecting and then successfully pattern matching against @Score@
pattern ScorePrj :: (Member Score es) => (DiffDistribution d, x ~ Base d) => d -> d -> Addr -> EffectSum es x
pattern ScorePrj d q α <- (prj -> Just (Score d q α))

-- | Handle the @Dist@ effect to a @Sample@ or @Observe@ effect and assign an address
handleDist :: Prog (Dist : es) a -> Prog (Observe : Sample : es) a
handleDist = loop "" 0 Map.empty
  where
  loop :: String              -- ^ the tag of the previous @Dist@ operation
       -> Int                 -- ^ a counter for giving tags to unnamed @Dist@ operations
       -> Map.Map Tag Int     -- ^ a mapping from tags to their run-time occurrence
       -> Prog (Dist : es) a
       -> Prog (Observe : Sample : es) a
  loop _ _ _ (Val x) = return x
  loop prevTag counter tagMap (Op u k) = case discharge u of
    Right (Dist d maybe_y maybe_tag) ->
         case maybe_y of
              Just y  -> do call (Observe d y α) >>= k'
              Nothing -> do call (Sample d α)    >>= k'
          where α       = Addr counter tag tagIdx
                tag     = fromMaybe "" maybe_tag
                tagIdx  = case Map.lookup tag tagMap of
                            Just currentIdx -> if tag /= prevTag then roundUp16 currentIdx else currentIdx
                            Nothing         -> 0
                tagMap' = Map.insert tag (tagIdx + 1) tagMap
                k'      = loop tag (counter + 1) tagMap' . k
    Left  u'  -> Op (weaken (weaken u')) (loop prevTag counter tagMap . k)
