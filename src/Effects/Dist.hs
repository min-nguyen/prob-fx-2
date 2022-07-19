{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | The effects for primitive distributions, sampling, and observing.
-}

module Effects.Dist (
  -- ** Address
  -- $Address
    Tag
  , Addr
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
  ) where

import Data.Maybe ( fromMaybe )
import qualified Data.Map as Map
import PrimDist ( PrimVal, PrimDist, pattern PrimDistPrf)
import Prog ( call, discharge, weaken, Member(..), Prog(..), EffectSum )
import qualified OpenSum

{- $Address
   Run-time identifiers for probabilistic operations
-}

-- | An observable variable name assigned to a primitive distribution, representing a compile-time identifier
type Tag  = String
-- | An observable variable name and the index of its run-time occurrence, representing a run-time identifier
type Addr = (Tag, Int)

-- | The effect @Dist@ for primitive distributions
data Dist a = Dist
  { getPrimDist :: PrimDist a  -- ^ primitive distribution
  , getObs :: Maybe a          -- ^ optional observed value
  , getTag :: Maybe Tag        -- ^ optional observable variable name
  }

-- | The effect @Sample@ for sampling from distirbutions
data Sample a where
  Sample  :: PrimDist a     -- ^ distribution to sample from
          -> Addr           -- ^ address of @Sample@ operation
          -> Sample a

-- | For projecting and then successfully pattern matching against @Sample@
pattern SampPrj :: (Member Sample es) => (Show x, OpenSum.Member x PrimVal) => PrimDist x -> Addr -> EffectSum es x
pattern SampPrj d α <- (prj -> Just (Sample (PrimDistPrf d) α))

-- | For discharging and then successfully pattern matching against @Sample@
pattern SampDis :: (Show x, OpenSum.Member x PrimVal) => PrimDist x -> Addr -> EffectSum (Sample : es) x
pattern SampDis d α <- (discharge -> Right (Sample (PrimDistPrf d) α))

-- | The effect @Observe@ for conditioning against observed values
data Observe a where
  Observe :: PrimDist a     -- ^ distribution to condition with
          -> a              -- ^ observed value
          -> Addr           -- ^ address of @Observe@ operation
          -> Observe a

-- | For projecting and then successfully pattern matching against @Observe@
pattern ObsPrj :: (Member Observe es) => (Show x, OpenSum.Member x PrimVal) => PrimDist x -> x -> Addr -> EffectSum es x
pattern ObsPrj d y α <- (prj -> Just (Observe (PrimDistPrf d) y α))

-- | For discharging and then successfully pattern matching against @Observe@
pattern ObsDis :: (Show x, OpenSum.Member x PrimVal) => PrimDist x -> x -> Addr -> EffectSum (Observe : es) x
pattern ObsDis d y α <- (discharge -> Right (Observe (PrimDistPrf d) y α))

-- | Handle the @Dist@ effect to a @Sample@ or @Observe@ effect and assign an address
handleDist :: Prog (Dist : es) a -> Prog (Observe : Sample : es) a
handleDist = loop 0 Map.empty
  where
  loop :: Int -> Map.Map Tag Int -> Prog (Dist : es) a -> Prog (Observe : Sample : es) a
  loop _ _ (Val x) = return x
  loop counter tagMap (Op u k) = case discharge u of
    Right (Dist d maybe_y maybe_tag) ->
         case maybe_y of
              Just y  -> do call (Observe d y (tag, tagIdx)) >>= k'
              Nothing -> do call (Sample d (tag, tagIdx))    >>= k'
          where tag     = fromMaybe (show counter) maybe_tag
                tagIdx  = Map.findWithDefault 0 tag tagMap
                tagMap' = Map.insert tag (tagIdx + 1) tagMap
                k'      = loop (counter + 1) tagMap' . k
    Left  u'  -> Op (weaken (weaken u')) (loop counter tagMap . k)

