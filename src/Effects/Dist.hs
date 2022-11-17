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
  -- ** Learn effect
  , Learn(..)
  , pattern LearnSPrj
  , pattern LearnOPrj
  , setLearnDist
  -- ** Learn effect
  , Score(..)
  , pattern ScorePrj
  ) where

import           Data.Maybe ( fromMaybe )
import qualified Data.Map as Map
import           PrimDist
import           Prog ( call, discharge, weaken, Member(..), Prog(..), EffectSum )
import           Util

{- $Address
   Identifiers for probabilistic operations.
-}

-- | An observable variable name assigned to a primitive distribution, representing a compile-time identifier
type Tag  = String
-- | An observable variable name and the index of its run-time occurrence, representing a run-time identifier
type Addr = (Tag, Int)

instance {-# OVERLAPPING #-} Show (String, Int) where
  show :: (String, Int) -> String
  show (x, n) = "⟨" ++ x ++ "," ++ show n ++ "⟩"

-- | The effect @Dist@ for primitive distributions
data Dist a where
  Dist :: (Distribution d, a ~ Support d)
        => { getPrimDist :: d         -- ^ primitive distribution
           , getObs :: Maybe a        -- ^ optional observed value
           , getTag :: Maybe Tag      -- ^ optional observable variable name
           }
        -> Dist a

-- | The effect @Sample@ for sampling from distirbutions
data Sample a where
  Sample  :: (Distribution d, a ~ Support d)
          => d              -- ^ distribution to sample from
          -> Addr           -- ^ address of @Sample@ operation
          -> Sample a

-- | For projecting and then successfully pattern matching against @Sample@
pattern SampPrj :: (Member Sample es) => (Distribution d, x ~ Support d) => d -> Addr -> EffectSum es x
pattern SampPrj d α <- (prj -> Just (Sample d α))

-- | For discharging and then successfully pattern matching against @Sample@
pattern SampDis :: (Show x) =>  (Distribution d, x ~ Support d) => d -> Addr -> EffectSum (Sample : es) x
pattern SampDis d α <- (discharge -> Right (Sample d α))

-- | The effect @Observe@ for conditioning against observed values
data Observe a where
  Observe :: (Distribution d, a ~ Support d)
          => d              -- ^ distribution to condition with
          -> a              -- ^ observed value
          -> Addr           -- ^ address of @Observe@ operation
          -> Observe a

-- | For projecting and then successfully pattern matching against @Observe@
pattern ObsPrj :: (Member Observe es) => (Distribution d, x ~ Support d) => d -> x -> Addr -> EffectSum es x
pattern ObsPrj d y α <- (prj -> Just (Observe d y α))

-- | For discharging and then successfully pattern matching against @Observe@
pattern ObsDis :: () => (Distribution d, x ~ Support d) => d -> x -> Addr -> EffectSum (Observe : es) x
pattern ObsDis d y α <- (discharge -> Right (Observe d y α))

-- | The effect @Learn@ for distributions with support for gradient log-pdfs
data Learn a where
  LearnS :: (DiffDistribution d, a ~ Support d)
         => d              -- ^ proposal distribution
         -> Addr           -- ^ address of operation
         -> Learn a        -- ^ observed point
  LearnO :: (DiffDistribution d, a ~ Support d)
         => d              -- ^ proposal distribution
         -> a              -- ^ an observed point
         -> Addr           -- ^ address of operation
         -> Learn a        -- ^ observed point

setLearnDist :: (DiffDistribution d, a ~ Support d) => Learn a -> d -> Learn a
setLearnDist (LearnS q α) q' = LearnS q' α
setLearnDist (LearnO q x α) q' = LearnO q' x α

-- | For projecting and then successfully pattern matching against @Learn@
pattern LearnSPrj :: (Member Learn es) => (DiffDistribution d, x ~ Support d) => d -> Addr -> EffectSum es x
pattern LearnSPrj q α <- (prj -> Just (LearnS q α))

pattern LearnOPrj :: (Member Learn es) => (DiffDistribution d, x ~ Support d) => d -> a -> Addr -> EffectSum es x
pattern LearnOPrj q x α <- (prj -> Just (LearnO q x α))

-- | The effect @Score@ is like @Learn@ but retains the original distribution to be optimised
data Score a where
  Score  :: (DiffDistribution d, a ~ Support d)
         => d              -- ^ original distribution
         -> d              -- ^ proposal distribution
         -> Addr           -- ^ address of operation
         -> Score a        -- ^ observed point

-- | For projecting and then successfully pattern matching against @Score@
pattern ScorePrj :: (Member Score es) => (DiffDistribution d, x ~ Support d) => d -> d -> Addr -> EffectSum es x
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
              Just y  -> do call (Observe d y (tag, tagIdx)) >>= k'
              Nothing -> do call (Sample d (tag, tagIdx))    >>= k'
          where tag     = fromMaybe (show counter) maybe_tag
                tagIdx  = case Map.lookup tag tagMap of
                            Just currentIdx -> if tag /= prevTag then roundUp16 currentIdx else currentIdx
                            Nothing         -> 0
                tagMap' = Map.insert tag (tagIdx + 1) tagMap
                k'      = loop tag (counter + 1) tagMap' . k
    Left  u'  -> Op (weaken (weaken u')) (loop prevTag counter tagMap . k)
