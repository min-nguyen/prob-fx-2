{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs, TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}

{- | The effect for primitive distributions 
-}

module Effects.Dist (
  -- ** Address
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

import Data.Map (Map)
import Data.Maybe ( fromMaybe )
import Prog ( call, discharge, weaken, Member(..), Prog(..), EffectSum )
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified OpenSum
import Util ( boolToInt )
import PrimDist ( PrimVal, PrimDist, pattern PrimDistPrf)

-- | An observable variable name assigned to a primitive distribution
type Tag  = String
-- | An observable variable name and the index of its run-time occurrence
type Addr = (Tag, Int)

-- | The Dist effect
data Dist a = 
  Dist { getPrimDist :: PrimDist a  -- ^ Primitive distribution
       , getObs :: Maybe a          -- ^ Optional observed value
       , getTag :: Maybe String     -- ^ Optional observable variable name
       }

-- | An effect for sampling from distirbutions
data Sample a where
  Sample  :: PrimDist a -> Addr -> Sample a

-- | For projecting and then successfully pattern matching against @Sample@
pattern SampPrj :: (Member Sample es) => (Show x, OpenSum.Member x PrimVal) => PrimDist x -> Addr -> EffectSum es x
pattern SampPrj d α <- (prj -> Just (Sample (PrimDistPrf d) α))

-- | For discharging and then successfully pattern matching against @Sample@
pattern SampDis :: (Show x, OpenSum.Member x PrimVal) => PrimDist x -> Addr -> EffectSum (Sample : es) x
pattern SampDis d α <- (discharge -> Right (Sample (PrimDistPrf d) α))

-- | An effect for conditioning against observed values
data Observe a where
  Observe :: PrimDist a -> a -> Addr -> Observe a

-- | For projecting and then successfully pattern matching against @Observe@
pattern ObsPrj :: (Member Observe es) => (Show x, OpenSum.Member x PrimVal) => PrimDist x -> x -> Addr -> EffectSum es x
pattern ObsPrj d y α <- (prj -> Just (Observe (PrimDistPrf d) y α))

-- | For discharging and then successfully pattern matching against @Observe@
pattern ObsDis :: (Show x, OpenSum.Member x PrimVal) => PrimDist x -> x -> Addr -> EffectSum (Observe : es) x
pattern ObsDis d y α <- (discharge -> Right (Observe (PrimDistPrf d) y α))

-- | Handle the @Dist@ effect to a @Sample@ or @Observe@ effect and assign address
handleDist :: Prog (Dist : es) a -> Prog (Observe : Sample : es) a
handleDist = loop 0 Map.empty
  where
  loop :: Int ->  Map Tag Int -> Prog (Dist : es) a -> Prog (Observe : Sample : es) a
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

