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
module Effects.Dist where

import Data.Kind
import Data.Map (Map)
import Data.Maybe
import Numeric.Log
import Prog ( call, discharge, Member, Prog(..) )
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified OpenSum as OpenSum
import Sampler
import Statistics.Distribution
import Statistics.Distribution.Beta
import Statistics.Distribution.Binomial
import Statistics.Distribution.CauchyLorentz
import Statistics.Distribution.Dirichlet
import Statistics.Distribution.DiscreteUniform
import Statistics.Distribution.Gamma
import Statistics.Distribution.Normal
import Statistics.Distribution.Poisson
import Statistics.Distribution.Uniform
import Util ( boolToInt )
import PrimDist
import Prog

-- ||| Distribution effect
data Dist a = 
  Dist { getPrimDist :: PrimDist a  -- ^ Primitive distribution
       , getObs :: Maybe a          -- ^ Optional observed value
       , getTag :: Maybe String     -- ^ Optional observable variable name
       }

-- ||| Sample effect
data Sample a where
  Sample  :: PrimDist a -> Addr -> Sample a

pattern SampPrj :: (Member Sample es) => (Show x, OpenSum.Member x PrimVal) => PrimDist x -> Addr -> EffectSum es x
pattern SampPrj d α <- (prj -> Just (Sample (PrimDistDict d) α))

pattern SampDis :: (Show x, OpenSum.Member x PrimVal) => PrimDist x -> Addr -> EffectSum (Sample : es) x
pattern SampDis d α <- (discharge -> Right (Sample (PrimDistDict d) α))

-- ||| Observe effect
data Observe a where
  Observe :: PrimDist a -> a -> Addr -> Observe a

pattern ObsPrj :: (Member Observe es) => (Show x, OpenSum.Member x PrimVal) => PrimDist x -> x -> Addr -> EffectSum es x
pattern ObsPrj d y α <- (prj -> Just (Observe (PrimDistDict d) y α))

pattern ObsDis :: (Show x, OpenSum.Member x PrimVal) => PrimDist x -> x -> Addr -> EffectSum (Observe : es) x
pattern ObsDis d y α <- (discharge -> Right (Observe (PrimDistDict d) y α))

-- ||| Handle Dist to Sample or Observe, and add address
type Tag    = String
type Addr   = (Tag, Int)
type TagMap = Map Tag Int

handleDist :: Prog (Dist : es) a -> Prog (Observe : Sample : es) a
handleDist = loop 0 Map.empty
  where
  loop :: Int -> TagMap -> Prog (Dist : es) a -> Prog (Observe : Sample : es) a
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

