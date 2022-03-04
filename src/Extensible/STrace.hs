{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Extensible.STrace where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import GHC.TypeLits
import Extensible.Dist
import Extensible.ModelEnv
import qualified Extensible.OpenSum as OpenSum
import Extensible.OpenSum (OpenSum)
import Util

type STrace = Map Addr (OpenSum PrimVal)

type Trace a = [(a, STrace)]

class FromSTrace a where
  fromSTrace :: STrace -> ModelEnv a

instance FromSTrace '[] where
  fromSTrace _ = nil

instance (UniqueKey x env ~ 'True, KnownSymbol x, Eq a, OpenSum.Member a PrimVal, FromSTrace env) => FromSTrace ((x := a) : env) where
  fromSTrace sMap = HCons (extractSamples (ObsVar @x, Proxy @a) sMap) (fromSTrace sMap)

extractSamples ::  forall a x. (Eq a, OpenSum.Member a PrimVal) => (ObsVar x, Proxy a) -> STrace -> [a]
extractSamples (x, typ)  =
    map (fromJust . OpenSum.prj @a . snd)
  . Map.toList
  . Map.filterWithKey (\(tag, idx) _ -> tag == varToStr x)