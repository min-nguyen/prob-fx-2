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
import Extensible.Freer
import Extensible.State
import qualified Extensible.OpenSum as OpenSum
import Extensible.OpenSum (OpenSum)
import Util

type STrace = Map Addr (OpenSum PrimVal)

class FromSTrace env where
  fromSTrace :: STrace -> ModelEnv env

instance FromSTrace '[] where
  fromSTrace _ = nil

instance (UniqueKey x env ~ 'True, KnownSymbol x, Eq a, OpenSum.Member a PrimVal, FromSTrace env) => FromSTrace ((x := a) : env) where
  fromSTrace sMap = HCons (extractSamples (ObsVar @x, Proxy @a) sMap) (fromSTrace sMap)

updateSTrace :: forall es x. (Member (State STrace) es, OpenSum.Member x PrimVal) => Addr -> x -> Prog es ()
updateSTrace α x = modify (Map.insert α (OpenSum.inj x) :: STrace -> STrace)

extractSamples ::  forall a x. (Eq a, OpenSum.Member a PrimVal) => (ObsVar x, Proxy a) -> STrace -> [a]
extractSamples (x, typ)  =
    map (fromJust . OpenSum.prj @a . snd)
  . Map.toList
  . Map.filterWithKey (\(tag, idx) _ -> tag == varToStr x)


type SDTrace  = Map Addr (PrimDist, OpenSum PrimVal)

fromSDTrace :: FromSTrace env => SDTrace -> ModelEnv env
fromSDTrace sdtrace = fromSTrace $ snd <$> sdtrace

-- instance (UniqueKey x env ~ 'True, KnownSymbol x, Eq a, OpenSum.Member a PrimVal, FromSTrace env) => FromSTrace ((x := a) : env) where
--   fromSTrace sMap = HCons (extractSamples (ObsVar @x, Proxy @a) sMap) (fromSTrace sMap)

-- updateSTrace :: forall es x. (Member (State STrace) es, OpenSum.Member x PrimVal) => Addr -> x -> Prog es ()
-- updateSTrace α x = modify (Map.insert α (OpenSum.inj x) :: STrace -> STrace)

-- extractSamples ::  forall a x. (Eq a, OpenSum.Member a PrimVal) => (ObsVar x, Proxy a) -> STrace -> [a]
-- extractSamples (x, typ)  =
--     map (fromJust . OpenSum.prj @a . snd)
--   . Map.toList
--   . Map.filterWithKey (\(tag, idx) _ -> tag == varToStr x)
