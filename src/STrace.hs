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

module STrace where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import GHC.TypeLits
import Dist
import ModelEnv
import Freer
import State
import qualified OpenSum as OpenSum
import OpenSum (OpenSum)
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

updateSDTrace :: Show x => (Member (State SDTrace) es, OpenSum.Member x PrimVal)
  => Addr -> Dist x -> x -> Prog es ()
updateSDTrace α d x  = modify (Map.insert α (PrimDist d, OpenSum.inj x) :: SDTrace -> SDTrace)

fromSDTrace :: FromSTrace env => SDTrace -> ModelEnv env
fromSDTrace sdtrace = fromSTrace $ snd <$> sdtrace

type LPTrace = Map Addr Double

updateLPTrace :: (Member (State LPTrace) es) => Addr -> Dist x -> x -> Prog es ()
updateLPTrace α d x  = modify (Map.insert α (logProb d x) :: LPTrace -> LPTrace)

-- instance (UniqueKey x env ~ 'True, KnownSymbol x, Eq a, OpenSum.Member a PrimVal, FromSTrace env) => FromSTrace ((x := a) : env) where
--   fromSTrace sMap = HCons (extractSamples (ObsVar @x, Proxy @a) sMap) (fromSTrace sMap)

-- updateSTrace :: forall es x. (Member (State STrace) es, OpenSum.Member x PrimVal) => Addr -> x -> Prog es ()
-- updateSTrace α x = modify (Map.insert α (OpenSum.inj x) :: STrace -> STrace)

-- extractSamples ::  forall a x. (Eq a, OpenSum.Member a PrimVal) => (ObsVar x, Proxy a) -> STrace -> [a]
-- extractSamples (x, typ)  =
--     map (fromJust . OpenSum.prj @a . snd)
--   . Map.toList
--   . Map.filterWithKey (\(tag, idx) _ -> tag == varToStr x)
