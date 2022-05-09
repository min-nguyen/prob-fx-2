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

module Old.Trace where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import GHC.TypeLits
import Effects.Dist
import Env
import Prog
import Effects.State
import qualified OpenSum as OpenSum
import OpenSum (OpenSum)
import Util

type STrace = Map Addr (OpenSum PrimVal)

class FromSTrace env where
  fromSTrace :: STrace -> Env env

instance FromSTrace '[] where
  fromSTrace _ = ENil

instance (UniqueKey x env ~ 'True, KnownSymbol x, Eq a, OpenSum.Member a PrimVal, FromSTrace env) => FromSTrace ((x := a) : env) where
  fromSTrace sMap = ECons (extractSamples (ObsVar @x, Proxy @a) sMap) (fromSTrace sMap)

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

filterSDTrace :: [Tag] -> SDTrace -> SDTrace
filterSDTrace tags strace = Map.filterWithKey (\(tag, idx) _ -> tag `elem` tags) strace

fromSDTrace :: FromSTrace env => SDTrace -> Env env
fromSDTrace sdtrace = fromSTrace $ snd <$> sdtrace

type LPTrace = Map Addr Double

updateLPTrace :: (Member (State LPTrace) es) => Addr -> Dist x -> x -> Prog es ()
updateLPTrace α d x  = modify (Map.insert α (logProb d x) :: LPTrace -> LPTrace)

traceSamples :: (Member Sample es) => Prog es a -> Prog es (a, STrace)
traceSamples = handleState Map.empty . storeSamples
  where storeSamples :: (Member Sample es) => Prog es a -> Prog (State STrace ': es) a
        storeSamples = install return
          (\x tx k -> case tx of
              Sample d α -> case distDict d of
                Dict -> do updateSTrace α x
                           k x
              Printer s  -> k ()
          )

traceDSamples :: (Member Sample es) => Prog es a -> Prog es (a, SDTrace)
traceDSamples = handleState Map.empty . storeSamples
  where storeSamples :: (Member Sample es) => Prog es a -> Prog (State SDTrace ': es) a
        storeSamples = install return
          (\x tx k -> case tx of
              Sample d α -> case distDict d of
                Dict -> do updateSDTrace α d x
                           k x
              Printer s  -> k ()
          )
-- | Insert stateful operations for SDTrace and LPTrace when either Sample or Observe occur.
traceLPs ::(Member Sample es, Member Observe es) => Prog es a -> Prog es (a, LPTrace)
traceLPs = handleState Map.empty . storeLPs
  where storeLPs :: (Member Sample es, Member Observe es) => Prog es a -> Prog (State LPTrace: es) a
        storeLPs (Val x) = return x
        storeLPs (Op u k) = do
          case u of
            SampPatt d α
              -> Op (weaken u) (\x -> updateLPTrace α d x >>
                                      storeLPs (k x))
            ObsPatt d y α
              -> Op (weaken u) (\x -> updateLPTrace α d x >> storeLPs (k x))
            _ -> Op (weaken u) (storeLPs . k)

-- instance (UniqueKey x env ~ 'True, KnownSymbol x, Eq a, OpenSum.Member a PrimVal, FromSTrace env) => FromSTrace ((x := a) : env) where
--   fromSTrace sMap = HCons (extractSamples (ObsVar @x, Proxy @a) sMap) (fromSTrace sMap)

-- updateSTrace :: forall es x. (Member (State Trace) es, OpenSum.Member x PrimVal) => Addr -> x -> Prog es ()
-- updateSTrace α x = modify (Map.insert α (OpenSum.inj x) :: Trace -> Trace)

-- extractSamples ::  forall a x. (Eq a, OpenSum.Member a PrimVal) => (ObsVar x, Proxy a) -> Trace -> [a]
-- extractSamples (x, typ)  =
--     map (fromJust . OpenSum.prj @a . snd)
--   . Map.toList
--   . Map.filterWithKey (\(tag, idx) _ -> tag == varToStr x)
