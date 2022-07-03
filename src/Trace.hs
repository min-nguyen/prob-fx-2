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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Trace where

import Data.Map (Map)
import Data.Maybe
import Data.Proxy
import Effects.Dist as Dist
import Effects.State
import Env
import GHC.TypeLits
import OpenSum (OpenSum)
import PrimDist
import Prog
import qualified Data.Map as Map
import qualified OpenSum 

-- | Sample trace 
type STrace  = Map Addr (ErasedPrimDist, OpenSum PrimVal)

class FromSTrace (env :: [Assign Symbol *]) where
  fromSTrace    :: STrace -> Env env

instance FromSTrace '[] where
  fromSTrace _       = ENil

instance (UniqueKey x env ~ 'True, KnownSymbol x, Eq a, OpenSum.Member a PrimVal, FromSTrace env) => FromSTrace ((x := a) : env) where
  fromSTrace sMap    = ECons (extractSTrace (ObsVar @x, Proxy @a) sMap) (fromSTrace sMap)

extractSTrace ::  forall a x. (Eq a, OpenSum.Member a PrimVal) => (ObsVar x, Proxy a) -> STrace -> [a]
extractSTrace (x, typ)  =
    map (fromJust . OpenSum.prj @a . snd . snd)
  . Map.toList
  . Map.filterWithKey (\(tag, idx) _ -> tag == varToStr x)

filterSTrace :: [Tag] -> STrace -> STrace
filterSTrace tags = Map.filterWithKey (\(tag, idx) _ -> tag `elem` tags)

updateSTrace :: Show x => (Member (State STrace) es, OpenSum.Member x PrimVal) => Addr -> PrimDist x -> x -> Prog es ()
updateSTrace α d x  = modify (Map.insert α (ErasedPrimDist d, OpenSum.inj x) :: STrace -> STrace)

-- | Insert stateful operations for STrace when Sample occurs.
traceSamples :: (Member Sample es) => Prog es a -> Prog es (a, STrace)
traceSamples = handleState Map.empty . storeSamples
  where storeSamples :: (Member Sample es) => Prog es a -> Prog (State STrace ': es) a
        storeSamples = install pure
          (\x tx k -> case tx of
              Sample d α -> case primDistDict d of
                Dict -> do updateSTrace α d x
                           k x
              Printer s  -> k ()
          )

{- Log probability trace -}
type LPTrace = Map Addr Double

updateLPTrace :: (Member (State LPTrace) es) => Addr -> PrimDist x -> x -> Prog es ()
updateLPTrace α d x  = modify (Map.insert α (PrimDist.logProb d x) :: LPTrace -> LPTrace)

-- | Insert stateful operations for LPTrace when either Sample or Observe occur.
traceLPs ::(Member Sample es, Member Observe es) => Prog es a -> Prog es (a, LPTrace)
traceLPs = handleState Map.empty . storeLPs
  where storeLPs :: (Member Sample es, Member Observe es) => Prog es a -> Prog (State LPTrace: es) a
        storeLPs (Val x) = pure x
        storeLPs (Op u k) = do
          case u of
            SampPatt d α
              -> Op (weaken u) (\x -> updateLPTrace α d x >>
                                      storeLPs (k x))
            ObsPatt d y α
              -> Op (weaken u) (\x -> updateLPTrace α d x >> storeLPs (k x))
            _ -> Op (weaken u) (storeLPs . k)