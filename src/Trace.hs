{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Trace where

import Data.Map (Map)
import Data.Maybe
import Data.Proxy
import Effects.Dist
import PrimDist
import Env
import GHC.TypeLits
import OpenSum (OpenSum)
import qualified Data.Map as Map
import qualified OpenSum
import Prog
import Effects.State

-- ||| (Section 6.1) Sample trace, mapping addresses of sample/observe operations to their primitive distributions and sampled values
type STrace = Map Addr (ErasedPrimDist, OpenSum PrimVal)

type Trace a = [(a, STrace)]

-- | For converting sample traces, as used by simulation and inference, to output model environments
class FromSTrace a where
  fromSTrace :: STrace -> Env a

instance FromSTrace '[] where
  fromSTrace _ = nil

instance (UniqueKey x env ~ 'True, KnownSymbol x, Eq a, OpenSum.Member a PrimVal, FromSTrace env) => FromSTrace ((x := a) : env) where
  fromSTrace sMap = ECons (extractSamples (ObsVar @x, Proxy @a) sMap) (fromSTrace sMap)

extractSamples ::  forall a x. (Eq a, OpenSum.Member a PrimVal) => (ObsVar x, Proxy a) -> STrace -> [a]
extractSamples (x, typ)  =
    map (fromJust . OpenSum.prj @a . snd . snd)
  . Map.toList
  . Map.filterWithKey (\(tag, idx) _ -> tag == varToStr x)

-- | Trace sampled values for each Sample operation
traceSamples :: (Member Sample es) => Prog es a -> Prog es (a, STrace)
traceSamples = handleState Map.empty . storeSamples
  where storeSamples :: (Member Sample es) => Prog es a -> Prog (State STrace ': es) a
        storeSamples = install pure
          (\x tx k -> case tx of
              Sample d α -> case primDistDict d of
                Dict -> do updateSTrace α d x
                           k x
          )

updateSTrace :: Show x => (Member (State STrace) es, OpenSum.Member x PrimVal) => Addr -> PrimDist x -> x -> Prog es ()
updateSTrace α d x  = modify (Map.insert α (ErasedPrimDist d, OpenSum.inj x) :: STrace -> STrace)

-- ||| (Section 6.2.2) Log probability trace, mapping addresses of sample/observe operations to their log probabilities
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
            Samp d α
              -> Op (weaken u) (\x -> updateLPTrace α d x >>
                                      storeLPs (k x))
            Obs d y α
              -> Op (weaken u) (\x -> updateLPTrace α d x >> storeLPs (k x))
            _ -> Op (weaken u) (storeLPs . k)