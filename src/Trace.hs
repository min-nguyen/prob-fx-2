{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{- | For recording samples and log-probabilities during model execution.
-}

module Trace (
  -- * Sample trace
    STrace
  , FromSTrace(..)
  , filterSTrace
  , traceSamples
  -- * Log-probability trace
  , LPTrace
  , traceLogProbs) where

import Data.Map (Map)
import Data.Maybe ( fromJust )
import Data.Proxy ( Proxy(..) )
import Effects.Dist ( Tag, Addr, Observe, Sample(..), pattern ObsPrj, pattern SampPrj )
import Env ( enil, varToStr, UniqueVar, Var(..), Env(ECons), Assign((:=)) )
import GHC.TypeLits ( KnownSymbol )
import OpenSum (OpenSum)
import LogP
import qualified Data.Map as Map
import qualified OpenSum
import Prog ( Member, Prog(..), weaken, install )
import PrimDist ( ErasedPrimDist(..), PrimVal, PrimDist, logProb, pattern PrimDistPrf )
import Effects.State ( State, modify, handleState )

{- | The type of sample traces, mapping addresses of sample/observe operations
     to their primitive distributions and sampled values.
-}
type STrace = Map Addr (ErasedPrimDist, OpenSum PrimVal)

-- | For converting sample traces to model environments
class FromSTrace a where
  -- | Convert a sample trace to a model environment
  fromSTrace :: STrace -> Env a

instance FromSTrace '[] where
  fromSTrace _ = enil

instance (UniqueVar x env ~ 'True, KnownSymbol x, Eq a, OpenSum.Member a PrimVal, FromSTrace env) => FromSTrace ((x := a) : env) where
  fromSTrace sMap = ECons (extractSTrace (Var @x, Proxy @a) sMap) (fromSTrace sMap)
    where extractSTrace ::  forall a x. (Eq a, OpenSum.Member a PrimVal) => (Var x, Proxy a) -> STrace -> [a]
          extractSTrace (x, typ)  =
              map (fromJust . OpenSum.prj @a . snd . snd)
            . Map.toList
            . Map.filterWithKey (\(tag, idx) _ -> tag == varToStr x)

-- | Retrieve the sampled values for the specified observable variable names
filterSTrace ::
  -- | observable variable names
    [Tag]
  -- | sample trace
  -> STrace
  -- | filtered sample trace
  -> STrace
filterSTrace tags = Map.filterWithKey (\(tag, idx) _ -> tag `elem` tags)

-- | Update a sample trace at an address
updateSTrace :: (Show x, Member (State STrace) es, OpenSum.Member x PrimVal) =>
  -- | address of sample site
     Addr
  -- | primitive distribution at address
  -> PrimDist x
  -- | sampled value
  -> x
  -> Prog es ()
updateSTrace α d x  = modify (Map.insert α (ErasedPrimDist d, OpenSum.inj x) :: STrace -> STrace)

-- | Insert stateful operations for recording the sampled values at each @Sample@ operation
traceSamples :: (Member Sample es) => Prog es a -> Prog es (a, STrace)
traceSamples = handleState Map.empty . storeSamples
  where storeSamples :: (Member Sample es) => Prog es a -> Prog (State STrace ': es) a
        storeSamples = install pure
          (\x tx k -> case tx of
              Sample (PrimDistPrf d) α -> do updateSTrace α d x
                                             k x
          )

{- | The type of log-probability traces, mapping addresses of sample/observe operations
     to their log probabilities.
-}
type LPTrace = Map Addr LogP

-- | Compute and update a log-probability trace at an address
updateLPTrace :: (Member (State LPTrace) es) =>
  -- | address of sample/observe site
    Addr
  -- | primitive distribution at address
  -> PrimDist x
  -- | sampled or observed value
  -> x
  -> Prog es ()
updateLPTrace α d x  = modify (Map.insert α (PrimDist.logProb d x) :: LPTrace -> LPTrace)

-- | Insert stateful operations for recording the log-probabilities at each @Sample@ or @Observe@ operation
traceLogProbs :: (Member Sample es, Member Observe es) => Prog es a -> Prog es (a, LPTrace)
traceLogProbs = handleState Map.empty . storeLPs
  where storeLPs :: (Member Sample es, Member Observe es) => Prog es a -> Prog (State LPTrace: es) a
        storeLPs (Val x) = pure x
        storeLPs (Op u k) = do
          case u of
            SampPrj d α
              -> Op (weaken u) (\x -> updateLPTrace α d x >>
                                      storeLPs (k x))
            ObsPrj d y α
              -> Op (weaken u) (\x -> updateLPTrace α d x >> storeLPs (k x))
            _ -> Op (weaken u) (storeLPs . k)