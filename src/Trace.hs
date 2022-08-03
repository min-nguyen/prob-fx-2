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
  , InvSTrace
  , FromSTrace(..)
  , filterTrace
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

{- | The type of generic traces, mapping addresses of probabilistic operations
     to some data.
-}
type Trace a = Map Addr a

-- | Retrieve the values for the specified observable variable names
filterTrace ::
  -- | observable variable names
    [Tag]
  -- | trace
  -> Map Addr a
  -- | filtered trace
  -> Map Addr a
filterTrace tags = Map.filterWithKey (\(tag, idx) _ -> tag `elem` tags)

-- | Update a sample trace at an address
updateTrace :: (Member (State (Map Addr v)) es) =>
  -- | address of sample site
     Addr
  -- | primitive distribution at address
  -- | sampled value
  -> v
  -> Prog es ()
updateTrace α v = modify (Map.insert α v)


{- | The type of inverse sample traces, mapping addresses of sample operations
     to the random values between 0 and 1 passed to their inverse CDF functions.
-}
type InvSTrace = Trace Double


{- | The type of sample traces, mapping addresses of sample operations
     to their primitive distributions and sampled values.
-}
type STrace = Trace (ErasedPrimDist, OpenSum PrimVal)

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

-- | Insert stateful operations for recording the sampled values at each @Sample@ operation
traceSamples :: (Member Sample es) => Prog es a -> Prog es (a, STrace)
traceSamples = handleState Map.empty . storeSamples
  where storeSamples :: (Member Sample es) => Prog es a -> Prog (State STrace ': es) a
        storeSamples = install pure
          (\x tx k -> case tx of
              Sample (PrimDistPrf d) α -> do updateTrace α (ErasedPrimDist d, OpenSum.inj x :: OpenSum PrimVal)
                                             k x
          )


{- | The type of log-probability traces, mapping addresses of sample/observe operations
     to their log probabilities.
-}
type LPTrace = Trace LogP

-- | Insert stateful operations for recording the log-probabilities at each @Sample@ or @Observe@ operation
traceLogProbs :: (Member Sample es, Member Observe es) => Prog es a -> Prog es (a, LPTrace)
traceLogProbs = handleState Map.empty . storeLPs
  where storeLPs :: (Member Sample es, Member Observe es) => Prog es a -> Prog (State LPTrace: es) a
        storeLPs (Val x) = pure x
        storeLPs (Op u k) = do
          case u of
            SampPrj d α
              -> Op (weaken u) (\x -> updateTrace α (PrimDist.logProb d x) >> storeLPs (k x))
            ObsPrj d y α
              -> Op (weaken u) (\x -> updateTrace α (PrimDist.logProb d x) >> storeLPs (k x))
            _ -> Op (weaken u) (storeLPs . k)