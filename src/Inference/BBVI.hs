{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Likelihood-Weighting inference.
-}

module Inference.BBVI
  where

import Data.Bifunctor ( Bifunctor(first) )
import Control.Monad ( replicateM )
import Effects.Dist
import Effects.Lift ( handleLift, Lift, lift )
import Effects.ObsRW ( ObsRW )
import Effects.State ( modify, handleState, State )
import Env ( Env )
import LogP ( LogP(unLogP) )
import Model ( handleCore, Model )
import PrimDist ( sample, pattern TypeableDistPrf )
import Prog ( discharge, Prog(..) )
import Sampler ( Sampler )
import Trace

handleSamp :: DTrace -> Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
handleSamp dtrace (Val x) = return x
handleSamp dtrace (Op op k) = case discharge op of
  Right (SampleTypeable d α) -> do
    let (q, dtrace') = lookupOrInsert (Key α) d dtrace
    y <- lift $ sample q
    undefined
  Left op' -> do
     Op op' (handleSamp dtrace . k)