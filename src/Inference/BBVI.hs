{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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
import PrimDist ( sample, logProb, gradLogProb, pattern TypeableDistPrf )
import Prog ( discharge, Prog(..) )
import Sampler ( Sampler )
import Trace



handleSamp :: DTrace -> GTrace -> LogP -> Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
handleSamp dtrace gtrace logW (Val x)   = return x
handleSamp dtrace gtrace logW (Op op k) = case discharge op of
  Right (SampleTypeable d α) -> do
    let (q, dtrace') = lookupOrInsert (Key α) d dtrace
    x <- lift $ sample q
    let gtrace'      = insert (Key α) (gradLogProb q x) gtrace
        logW'        = logW + logProb d x - logProb q x
    handleSamp dtrace' gtrace' logW' (k x)
  Left op' -> do
     Op op' (handleSamp dtrace gtrace logW . k)

optimizerStep :: DTrace -> GTrace -> DTrace
optimizerStep dtrace gtrace = do

  undefined