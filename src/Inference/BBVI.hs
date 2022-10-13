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
import PrimDist
import Prog ( discharge, Prog(..) )
import Sampler ( Sampler )
import Trace

handleSamp :: DTrace -> GTrace -> LogP -> Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] ((DTrace, GTrace, LogP), a)
handleSamp qTrace gradTrace logW (Val x)   = return ((qTrace, gradTrace, logW), x)
handleSamp qTrace gradTrace logW (Op op k) = case discharge op of
  Right (Sample d α) -> do
    (x, q, qTrace', gradTrace') <- case isDifferentiable d of
          Nothing   -> do x <- lift (sample d)
                          pure (x, d, qTrace, gradTrace)
          Just Dict -> do -- | Get the recently proposed distribution, else initialise with prior
                          let (q, qTrace') = dlookupOrInsert (Key α) d qTrace
                          x <- lift (sample q)
                          -- | Store the gradient log-probability of the proposed distribution
                          let gradTrace' = dinsert (Key α) (gradLogProb q x) gradTrace
                          pure (x, q, qTrace', gradTrace')
    -- | Update the log-weight
    let logW' = logW + logProb d x - logProb q x
    handleSamp qTrace' gradTrace' logW' (k x)
  Left op' -> do
     Op op' (handleSamp qTrace gradTrace logW . k)

optimizerStep :: Double -> DTrace -> GTrace -> DTrace
optimizerStep η =
  dintersectLeftWith (\d g ->
    case isDifferentiable d of
      Nothing   -> d
      Just Dict -> addGrad d (scaleGrad η g))

-- | Scale each iteration's gradient trace by the iteration's total log weight
scaleGradients :: [(LogP, GTrace)] -> [GTrace]
scaleGradients wgs =
  let (logWs, gradTraces) = unzip wgs
  in  zipWith (\logW -> dmap (scaleGrad (unLogP logW))) logWs gradTraces

computeBaseline :: DiffDistribution d => DKey d -> [GTrace] -> [GTrace] -> Double
computeBaseline x gradTraces weightedGradTraces =
  let f = map (dlookup x) gradTraces
  in  undefined