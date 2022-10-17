{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

{- | Likelihood-Weighting inference.
-}

module Inference.BBVI
  where

import Data.Functor
import Data.Maybe
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

handleSamp
  :: DTrace   -- ^ optimisable distributions  Q
  -> GTrace   -- ^ gradients of log-pdfs      G
  -> LogP     -- ^ total importance weight    logW
  -> Prog '[Sample, Lift Sampler] a
  -> Prog '[Lift Sampler] ((DTrace, GTrace, LogP), a)
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

optimizerStep
  :: Double  -- ^ learning rate             η
  -> DTrace  -- ^ optimisable distributions Q
  -> GTrace  -- ^ elbo gradient estimates   g
  -> DTrace  -- ^ updated distributions     Q'
optimizerStep η =
  dintersectLeftWith (\d elboGradEst ->
    case isDifferentiable d of
      Nothing   -> d
      Just Dict -> addGrad d (liftUnOp (*η) elboGradEst))

{- | Scale each iteration's gradient trace by the iteration's total log weight.
     For each gradient trace G^l, uniformly scale all gradients to produce F^l = logW^l * G^l
-}
scaleGrads
  :: [LogP]     -- ^ logW^{1:L}
  -> [GTrace]   -- ^ G^{1:L}
  -> [GTrace]   -- ^ F^{1:L}
scaleGrads =
  zipWith (\logW -> dmap (liftUnOp (* unLogP logW)))

-- | Compute the ELBO gradient estimate g for a random variable v's associated parameters
estELBOGrad :: forall d. DiffDistribution d
  => DKey d    -- ^   v
  -> [GTrace]  -- ^   G^{1:L}
  -> [GTrace]  -- ^   F^{1:L}
  -> d         -- ^   g(v)
estELBOGrad v traceGs traceFs =
  let {-  G_v^{1:L} -}
      vG = map (fromJust . dlookup v) traceGs
      {-  F_v^{1:L} -}
      vF = map (fromJust . dlookup v) traceFs
      {- | Compute the baseline vector constant b_v for a random variable's associated parameters
              b_v = covar(F_v^{1:L}, G_v^{1:L}) / var(G_v^{1:L}) -}
      vB :: d = fromList $ zipWith (/) (toList $ covarGrad vF vG) (toList $ varGrad vG)

      {- | Compute the gradient estimate for v
              grad_v = Sum (F_v^{1:L} - b_v * G_v^{1:L}) / L -}
      _L          = length traceGs
      elboGrads   = zipWith (\vG_l vF_l -> liftBinOp (-) vF_l (liftBinOp (*) vB vG_l)) vG vF
      elboGradEst = liftUnOp (/fromIntegral _L) $ foldr (liftBinOp (+)) zeroGrad elboGrads
  in  elboGradEst

-- | Compute the ELBO gradient estimates g for all random variables
estELBOGrads ::  [LogP] -> [GTrace] -> GTrace
estELBOGrads logWs traceGs =
  let traceFs = scaleGrads logWs traceGs
  in  foldr (\(Some var) elboGrads ->
                  dinsert var (estELBOGrad var traceGs traceFs) elboGrads
            ) dempty (dkeys $ head traceGs)

-- bbviStep :: Int ->