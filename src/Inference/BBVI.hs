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
import Effects.Lift
import Effects.ObsRW ( ObsRW )
import Effects.State ( modify, handleState, State )
import Env ( Env )
import LogP ( LogP(unLogP) )
import Model ( handleCore, Model )
import PrimDist
import Prog ( discharge, Prog(..) )
import Sampler
import Trace
import Model
import Debug.Trace

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
    -- lift $ liftIO $ print $ "logW is" ++ show logW'
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

-- | Handle each @Observe@ operation by computing and accumulating a log probability
handleObs
  -- | accumulated log-probability
  :: LogP
  -> Prog (Observe : es) a
  -- | (model output, final likelihood weighting)
  -> Prog es (a, LogP)
handleObs logW (Val x) = return (x, logW)
handleObs logW (Op u k) = case discharge u of
    Right (Observe d y α) -> do
      let logW' = logProb d y
      handleObs (logW + logW') (k y)
    Left op' -> Op op' (handleObs logW . k)

{- | Scale each iteration's gradient log-pdf trace by the iteration's total importance weight.
     For each gradient trace G^l, uniformly scale all gradients to produce F^l = logW^l * G^l (should this be W^l * G^l ?)
-}
scaleGrads
  :: [LogP]     -- ^ logW^{1:L}
  -> [GTrace]   -- ^ G^{1:L}
  -> [GTrace]   -- ^ F^{1:L}
scaleGrads =
  zipWith (\logW -> dmap (liftUnOp (* (exp . unLogP) logW)))

-- | Compute the ELBO gradient estimate g for a random variable v's associated parameters
estELBOGrad :: forall d. DiffDistribution d
  => DKey d    -- ^   v
  -> [GTrace]  -- ^   G^{1:L}
  -> [GTrace]  -- ^   F^{1:L}
  -> d         -- ^   g(v)
estELBOGrad v traceGs traceFs =
  let {-  G_v^{1:L} -}
      vG :: [d] = map (fromJust . dlookup v) traceGs
      {-  F_v^{1:L} -}
      vF :: [d] = -- trace ("G(" ++ show v ++"): " ++ show vG) $
                    map (fromJust . dlookup v) traceFs
      {- | Compute the baseline vector constant b_v for a random variable's associated parameters
              b_v = covar(F_v^{1:L}, G_v^{1:L}) / var(G_v^{1:L}) -}
      vB :: d = -- trace ("F(" ++ show v ++"): " ++ show vF) $
                    liftBinOp (/) (covarGrad vF vG) (varGrad vG)

      {- | Compute the gradient estimate for v
              grad_v = Sum (F_v^{1:L} - b_v * G_v^{1:L}) / L -}
      _L          = -- trace ("vB(" ++ show v ++"): " ++ show vB) $
                          length traceGs
      elboGrads   = zipWith (\vG_l vF_l -> liftBinOp (-) vF_l (liftBinOp (*) vB vG_l)) vG vF
      elboGradEst = liftUnOp (/fromIntegral _L) $ foldr (liftBinOp (+)) zeroGrad elboGrads
  in  -- trace ("ElboGrads(" ++ show v ++"): " ++ show elboGrads)
      elboGradEst

-- | Compute the ELBO gradient estimates g for all random variables
estELBOGrads ::  [LogP] -> [GTrace] -> GTrace
estELBOGrads logWs traceGs =
  let traceFs = scaleGrads logWs traceGs
  in  foldr (\(Some var) elboGrads ->
                  dinsert var (estELBOGrad var traceGs traceFs) elboGrads
            ) dempty (dkeys $ head traceGs)

{- | Execute a model once and accumulate:
      - proposal distributions Q_l
      - gradient log-pdfs G_l
      - total importance weight logW_l
-}
bbviStep ::
  Prog [Observe, Sample, Lift Sampler] a
  -> Sampler (DTrace, GTrace, LogP)
bbviStep prog = do
  let logW_l = 0
      _Q_l   = dempty
      _G_l   = dempty

  ((_Q_l, _G_l, logW_l_samp), (x, logW_l_obs)) <- handleLift $ handleSamp _Q_l _G_l 0 (handleObs 0 prog)
  let logW_l = logW_l_samp + logW_l_obs
  pure (_Q_l, _G_l, logW_l)

bbviSteps
  :: Int
  -> Prog [Observe, Sample, Lift Sampler] a
  -> Sampler (([DTrace], [GTrace], [LogP]), GTrace)
bbviSteps _L prog = do
  bbvi_trace <- replicateM _L (bbviStep prog)
  let (traceQs, traceGs, logWs) = unzip3 bbvi_trace
  let  elboGradEst = estELBOGrads logWs traceGs
  pure ((traceQs, traceGs, logWs), elboGradEst)