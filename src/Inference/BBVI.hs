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
import Prog ( discharge, Prog(..), call, weaken, LastMember )
import Sampler
import Trace
import Model
import Debug.Trace

{-
handleSamp
  :: DTrace   -- ^ optimisable distributions  Q
  -> GTrace   -- ^ gradients of log-pdfs      G
  -> LogP     -- ^ total importance weight    logW
  -> Prog '[Sample, Lift Sampler] a
  -> Prog '[Lift Sampler] ((DTrace, GTrace, LogP), a)
handleSamp traceQ traceG logW (Val x)   = return ((traceQ, traceG, logW), x)
handleSamp traceQ traceG logW (Op op k) = case discharge op of
  Right (Sample d α) -> do
    (x, q, traceQ', traceG') <- case isDifferentiable d of
          Nothing   -> do x <- lift (sample d)
                          pure (x, d, traceQ, traceG)
          Just Dict -> do -- | Get the recently proposed distribution, else initialise with prior
                          let (q, traceQ') = dlookupOrInsert (Key α) d traceQ
                          x <- lift (sample q)
                          -- | Store the gradient log-probability of the proposed distribution
                          let traceG' = dinsert (Key α) (gradLogProb q x) traceG
                          pure (x, q, traceQ', traceG')
    -- | Update the log-weight
    let logW' = logW + logProb d x - logProb q x
    -- lift $ liftIO $ print $ "logW is" ++ show logW'
    handleSamp traceQ' traceG' logW' (k x)
  Left op' -> do
     Op op' (handleSamp traceQ traceG logW . k)
-}
-- handleScore
--   ::

-- | Replace each @Sample@ with a @Score@ operation if its distribution is differentiable
handleSamp
  :: LastMember (Lift Sampler) es
  => Prog (Sample : es) a
  -> Prog (Score : es) a
handleSamp (Val x)   = return x
handleSamp (Op op k) = case discharge op of
  Right (Sample d α) ->
    case isDifferentiable d of
          Nothing   -> lift (sample d)  >>= handleSamp . k
          Just Dict -> call (Score d α) >>= handleSamp . k . fst
  Left op' -> do
     Op (weaken op') (handleSamp . k)

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
      traceGs_v :: [d] = map (fromJust . dlookup v) traceGs
      {-  F_v^{1:L} -}
      traceFs_v :: [d] = -- trace ("G(" ++ show v ++"): " ++ show vG) $
                    map (fromJust . dlookup v) traceFs
      {- | Compute the baseline vector constant b_v for a random variable's associated parameters
              b_v = covar(F_v^{1:L}, G_v^{1:L}) / var(G_v^{1:L}) -}
      baseline_v :: d = -- trace ("F(" ++ show v ++"): " ++ show vF) $
                    liftBinOp (/) (covarGrad traceFs_v traceGs_v) (varGrad traceGs_v)

      {- | Compute the gradient estimate for v
              grad_v = Sum (F_v^{1:L} - b_v * G_v^{1:L}) / L -}
      _L          = -- trace ("vB(" ++ show v ++"): " ++ show vB) $
                          length traceGs
      elboGrads   = zipWith (\vG_l vF_l -> liftBinOp (-) vF_l (liftBinOp (*) baseline_v vG_l)) traceGs_v traceFs_v
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

optimizerStep
  :: Double  -- ^ learning rate             η
  -> DTrace  -- ^ optimisable distributions Q
  -> GTrace  -- ^ elbo gradient estimates   g
  -> DTrace  -- ^ updated distributions     Q'
optimizerStep η =
  dintersectLeftWith (\d elboGradEst ->
    case isDifferentiable d of
      Nothing   -> d
      Just Dict -> safeAddGrad d (liftUnOp (*η) elboGradEst))

{- | Execute a model once for iteration l, and accumulate:
      - gradient log-pdfs G_l
      - total importance weight logW_l
-}
bbviStep
  :: DTrace
  -> Prog [Observe, Sample, Lift Sampler] a
  -> Sampler (DTrace, GTrace, LogP)
bbviStep _Q prog = do
  let logW_l = 0
      _G_l   = dempty

  ((_Q', _G'_l, logW_l_samp), (x, logW_l_obs)) <- handleLift $ handleSamp _Q _G_l 0 (handleObs 0 prog)
  let logW'_l = logW_l_samp + logW_l_obs
  pure (_Q', _G'_l, logW'_l)

{- | Execute a model for L iterations, and return for each iteration l:
      - the gradient log-pdfs G_l
      - the total importance weight logW_l
-}
bbviSteps
  :: Int          -- ^ number of samples to estimate gradient over (L)
  -> DTrace       -- ^ initial proposal distributions (Q)
  -> Prog [Observe, Sample, Lift Sampler] a
  -> Sampler ((GTrace, [GTrace], [LogP]), DTrace)
bbviSteps _L _Q prog = do

  bbvi_trace <- replicateM _L (bbviStep  _Q prog)
  let (traceQs, traceGs, logWs) = unzip3 bbvi_trace
  -- | _Q' and _Q are different only if _Q was empty (i.e. for the very first call of `bbviSteps`)
      _Q'          = head traceQs
      elboGradEst  = estELBOGrads logWs traceGs
      opt_Q'       = optimizerStep 1.0 _Q' elboGradEst

  pure ((elboGradEst, traceGs, logWs), opt_Q')

bbviUpdate
  :: Int        -- ^ T
  -> Int        -- ^ L
  -> Prog [Observe, Sample, Lift Sampler] a
  -> Sampler DTrace
bbviUpdate _T _L prog = do
  -- | Initialise proposal distributions
  ((_, _, _), _Q0) <- bbviSteps _L dempty prog

  bbviLoop 0 _Q0

  where
    bbviLoop :: Int -> DTrace -> Sampler DTrace
    bbviLoop t _Q
      | t >= _T    = pure _Q
      | otherwise = do
          liftIO $ putStrLn $ "## ITERATION t = " ++ show t ++ " ##"
          ((elboGradEst, traceGs, logWs), _Q') <- bbviSteps _L _Q prog
          liftIO $ putStrLn "Proposal Distributions Q:"
          liftIO $ print _Q
          liftIO $ putStrLn ""
          liftIO $ putStrLn "Gradient Log-Pdfs G_l for {1:L}:"
          liftIO $ print traceGs
          liftIO $ putStrLn "Log Importance Weights logW_l for {1:L}:"
          liftIO $ print logWs
          liftIO $ putStrLn ""
          liftIO $ putStrLn "ELBO Grad Estimate g:"
          liftIO $ print elboGradEst
          liftIO $ putStrLn ""
          liftIO $ putStrLn "Updated Proposal Distributions Q':"
          liftIO $ print _Q'
          liftIO $ putStrLn ""

          bbviLoop (t + 1) _Q'