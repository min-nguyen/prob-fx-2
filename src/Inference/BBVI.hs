{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
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
import LogP ( LogP(unLogP), normaliseLogPs )
import Model
import PrimDist
import Prog ( discharge, Prog(..), call, weaken, LastMember, Member (..), Members, weakenProg )
import Sampler
import Trace
import Model
import Debug.Trace
import qualified Inference.SIM as SIM
import qualified Inference.LW as LW
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
    -- lift $ liftPrint $ "logW is" ++ show logW'
    handleSamp traceQ' traceG' logW' (k x)
  Left op' -> do
     Op op' (handleSamp traceQ traceG logW . k)
-}

installScore :: forall es a. Member Sample es
  => Prog es a
  -> Prog (Score : es) (a, DTrace)
installScore = loop dempty where
  loop :: DTrace -> Prog es a -> Prog (Score : es) (a, DTrace)
  loop traceQ  (Val x)   = return (x, traceQ)
  loop traceQ  (Op op k) = case prj op of
    Just (Sample d α) ->
      case isDifferentiable d of
        Nothing   -> Op (weaken op) (loop traceQ . k)
        Just Dict -> do let traceQ' = dinsert (Key α) d traceQ
                        x <- call (Score d d α)
                        (loop traceQ' . k) x
    Nothing -> Op (weaken op) (loop traceQ . k)

-- | Replace each @Sample@ with a @Score@ operation if its distribution is differentiable
updateScore :: forall es a. Member Score es
  => DTrace
  -> Prog es a
  -> Prog es a
updateScore traceQ = loop where
  loop :: Prog es a -> Prog es a
  loop  (Val x)   = return x
  loop  (Op op k) = case prj op of
    Just (Score d _ α) -> do
      let q = dlookupDefault (Key α) traceQ d
      x <- call (Score d q α)
      (loop . k) x
    Nothing -> Op op (loop . k)

-- | Compute the total importance weight: log[P(X, Y) / Q(X; λ)]
traceLogProbs :: forall es a. (Members [Score, Observe] es)
  => Prog es a
  -> Prog es (a, LogP)
traceLogProbs = loop 0 where
  loop :: LogP -> Prog es a -> Prog es (a, LogP)
  loop logW (Val x) = pure (x, logW)
  loop logW (Op op k) = do
    case op of
      -- | Compute: log(P(Y))
      ObsPrj d y α   -> Op op (\x -> loop (logW + logProb d x) $ k x)
      -- | Compute: log(P(X)) - log(Q(X; λ))
      ScorePrj d q α -> Op op (\x -> loop (logW + logProb d x - logProb q x) $ k x)
      _              -> Op op (loop logW . k)

-- | Record the gradients ∇log(Q(X;λ)) at each @Score@ operation
traceGrads :: forall es a. (Member Score es)
  => Prog es a
  -> Prog es (a, GTrace) -- Compute ∇log(Q(X;λ))
traceGrads = loop dempty where
  loop :: GTrace -> Prog es a -> Prog es (a, GTrace)
  loop traceG (Val x) = pure (x, traceG)
  loop traceG (Op op k) = do
    case prj op of
      Just (Score _ q α)
        -> Op op (\x -> let traceG' = dinsert (Key α) (gradLogProb q x) traceG
                        in  (loop traceG' . k) x)
      _ -> Op op (loop traceG . k)

-- | Trivially handle each @Score@ operation by sampling X ~ Q
handleScore :: LastMember (Lift Sampler) es => Prog (Score : es) a -> Prog es a
handleScore (Val x) = return x
handleScore (Op op k) = case discharge op of
  Right (Score _ q α) -> lift (sample q) >>= handleScore . k
  Left op'            -> Op op' (handleScore  . k)

-- | Trivially handle each @Observe@ operation, Y ~ P
handleObs :: Prog (Observe : es) a -> Prog es a
handleObs = SIM.handleObs

-- | Trivially handle each unscoreable @Sample@ operation by sampling, X ~ P
handleSamp :: LastMember (Lift Sampler) es => Prog (Sample : es) a -> Prog es a
handleSamp = SIM.handleSamp

{- One iteration of model execution
-}
runBBVI :: Members [Score, Observe, Sample] es
  => DTrace
  -> Prog es a
  -> Prog es ((a, GTrace), LogP)
runBBVI proposals = traceLogProbs . traceGrads . updateScore proposals

{- | Execute a model for L iterations, and accumulate:
      - gradient log-pdfs G_l
      - total importance weight logW_l
-}
bbviStep :: (ProbSig es, Member Score es)
  => Int                         -- ^ number of samples to estimate gradient over (L)
  -> DTrace                      -- ^ initial proposal distributions (Q)
  -> Prog es a
  -> Prog es ([(a, LogP)], DTrace)
bbviStep l_samples traceQ prog = do
  -- | Execute a model for L iterations, collecting gradient traces G_l and importance weights logW_l:
  ((as, traceGs), logWs) <- first unzip . unzip <$> replicateM l_samples (runBBVI traceQ prog)
  -- | Compute the ELBO gradient estimates
  let elboG   = estELBOGrads logWs traceGs
  -- | Update the parameters of the proposal distributions Q
      traceQ' = optimizeParam 1.0 traceQ elboG
  liftPutStrLn $ "Proposal Distributions Q:\n" ++ show traceQ ++ "\n"
  liftPutStrLn $ "Gradient Log-Pdfs G_l for {1:L}:\n" ++ show traceGs ++ "\n"
  liftPutStrLn $ "Log Importance Weights logW_l for {1:L}:\n" ++ show logWs ++ "\n"
  liftPutStrLn $ "ELBO Grad Estimate g:\n" ++ show elboG ++ "\n"
  liftPutStrLn $ "Updated Proposal Distributions Q':\n" ++ show traceQ' ++ "\n"
  pure (zip as logWs, traceQ')

bbviLoop :: forall es a. (ProbSig es)
  => Int        -- ^ T
  -> Int        -- ^ L
  -> Prog es a
  -> Prog (Score : es) DTrace
bbviLoop t_steps l_samples prog = do
  -- | Initial proposal distributions from priors
  traceQ0 <- snd <$> installScore prog
  loop 0 traceQ0
  where
    bbvi_prog :: Prog (Score : es) a
    bbvi_prog = fst <$> installScore prog

    loop :: Int -> DTrace -> Prog (Score : es) DTrace
    loop t traceQ
      | t >= t_steps    = pure traceQ
      | otherwise = do
          liftPutStrLn $ "## ITERATION t = " ++ show t ++ " ##"
          (out, traceQ') <- bbviStep l_samples traceQ bbvi_prog
          loop (t + 1) traceQ'

bbvi :: Int -> Int -> Prog [Observe, Sample, Lift Sampler] a -> Sampler DTrace
bbvi t_steps l_samples prog =
  (handleLift . handleSamp . handleObs . handleScore) (bbviLoop t_steps l_samples prog)

{- | Uniformly scale each iteration's gradient trace G^l by its corresponding total importance weight W^l.
        F^l = W^l * G^l
-}
scaleGrads
  :: [LogP]     -- ^ logW^{1:L}
  -> [GTrace]   -- ^ G^{1:L}
  -> [GTrace]   -- ^ F^{1:L}
scaleGrads = zipWith (\logW -> dmap (liftUnOp (* (exp . unLogP) logW)))

-- | Compute the ELBO gradient estimates g for all random variables
estELBOGrads :: [LogP] -> [GTrace] -> GTrace
estELBOGrads logWs traceGs =
  foldr (\(Some var) elboGrads ->
                  dinsert var (estELBOGrad var traceGs traceFs) elboGrads
            ) dempty vars
  where
    normLogWs = normaliseLogPs logWs
    traceFs   = scaleGrads normLogWs traceGs
    vars      = dkeys $ head traceGs
    -- | Compute the ELBO gradient estimate g for a random variable v's associated parameters
    estELBOGrad :: forall d. DiffDistribution d
      => DKey d    -- ^   v
      -> [GTrace]  -- ^   G^{1:L}
      -> [GTrace]  -- ^   F^{1:L}
      -> d         -- ^   g(v)
    estELBOGrad v traceGs traceFs =
      let {-  G_v^{1:L} -}
          traceGs_v :: [d] =  map (fromJust . dlookup v) traceGs
          {-  F_v^{1:L} -}
          traceFs_v :: [d] = trace ("G(" ++ show v ++"): " ++ show (map show traceGs_v)) $
                              map (fromJust . dlookup v) traceFs
          {- b_v = covar(F_v^{1:L}, G_v^{1:L}) / var(G_v^{1:L}) -}
          baseline_v :: d = trace ("F(" ++ show v ++"): " ++ show (map show traceFs_v)) $
                              liftBinOp (/) (covarGrad traceFs_v traceGs_v) (varGrad traceGs_v)

          {- elbo_v = sum (F_v^{1:L} - b_v * G_v^{1:L}) / L -}
          _L          = -- trace ("vB(" ++ show v ++"): " ++ show vB) $
                              length traceGs
          elboGrads   = zipWith (\vG_l vF_l -> liftBinOp (-) vF_l (liftBinOp (*) baseline_v vG_l)) traceGs_v traceFs_v
          elboGradEst = liftUnOp (/fromIntegral _L) $ foldr (liftBinOp (+)) zeroGrad elboGrads
      in  -- trace ("ElboGrads(" ++ show v ++"): " ++ show elboGrads)
          trace (show elboGradEst) elboGradEst

{- | Update the proposal distribution parameters λ using the estimated ELBO gradients ∇L(λ).
        λ_t = λ_{t−1} + η_t * ∇L(λ)
-}
optimizeParam
  :: Double  -- ^ learning rate             η
  -> DTrace  -- ^ optimisable distributions Q
  -> GTrace  -- ^ elbo gradient estimates   δλ
  -> DTrace  -- ^ updated distributions     Q'
optimizeParam η =
  dintersectLeftWith (\q δλ ->
    safeAddGrad q (liftUnOp (*η) δλ))
