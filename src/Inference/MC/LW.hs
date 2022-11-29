{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{- | Likelihood-Weighting inference.
-}

module Inference.MC.LW
  ( -- * Inference wrapper functions
    lw
  , lwInternal
    -- * Inference effect handlers
  , runLW
  , weighLikelihood
  ) where

import Data.Bifunctor ( Bifunctor(first), second, bimap )
import Control.Monad ( replicateM )
import Effects.Dist ( Sample, Observe(..), Dist )
import Effects.Lift ( handleLift, Lift )
import Effects.ObsRW ( ObsRW )
import Effects.State ( modify, handleState, State )
import Env ( Env )
import LogP ( LogP, expLogP )
import Inference.MC.SIM as SIM (handleSamp)
import Model ( handleCore, Model )
import PrimDist ( logProb )
import Prog ( discharge, Prog(..) )
import Sampler ( Sampler )

-- | Top-level wrapper for Likelihood-Weighting (LW) inference
lw
  -- | number of LW iterations
  :: Int
  -- | model
  -> Model env [ObsRW env, Dist] a
  -- | input model environment
  -> Env env
  -- | [(output model environment, likelihood-weighting)]
  -> Sampler [(Env env, Double)]
lw n model env_in = do
  let prog = handleCore env_in model
  lwTrace <- lwInternal n prog
  pure (map (bimap snd expLogP) lwTrace)

-- | Run LW n times
lwInternal
  -- | number of LW iterations
  :: Int
  -> Prog [Observe, Sample] a
  -- | list of weighted model outputs and sample traces
  -> Sampler [(a, LogP)]
lwInternal n prog = replicateM n (runLW prog)

-- | Handler for one iteration of LW
runLW
  :: Prog [Observe, Sample] a
  -- | ((model output, sample trace), likelihood-weighting)
  -> Sampler (a, LogP)
runLW = SIM.handleSamp . weighLikelihood

-- | Handle each @Observe@ operation by accumulating the log-likelihood P(Y | X)
weighLikelihood
  :: Prog (Observe : es) a
  -- | (model output, final likelihood weighting)
  -> Prog es (a, LogP)
weighLikelihood = loop 0 where
  loop :: LogP -> Prog (Observe : es) a -> Prog es (a, LogP)
  loop logp (Val x) = return (x, logp)
  loop logp (Op u k) = case discharge u of
      Right (Observe d y α) -> do
        let logp' = logProb d y
        loop (logp + logp') (k y)
      Left op' -> Op op' (loop logp . k)