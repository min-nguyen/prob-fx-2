{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{- | Likelihood-Weighting inference.
-}

module Inference.LW
  ( -- * Inference wrapper functions
    lw
  , lwInternal
    -- * Inference effect handlers
  , runLW
  , handleObs
  ) where

import Control.Monad ( replicateM )
import Effects.Dist ( Sample, Observe(..), Dist )
import Effects.Lift ( handleLift, Lift )
import Effects.ObsRW
import Effects.State ( modify, handleState, State )
import Env ( Env )
import LogP
import Inference.SIM as SIM (handleSamp)
import Model ( handleCore, Model )
import PrimDist ( logProb )
import Prog ( discharge, Prog(..) )
import qualified Data.Map as Map
import Sampler ( Sampler )
import Trace ( traceSamples, STrace, FromSTrace(..) )

-- | Top-level wrapper for Likelihood-Weighting (LW) inference
lw
  -- | number of LW iterations
  :: Int
  -- | model
  -> Model env [ObsRW env, Dist, Lift Sampler] a
  -- | input model environment
  -> Env env
  -- | [(output model environment, likelihood-weighting)]
  -> Sampler [(Env env, Double)]
lw n model env_in = do
  let prog = handleCore env_in model
  lwTrace <- lwInternal n prog
  pure $ map (\((_, env_out), p) -> (env_out, p)) lwTrace

-- | Run LW n times
lwInternal
  -- | number of LW iterations
  :: Int
  -> Prog [Observe, Sample, Lift Sampler] a
  -- | list of weighted model outputs and sample traces
  -> Sampler [(a, Double)]
lwInternal n prog = replicateM n (runLW prog)

-- | Handler for one iteration of LW
runLW
  :: Prog [Observe, Sample, Lift Sampler] a
  -- | ((model output, sample trace), likelihood-weighting)
  -> Sampler (a, Double)
runLW = handleLift . SIM.handleSamp . handleObs 0

-- | Handle each @Observe@ operation by computing and accumulating a log probability
handleObs
  -- | accumulated log-probability
  :: LogP
  -> Prog (Observe : es) a
  -- | (model output, final likelihood weighting)
  -> Prog es (a, Double)
handleObs logp (Val x) = return (x, (exp . unLogP) logp)
handleObs logp (Op u k) = case discharge u of
    Right (Observe d y α) -> do
      let logp' = logProb d y
      handleObs (logp + logp') (k y)
    Left op' -> Op op' (handleObs logp . k)
