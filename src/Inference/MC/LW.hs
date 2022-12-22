{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{- | Likelihood-Weighting inference.
-}

module Inference.MC.LW
  ( -- * Inference wrapper functions
    lw
    -- * Inference effect handlers
  , runLW
  , likelihood
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
  lwTrace <- replicateM n (runLW prog)
  pure (map (bimap snd expLogP) lwTrace)

-- | Handler for one iteration of LW
runLW
  :: Prog [Observe, Sample] a
  -- | ((model output, sample trace), likelihood-weighting)
  -> Sampler (a, LogP)
runLW = SIM.handleSamp . likelihood 0

-- | Handle each @Observe@ operation by accumulating the log-likelihood P(Y | X)
likelihood
  :: LogP
  -> Prog (Observe : es) a
  -- | (model output, final likelihood weighting)
  -> Prog es (a, LogP)
likelihood lρ (Val x) = return (x, lρ)
likelihood lρ (Op u k) = case discharge u of
    Right (Observe d y α) -> do
      let lρ' = logProb d y
      likelihood (lρ + lρ') (k y)
    Left op' -> Op op' (likelihood lρ . k)