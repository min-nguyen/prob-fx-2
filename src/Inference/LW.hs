{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE TypeOperators #-}
module Inference.LW where

import qualified Data.Map as Map
import Env
import Effects.ObsReader
import Effects.Lift
import Control.Monad
import Effects.Dist
import Prog
import PrimDist
import Model hiding (runModelFree)
import Sampler
import Effects.State ( modify, handleState, State )
import Trace
import Inference.SIM as SIM (handleSamp)

-- ||| (Section 6.2.1) Likelihood Weighting (LW)
lw :: forall env es a b. (FromSTrace env)
    => 
    -- | Number of LW iterations
       Int                          
    -- | A model
    -> Model env [ObsReader env, Dist, Lift Sampler] a       
    -- | A model environment (containing observed values to condition on)
    -> Env env         
    -- | List of n likelihood weightings for each data point
    -> Sampler [(Env env, Double)]  
lw n model env = do
  let prog = (handleDist . handleObsRead env) (runModel model)
  lwTrace <- runLWs n prog
  pure $ map (\((_, strace), p) -> (fromSTrace strace, p)) lwTrace

-- | LW handler
runLWs :: Int -> Prog [Observe, Sample, Lift Sampler] a -> Sampler [((a, STrace), Double)]
runLWs n prog = replicateM n (runLW prog)

-- | Run LW once
runLW :: Prog [Observe, Sample, Lift Sampler] a -> Sampler ((a, STrace), Double)
runLW  = handleLift . SIM.handleSamp . handleObs 0 . traceSamples

-- | Accumulate log probabilities of each Observe operation
handleObs :: Double -> Prog (Observe : es) a -> Prog es (a, Double)
handleObs logp (Val x) = return (x, exp logp)
handleObs logp (Op u k) = case discharge u of
    Right (Observe d y α) -> do
      let logp' = logProb d y
      handleObs (logp + logp') (k y)
    Left op' -> Op op' (handleObs logp . k)
