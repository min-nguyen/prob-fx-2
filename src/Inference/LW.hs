{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Inference.LW where

import qualified Data.Map as Map
import Data.Map (Map)
import Env
import Effects.ObsReader ( handleObsRead, ObsReader )
import Effects.Lift
import Control.Monad
import Effects.Dist
import Prog
import PrimDist
import Model hiding (runModelFree)
import Sampler
import Effects.State ( modify, handleState, State )
import qualified Inference.SIM as SIM (handleSamp)
import Trace ( traceSamples, FromSTrace(..), STrace )

type LWTrace a = [(a, STrace, Double)]

-- | Run LW n times
lwTopLevel :: FromSTrace env
   =>                                  
   -- | Number of LW iterations
   Int  
   -- | Model
   -> Model env [ObsReader env, Dist, Lift Sampler] a 
   -> Env env                             -- List of model observed variables
   -> Sampler [(Env env, Double)]         -- List of n likelihood weightings for each data point
lwTopLevel n model env = do
  let prog = (handleDist . handleObsRead env) (runModel model)
  lwTrace <- lw n prog
  pure (map (\(_, env, p) -> (fromSTrace env, p)) lwTrace)

lw :: Int                         -- Number of lw iterations
   -> Prog [Observe, Sample, Lift Sampler] a    -- Model
   -> Sampler (LWTrace a)         -- List of n likelihood weightings for each data point
lw n prog = replicateM n (runLW prog)

-- | Run LW once
runLW :: Prog [Observe, Sample, Lift Sampler] a -> Sampler (a, STrace, Double)
runLW prog = do
  ((x, samples), p) <- (handleLift . SIM.handleSamp . handleObs 0 . traceSamples) prog
  pure (x, samples, p)

handleObs :: Member Sample es => Double -> Prog (Observe : es) a -> Prog es (a, Double)
handleObs logp (Val x) = pure (x, exp logp)
handleObs logp (Op u k) = case discharge u of
    Right (Observe d y α) -> do
      let logp' = logProb d y
      handleObs (logp + logp') (k y)
    Left op' -> Op op' (handleObs logp . k)
