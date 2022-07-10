{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE TypeOperators #-}
module Inference.LW where


import Control.Monad
import Effects.Dist
import Effects.Lift
import Effects.ObsReader
import Effects.State ( modify, handleState, State )
import Env
import Inference.SIM as SIM (handleSamp)
import Model hiding (runModelFree)
import PrimDist
import Prog
import qualified Data.Map as Map
import Sampler
import Trace

-- ||| Likelihood Weighting (LW)
lw :: forall env es a b. (FromSTrace env)
    => 
    -- | Number of LW iterations
       Int                          
    -- | A model
    -> Model env [ObsReader env, Dist, Lift Sampler] a       
    -- | A model environment (containing observed values to condition on)
    -> Env env         
    -- | List of n weightings for each data point
    -> Sampler [(Env env, Double)]  
lw n model env = do
  let prog = handleCore env model
  lwTrace <- lwInternal n prog
  pure $ map (\((_, strace), p) -> (fromSTrace strace, p)) lwTrace

-- ||| Run LW 'n' times on probabilistic program 
lwInternal :: Int -> Prog [Observe, Sample, Lift Sampler] a -> Sampler [((a, STrace), Double)]
lwInternal n prog = replicateM n (runLW prog)

-- ||| Handle LW once on probabilistic program 
runLW :: Prog [Observe, Sample, Lift Sampler] a -> Sampler ((a, STrace), Double)
runLW  = handleLift . SIM.handleSamp . handleObs 0 . traceSamples

-- ||| Accumulate log probabilities of each Observe operation
handleObs :: Double -> Prog (Observe : es) a -> Prog es (a, Double)
handleObs logp (Val x) = return (x, exp logp)
handleObs logp (Op u k) = case discharge u of
    Right (Observe d y α) -> do
      let logp' = logProb d y
      handleObs (logp + logp') (k y)
    Left op' -> Op op' (handleObs logp . k)
