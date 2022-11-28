{-# LANGUAGE DataKinds #-}

{- | Independence Metropolis inference, where proposals are independent of each other.
-}

module Inference.MC.IM where

import qualified Data.Map as Map
import Trace ( STrace )
import LogP ( LogP )
import Model ( Model, handleCore, ProbProg )
import Effects.ObsRW ( ObsRW )
import Env (  Env )
import Effects.Dist ( Dist )
import Effects.Lift ( Lift, lift, handleLift, liftPutStrLn )
import Sampler ( Sampler, sampleRandom )
import qualified Inference.MC.LW as LW
import qualified Inference.MC.RWM as RWM
import qualified Inference.MC.Metropolis as Metropolis

{- | Top-level wrapper for Independence Metropolis
-}
im ::
     Int                            -- ^ number of iterations
  -> Model env [ObsRW env, Dist] a  -- ^ model
  -> Env env                        -- ^ input environment
  -> Sampler [Env env]              -- ^ output model environments
im n model env_in   = do
  -- | Handle model to probabilistic program
  let prog_0   = handleCore env_in model
      strace_0 = Map.empty
  rwm_trace <- (handleLift . RWM.handleAccept . Metropolis.metropolisLoop n strace_0 handleModel) prog_0
  pure (map (snd . fst . fst) rwm_trace)

{- | Handler for one iteration of IM.
-}
handleModel ::
     STrace                             -- ^ sample trace of previous RWM iteration
  -> ProbProg a                         -- ^ probabilistic program
  -> Sampler ((a, LogP), STrace)        -- ^ ((model output, sample trace), log-probability trace)
handleModel strace =
  Metropolis.reuseSamples strace . LW.weighLikelihood
