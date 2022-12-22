{-# LANGUAGE DataKinds #-}

{- | Independence Metropolis inference, where proposals are independent of each other.
-}

module Inference.MC.IM where

import qualified Data.Map as Map
import Trace ( Trace )
import LogP ( LogP (..) )
import Model ( Model, handleCore, ProbProg )
import Effects.ObsRW ( ObsRW )
import Env (  Env )
import Effects.Dist ( Dist )
import Effects.Lift ( Lift, lift, handleLift, liftPutStrLn )
import Sampler ( Sampler, sampleRandom )
import qualified Inference.MC.LW as LW
import qualified Inference.MC.RWM as RWM
import qualified Inference.MC.Metropolis as Metropolis
import Util ( assocR )

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
      s_0    = LogP 0
      trace_0 = Map.empty
  rwm_trace <- (handleLift . RWM.handleAccept . Metropolis.metroLoop n (s_0, trace_0) handleModel) prog_0
  pure (map (snd . fst) rwm_trace)

{- | Handler for one iteration of IM.
-}
handleModel ::
     ProbProg a                         -- ^ probabilistic program
  -> (LogP, Trace)                     -- ^ proposed initial log-prob + sample trace
  -> Sampler (a, (LogP, Trace))        -- ^ proposed final log-prob + sample trace
handleModel prog (logp, τ) =
  ((assocR <$>) . Metropolis.reuseSamples τ . LW.likelihood logp) prog
