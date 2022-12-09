{-# LANGUAGE DataKinds #-}

{- | Independence Metropolis inference, where proposals are independent of each other.
-}

module Inference.MC.IM where

import qualified Data.Map as Map
import Trace ( STrace )
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
      ctx_0    = LogP 0
      strace_0 = Map.empty
  rwm_trace <- (handleLift . RWM.handleAccept . Metropolis.metropolisLoop n (ctx_0, strace_0) handleModel) prog_0
  pure (map (snd . fst) rwm_trace)

{- | Handler for one iteration of IM.
-}
handleModel ::
     ProbProg a                         -- ^ probabilistic program
  -> (LogP, STrace)                     -- ^ proposed initial log-prob + sample trace
  -> Sampler (a, (LogP, STrace))        -- ^ proposed final log-prob + sample trace
handleModel prog (logp, strace) =
  ((assocR <$>) . Metropolis.reuseSamples strace . LW.likelihood logp) prog
