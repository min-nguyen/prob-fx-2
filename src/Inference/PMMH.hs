{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Inference.PMMH where

import Control.Monad
import Prog
import Sampler
import LogP
import Trace
import Effects.Dist
import PrimDist
import Model
import Env
import Effects.Lift
import Effects.ObsRW
import qualified Data.Map as Map
import qualified Inference.SIM as SIM
import qualified Inference.MH as MH
import qualified Inference.SIS as SIS
import qualified Inference.SMC as SMC
import Util
import Inference.SMC (SMCParticle(particleLogProb, SMCParticle))

pmmh :: forall env es a xs. (env `ContainsVars` xs)
  -- | number of MH iterations
  => Int
  -- | number of SMC particles
  -> Int
  -- | model
  -> Model env [ObsRW env, Dist, Lift Sampler] a
  -- | input model environment
  -> Env env
  -- | model parameter names
  -> Vars xs
  -- | output model environments
  -> Sampler [Env env]
pmmh mh_steps n_particles model env obs_vars = do
  let prog = (handleDist . handleObsRW env) (runModel model)
  -- | Convert observable variables to strings
      tags = varsToStrs @env obs_vars
  -- | Run PMMH
  pmmh_trace <- pmmhInternal mh_steps n_particles prog Map.empty tags
  -- Return the accepted model environments
  pure (map (snd . fst . fst) pmmh_trace)

pmmhInternal :: forall a.
      Int                                     -- ^ number of MH steps
   -> Int                                     -- ^ number of particles
   -> Prog [Observe, Sample, Lift Sampler] a  -- ^ probabilistic program
   -> InvSTrace                               -- ^ initial sample trace
   -> [Tag]                                   -- ^ tags indicating the model parameters
   -> Sampler  [((a, LogP), InvSTrace)]       -- ^ trace of accepted outputs, samples, and logps
pmmhInternal mh_steps n_particles prog strace param_tags = do
  -- | Perform initial run of MH
  pmmh_0 <- runPMMH n_particles prog param_tags strace
  -- | A function performing n pmmhsteps
  foldl (>=>) pure (replicate mh_steps (pmmhStep n_particles prog param_tags)) [pmmh_0]

pmmhStep ::
     Int                                          -- ^ number of particles
  -> Prog [Observe, Sample, Lift Sampler] a       -- ^ model
  -> [Tag]                                        -- ^ tags indicating prior random variables
  -> [((a, LogP), InvSTrace)]                     -- ^ trace of previous mh outputs
  -> Sampler [((a, LogP), InvSTrace)]
pmmhStep n_particles prog tags pmmh_trace = do
  let pmmh_ctx@(_, strace) = head pmmh_trace
  -- | Propose a new random value for a sample site
  (α_samp, r) <- MH.propose strace tags
  pmmh_ctx'   <- runPMMH n_particles prog tags (Map.insert α_samp r strace)
  b <- accept pmmh_ctx pmmh_ctx'
  if b then pure (pmmh_ctx':pmmh_trace)
       else pure pmmh_trace

-- | Handle probabilistic program once under PMMH
runPMMH ::
     Int
  -> Prog '[Observe, Sample, Lift Sampler] a
  -> [Tag]
  -> InvSTrace
  -> Sampler ((a, LogP), InvSTrace)
runPMMH n_particles prog tags strace = do
  ((a, _), strace') <- MH.runMH strace prog
  let params = filterTrace tags strace'
  prts   <- ( (map snd . fst <$>)
            . handleLift
            . MH.handleSamp params
            . SIM.handleObs
            . SIS.sis n_particles SMC.particleRunner SMC.particleResampler) prog
  let logZ  = logMeanExp (map SMC.particleLogProb prts)
  pure ((a, logZ), strace')

accept ::
  -- | previous PMMH ctx
     ((a, LogP), InvSTrace)
  -- | proposed PMMH ctx
  -> ((a, LogP), InvSTrace)
  -> Sampler Bool
accept ((_, log_p), _) ((_, log_p'), _) = do
  let acceptance_ratio = (exp . unLogP) (log_p' - log_p)
  u <- sample (Uniform 0 1)
  pure (u < acceptance_ratio)

