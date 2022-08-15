{-# LANGUAGE DataKinds #-}
module Inference.PMMH where

import Prog
import Sampler
import LogP
import Trace
import Effects.Dist
import Effects.Lift
import qualified Inference.SIM as SIM
import qualified Inference.MH as MH
import qualified Inference.SIS as SIS
import qualified Inference.SMC as SMC

pmmhInternal ::
      Int                                     -- ^ number of MH steps
   -> Int                                     -- ^ number of particles
   -> Prog [Observe, Sample, Lift Sampler] a  -- ^ probabilistic program
   -> InvSTrace                               -- ^ initial sample trace
   -> [Tag]                                   -- ^ tags indicating the model parameters
   -> Sampler  [((a, LogP), InvSTrace)]       -- ^ trace of accepted outputs, samples, and logps
pmmhInternal mh_steps n_particles prog_0 strace_0 param_tags = do
  -- | Perform initial run of MH
  mh_ctx_0 <- MH.runMH strace_0 prog_0
  -- | Get samples used for model parameters
  let param_strace_0 = filterTrace param_tags strace_0
  -- | Perform initial run of smc to compute likelihood
  ctxs <- ( handleLift . MH.handleSamp param_strace_0 . SIM.handleObs
          . SIS.sis n_particles SMC.particleRunner SMC.particleResampler) prog_0
  -- let -- get final log probabilities of each particle
  --     lps     = map (snd3 . snd) ctxs
  --     -- compute average
  --     logW_0  = logMeanExp lps

  -- -- A function performing n pmmhsteps
  -- let pmmhs  = foldl (>=>) pure (replicate mh_steps (pmmhStep n_particles prog tags))
  -- l <- pmmhs [(y_0, strace_0, logW_0)]
  -- -- Return pmmhTrace in correct order of execution (due to pmmhStep prepending new results onto head of trace)
  -- pure $ reverse l
  undefined

