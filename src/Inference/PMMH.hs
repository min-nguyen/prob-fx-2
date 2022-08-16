{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Inference.PMMH where

import Control.Monad
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
import Util
import Inference.SMC (SMCParticle(particleLogProb, SMCParticle))

pmmhInternal :: forall a.
      Int                                     -- ^ number of MH steps
   -> Int                                     -- ^ number of particles
   -> Prog [Observe, Sample, Lift Sampler] a  -- ^ probabilistic program
   -> InvSTrace                               -- ^ initial sample trace
   -> [Tag]                                   -- ^ tags indicating the model parameters
   -> Sampler  [((a, LogP), InvSTrace)]       -- ^ trace of accepted outputs, samples, and logps
pmmhInternal mh_steps n_particles prog strace param_tags = do
  -- | Perform initial run of MH
  mh_0 <- MH.runMH strace prog
  -- | Get samples used for model parameters
  let params_0 = filterTrace param_tags strace
  -- | Perform initial run of SMC using model parameters
  prts_0 <- map snd . fst <$> ( handleLift . MH.handleSamp params_0 . SIM.handleObs
                              . SIS.sis n_particles SMC.particleRunner SMC.particleResampler) prog
  -- | Compute average log probabilities
  let logZ_0  = logMeanExp (map SMC.particleLogProb prts_0)
  -- -- A function performing n pmmhsteps
  -- let pmmhs  = foldl (>=>) pure (replicate mh_steps (pmmhStep n_particles prog tags))
  -- l <- pmmhs [(y_0, strace_0, logW_0)]
  -- -- Return pmmhTrace in correct order of execution (due to pmmhStep prepending new results onto head of trace)
  -- pure $ reverse l
  undefined

pmmhStep ::
     Int                                          -- ^ number of particles
  -> Prog [Observe, Sample, Lift Sampler] a       -- ^ model
  -> [Tag]                                        -- ^ tags indicating prior random variables
  -> [((a, LogP), InvSTrace)]                     -- ^ trace of previous mh outputs
  -> Sampler [((a, LogP), InvSTrace)]
pmmhStep n_particles prog tags  =
  MH.mhStep prog tags (undefined n_particles prog tags)

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
  prts   <- ( handleLift
            . MH.handleSamp params
            . SIM.handleObs
            . SIS.sis n_particles SMC.particleRunner SMC.particleResampler) prog
  let logZ  = logMeanExp (map (SMC.particleLogProb . snd) (fst prts))
  pure ((a, logZ), strace')

-- acceptSMC ::
--      Int
--   -> Prog '[Observe, Sample, Lift Sampler] a
--   -> [Tag]
--   -> MH.Accept LogP a
-- acceptSMC n_particles prog tags _ ((_, logZ), _) ((a, lptrace'), strace') = do
--   -- | Run SMC using prior samples
--   let params = filterTrace tags strace'
--   prts <- map snd . fst <$> ( handleLift . MH.handleSamp params . SIM.handleObs
--                               . SIS.sis n_particles SMC.particleRunner SMC.particleResampler) prog
--    -- get final log probabilities of each particle
--   let logZ'  = logMeanExp (map SMC.particleLogProb prts)
--       {-  if logW' and logW = -Infinity, this ratio can be NaN which is fine, in which case comparing u < Nan returns false
--           if logW > -Infinity and logW = -Infinity, this ratio can be Infinity, which is fine. -}
--       acceptance_ratio = exp (logP $ logW' - logW)
--   pure ((a, strace', logW'), acceptance_ratio)
