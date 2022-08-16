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
import PrimDist
import Effects.Lift
import qualified Data.Map as Map
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

