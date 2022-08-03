{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Inference.MHInv where

import Data.Map as Map
import Prog
import Trace
import PrimDist
import Effects.Dist
import Effects.Lift
import Sampler
import qualified Inference.MH as MH

handleSamp ::
  -- | Sample trace
     InvSTrace
  -- | Address of the proposal sample site for the current MH iteration
  -> Addr
  -> Prog  [Sample, Lift Sampler] a
  -> Prog '[Lift Sampler] (a, InvSTrace)
handleSamp strace _ (Val x) = pure (x, strace)
handleSamp strace α_samp (Op op k) = case discharge op of
    Right (Sample (PrimDistPrf d) α) ->
      case Map.lookup α strace of
          Nothing -> do r <- lift sampleRandom
                        y <- lift $ sampleInv d r
                        k' y
          Just r  -> do y <- lift (sampleInv d r)
                        k' y
    Left op' -> Op op' k'
  where k' = handleSamp strace α_samp . k



-- | Handler for one iteration of MH
runMH ::
  -- | sample trace of previous MH iteration
     InvSTrace
  -- | sample address of interest
  -> Addr
  -> Prog [Observe, Sample, Lift Sampler] a
  -- | ((model output, sample trace), log-probability trace)
  -> Sampler ((a, LPTrace), InvSTrace)
runMH strace α_samp = handleLift . handleSamp strace α_samp . MH.handleObs . traceLogProbs
