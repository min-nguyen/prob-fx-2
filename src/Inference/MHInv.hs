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

handleSamp ::
  -- | Sample trace
     STraceInv
  -- | Address of the proposal sample site for the current MH iteration
  -> Addr
  -> Prog [Sample, Lift Sampler] a
  -> Prog '[Lift Sampler] a
handleSamp _ _ (Val x) = return x
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
