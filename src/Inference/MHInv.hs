{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Inference.MHInv where

import qualified Data.Map as Map
import Data.Set ((\\))
import qualified Data.Set as Set
import Data.Maybe
import Prog
import Trace
import LogP
import PrimDist
import Effects.Dist
import Effects.Lift
import qualified Inference.SIM as SIM
import Sampler

-- -- | Perform one iteration of MH by drawing a new sample and then rejecting or accepting it.
-- mhStep
--   :: Prog [Observe, Sample, Lift Sampler] a
--   -- | tags indicating sample sites of interest
--   -> [Tag]
--   -- | a mechanism for accepting proposals
--   -> Accept p a
--   -- | trace of previous MH results
--   -> [((a, p), InvSTrace)]
--   -- | updated trace of MH results
--   -> Sampler [((a, p), InvSTrace)]
-- mhStep prog tags accepter trace = do
--   -- Get previous MH output
--   let mhCtx@(_, strace) = head trace
--   -- Get possible addresses to propose new samples for
--   let sampleSites = Map.keys (if Prelude.null tags then strace else filterTrace tags strace)
--   -- Draw a proposal sample address
--   α_samp_ind <- sample $ UniformD 0 (length sampleSites - 1)
--   let (α_samp, _) = sampleSites !! α_samp_ind
--   -- Run MH with proposal sample address to get an MHCtx using LPTrace as its probability type
--   mhCtx'_lp <- runMH samples prog
--   -- Compute acceptance ratio to see if we use the proposed mhCtx' (which is mhCtx'_lp with 'LPTrace' converted to some type 'p')
--   (mhCtx', acceptance_ratio) <- accepter α_samp mhCtx mhCtx'_lp
--   u <- sample (Uniform 0 1)
--   if u < acceptance_ratio
--     then do return (mhCtx':trace)
--     else do return trace

-- | Handler for one iteration of MH
runMH ::
  -- | sample trace of previous MH iteration
     InvSTrace
  -- | sample address of interest
  -> Prog [Observe, Sample, Lift Sampler] a
  -- | ((model output, sample trace), log-probability trace)
  -> Sampler ((a, LPTrace), InvSTrace)
runMH strace  = handleLift . handleSamp strace . SIM.handleObs . traceLogProbs

handleSamp ::
  -- | Sample trace
     InvSTrace
  -> Prog  [Sample, Lift Sampler] a
  -> Prog '[Lift Sampler] (a, InvSTrace)
handleSamp strace (Val x)   = pure (x, strace)
handleSamp strace (Op op k) = case discharge op of
    Right (Sample (PrimDistPrf d) α) ->
      case Map.lookup α strace of
          Nothing -> do r <- lift sampleRandom
                        y <- lift (sampleInv d r)
                        k' (Map.insert α r strace) y
          Just r  -> do y <- lift (sampleInv d r)
                        k' strace y
    Left op' -> Op op' (k' strace)

  where k' strace' = handleSamp strace' . k

{- | The result of a single MH iteration, where @a@ is the type of model output and
     @p@ is some representation of probability.
-}
type MHCtx p a = ((a, p), InvSTrace)

{- | An abstract mechanism for computing an acceptance probability, where @a@ is the
     type of model output and @p@ is some representation of probability.
-}
type Accept p a =
  -- | proposal sample address
    Addr
  -- | previous MH ctx, parameterised by @p@
  -> MHCtx p a
  -- | proposed MH ctx, parameterised by a log-probability map
  -> MHCtx LPTrace a
  -- | (proposed MH ctx using probability representation p, Acceptance ratio)
  -> Sampler (MHCtx p a, Double)

-- | An acceptance mechanism for MH
acceptMH :: Accept LPTrace a
acceptMH x0 ((_, lptrace ), strace) ((a, lptrace'),  strace') = do
  let dom_logα = log (fromIntegral $ Map.size strace) - log (fromIntegral $ Map.size strace')
      sampled  = Set.singleton x0 `Set.union` (Map.keysSet strace \\ Map.keysSet strace')
      sampled' = Set.singleton x0 `Set.union` (Map.keysSet strace' \\ Map.keysSet strace)
      logα     = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace))
                         0 (Map.keysSet lptrace \\ sampled)
      logα'    = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace'))
                         0 (Map.keysSet lptrace' \\ sampled')
  pure (((a, lptrace'), strace'), (exp . unLogP) (dom_logα + logα' - logα))
