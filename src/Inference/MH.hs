{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Inference.MH where

import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Kind (Type)
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import Data.Maybe
import Env
import Control.Monad
import Effects.Dist
import Prog
import Model hiding (runModelFree)
import Sampler
import PrimDist
import Trace
import Effects.Lift
import qualified OpenSum as OpenSum
import OpenSum (OpenSum(..))
import Effects.ObsReader
import Effects.State
import Unsafe.Coerce
import Inference.SIM (handleObs)


-- ||| (Section 6.2.2) Metropolis-Hastings
mh :: FromSTrace env
  -- | Number of MH iterations
  => Int
  -- | A model
  -> Model env [ObsReader env, Dist, Lift Sampler] a
  -- | A model environment (containing observed values to condition on)
  -> Env env
  -- | An optional list of observable variable names (strings) to specify sample sites of interest (e.g. for interest in sampling #mu, provide "mu"). This causes other variables to not be resampled unless necessary.
  -> [Tag]
  -- | Trace of output environments, containing values sampled for each MH iteration
  -> Sampler [Env env]
mh n model env tags  = do
  -- Handle model to probabilistic progrma
  let prog = (handleDist . handleObsRead env) (runModel model)
  -- Run MH for n iterations
  mhTrace <- mhInternal n prog Map.empty tags
  -- Convert each iteration's sample trace to a model environment
  pure (map (fromSTrace . snd . fst) mhTrace)

-- ||| MH on a model interpreted to a probabilistic program
mhInternal ::
   -- | Number of MH iterations
      Int                             
   -- | A probabilistic program
   -> Prog [Observe, Sample, Lift Sampler] a                   
   -- | Initial sample trace   
   -> STrace                           
   -- | Tags indicating sample sites of interest
   -> [Tag]                          
   -- | Trace of all accepted outputs, samples, and logps  
   -> Sampler  [((a, STrace), LPTrace)]  
mhInternal n prog strace_0 tags = do
  -- Perform initial run of mh
  let α_0 = ("", 0)
  mhCtx_0 <- runMH strace_0 α_0 prog
  -- A function performing n mhSteps using initial mhCtx. 
  -- Note: most recent samples are at the front (head) of the trace
  foldl (>=>) pure (replicate n (mhStep prog tags acceptMH)) [mhCtx_0]

-- ||| Perform one step of MH
mhStep :: 
  -- | Probabilistic program
     Prog [Observe, Sample,  Lift Sampler] a
  -- | Tags indicating sample sites of interest
  -> [Tag]
  -- | A mechanism for accepting proposals
  -> Accept p a
  -- | Trace of previous MH outputs
  -> [((a, STrace), p)]
  -- | Updated trace of MH outputs
  -> Sampler [((a, STrace), p)]
mhStep model tags accepter trace = do
  -- Get previous MH output
  let mhCtx@((_, samples), _) = head trace
  -- Get possible addresses to propose new samples for
  let sampleSites = if null tags then samples else filterSTrace tags samples
  -- Draw a proposal sample address
  α_samp_ind <- sample $ DiscrUniformDist 0 (Map.size sampleSites - 1)
  let (α_samp, _) = Map.elemAt α_samp_ind sampleSites
  -- Run MH with proposal sample address to get an MHCtx using LPTrace as its probability type
  mhCtx'_lp <- runMH samples α_samp model
  -- Compute acceptance ratio to see if we use the proposed mhCtx' (which is mhCtx'_lp with 'LPTrace' converted to some type 'p')
  (mhCtx', acceptance_ratio) <- accepter α_samp mhCtx'_lp mhCtx
  u <- sample (UniformDist 0 1)
  if u < acceptance_ratio
    then do return (mhCtx':trace)
    else do return trace

-- ||| MH handler
runMH :: 
  -- | Sample trace of previous MH iteration
     STrace
  -- | Sample address of interest
  -> Addr
  -- | Probabilistic program
  -> Prog [Observe, Sample,  Lift Sampler] a
  -> Sampler ((a, STrace), LPTrace)
runMH strace α_samp = handleLift . handleSamp strace α_samp . handleObs . traceLPs . traceSamples

-- ||| Selectively sample
handleSamp :: STrace -> Addr -> Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
handleSamp strace α_samp (Op op k) = case discharge op of
  Right (Sample (PrimDistDict d) α) ->
        do  let maybe_y = if α == α_samp then Nothing else lookupSample strace d α
            case maybe_y of
                Nothing -> lift (sample d) >>= (handleSamp strace α_samp . k)
                Just x  -> (handleSamp strace α_samp . k) x
  Left op' -> Op op' (handleSamp strace α_samp . k)
handleSamp _ _ (Val x) = return x

-- ||| Look up the previous iteration's sampled value for an address, returning
--     it only if the primitive distribution it was sampled from matches the current one.
lookupSample :: Show a => OpenSum.Member a PrimVal => STrace -> PrimDist a -> Addr -> Maybe a
lookupSample strace d α = do
    (ErasedPrimDist d', x) <- Map.lookup α strace
    if d == unsafeCoerce d'
      then OpenSum.prj x
      else Nothing

-- ||| The result of a single MH iteration: (An output, A sample trace, A representation of the iteration's probability) 
type MHCtx p a = ((a, STrace), p)

-- ||| An abstract mechanism for computing an acceptance probability
type Accept p a = 
  -- | Proposal sample address
    Addr                       
  -- | Proposed mh ctx, parameterised by log probability map
  -> MHCtx LPTrace a            
  -- | Previous mh ctx, parameterised by arbitrary probability representation p
  -> MHCtx p a                   
  -- | (Proposed mh ctx using probability representation p, Acceptance ratio)
  -> Sampler (MHCtx p a, Double) 

-- ||| MH mechanism for computing acceptance probability
acceptMH :: Accept LPTrace a
acceptMH x0 ((a, strace'), lptrace') ((_, strace), lptrace)  = do
  let sampled' = Set.singleton x0 `Set.union` (Map.keysSet strace' \\ Map.keysSet strace)
      sampled  = Set.singleton x0 `Set.union` (Map.keysSet strace \\ Map.keysSet strace')
      dom_logα = log (fromIntegral $ Map.size strace) - log (fromIntegral $ Map.size strace')
      logα     = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace))
                         0 (Map.keysSet lptrace \\ sampled)
      logα'    = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace'))
                         0 (Map.keysSet lptrace' \\ sampled')
  pure (((a, strace'), lptrace'), exp (dom_logα + logα' - logα))
