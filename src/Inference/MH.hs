{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module Inference.MH 
  ( -- * Inference wrapper functions
      mh
    , mhInternal
    , mhStep
    -- * Inference handlers
    , runMH
    , handleSamp
    -- * Auxiliary functions and definitions
    , lookupSample
    , MHCtx
    , Accept
    , acceptMH
  ) where

import Control.Monad ( (>=>) )
import Data.Kind (Type)
import Data.Map (Map)
import Data.Maybe ( fromJust )
import Data.Set (Set, (\\))
import Effects.Dist ( Addr, Sample(..), Observe, Dist, Tag )
import Effects.Lift ( handleLift, Lift, lift )
import Effects.ObsReader ( ObsReader )
import Env ( Env, ContainsVars(..), Vars )
import Inference.SIM (handleObs)
import Model ( Model, handleCore )
import OpenSum (OpenSum(..))
import PrimDist
    ( ErasedPrimDist(..),
      PrimVal,
      PrimDist(Uniform, UniformD),
      sample,
      pattern PrimDistPrf )
import Prog ( discharge, Prog(..) )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified OpenSum
import Sampler ( Sampler )
import Trace
    ( traceLPs,
      traceSamples,
      filterSTrace,
      LPTrace,
      FromSTrace(..),
      STrace )
import Unsafe.Coerce ( unsafeCoerce )

-- | Metropolis-Hastings (MH) inference
mh :: forall env es a xs. (FromSTrace env, env `ContainsVars` xs)
  -- | Number of MH iterations
  => Int
  -- | Model
  -> Model env [ObsReader env, Dist, Lift Sampler] a
  -- | Input model environment 
  -> Env env
  -- | An optional list of observable variable names to specify sample sites of interest 
      {- For example, for interest in sampling `#mu`, provide `#mu <#> vnil`. 
         This causes other variables to not be resampled unless necessary. -}
  -> Vars xs
  -- | Trace of output model environments, one for each MH iteration
  -> Sampler [Env env]
mh n model env obsvars  = do
  -- Handle model to probabilistic progrma
  let prog = handleCore env model
      tags = varsToStrs @env obsvars
  -- Run MH for n iterations
  mhTrace <- mhInternal n prog Map.empty tags
  -- Convert each iteration's sample trace to a model environment
  pure (map (fromSTrace . snd . fst) mhTrace)

-- | MH on a probabilistic program
mhInternal ::
   -- | Number of MH iterations
      Int                             
   -- | Probabilistic program
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

-- | Draw a new sample from one iteration of MH and then decide to reject or accept it.
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
  α_samp_ind <- sample $ UniformD 0 (Map.size sampleSites - 1)
  let (α_samp, _) = Map.elemAt α_samp_ind sampleSites
  -- Run MH with proposal sample address to get an MHCtx using LPTrace as its probability type
  mhCtx'_lp <- runMH samples α_samp model
  -- Compute acceptance ratio to see if we use the proposed mhCtx' (which is mhCtx'_lp with 'LPTrace' converted to some type 'p')
  (mhCtx', acceptance_ratio) <- accepter α_samp mhCtx'_lp mhCtx
  u <- sample (Uniform 0 1)
  if u < acceptance_ratio
    then do return (mhCtx':trace)
    else do return trace

-- | Handler for one iteration of MH on probabilistic program
runMH :: 
  -- | Sample trace of previous MH iteration
     STrace
  -- | Sample address of interest
  -> Addr
  -- | Probabilistic program
  -> Prog [Observe, Sample,  Lift Sampler] a
  -> Sampler ((a, STrace), LPTrace)
runMH strace α_samp = handleLift . handleSamp strace α_samp . handleObs . traceLPs . traceSamples

-- | Handler for selective sampling during MH
handleSamp :: 
  -- | Sample trace
     STrace 
  -- | Address of the proposal sample site for the current MH iteration
  -> Addr 
  -- | Probabilistic program with just Sample
  -> Prog [Sample, Lift Sampler] a 
  -> Prog '[Lift Sampler] a
handleSamp strace α_samp (Op op k) = case discharge op of
    Right (Sample (PrimDistPrf d) α) ->
      let maybe_y = if α == α_samp then Nothing else lookupSample α d strace 
      in  case maybe_y of
                  Nothing -> lift (sample d) >>= k'
                  Just x  -> k' x
    Left op' -> Op op' k'
  where k' = handleSamp strace α_samp . k
handleSamp _ _ (Val x) = return x

-- | For a given address, look up a sampled value from a sample trace, returning
--   it only if the primitive distribution it was sampled from matches the current one.
lookupSample :: OpenSum.Member a PrimVal =>
  -- | Address of sample site
     Addr 
  -- | Distribution to sample from
  -> PrimDist a 
  -- | Sample trace
  -> STrace 
  -- | Possibly a looked-up sampled value
  -> Maybe a
lookupSample α d strace   = do
    (ErasedPrimDist d', x) <- Map.lookup α strace
    if d == unsafeCoerce d'
      then OpenSum.prj x
      else Nothing

-- | The result of a single MH iteration, where @a@ is the type of model output and @p@ is some representation of probability.
type MHCtx p a = ((a, STrace), p)

-- | An abstract mechanism for computing an acceptance probability
type Accept p a = 
  -- | Proposal sample address
    Addr                       
  -- | Proposed mh ctx, parameterised by log probability map
  -> MHCtx LPTrace a            
  -- | Previous mh ctx, parameterised by arbitrary probability representation p
  -> MHCtx p a                   
  -- | (Proposed mh ctx using probability representation p, Acceptance ratio)
  -> Sampler (MHCtx p a, Double) 

-- | An acceptance mechanism for MH
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
