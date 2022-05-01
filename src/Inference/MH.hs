{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TypeFamilies #-}
module Inference.MH where

import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Kind (Type)
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import Data.Maybe
-- import Data.Extensible hiding (Member)
import ModelEnv
import Control.Monad
import Effects.Lift
import Effects.Dist
import Data.Bifunctor
import Freer
import Model hiding (runModelFree)
import Sampler
import Trace
import qualified OpenSum as OpenSum
import OpenSum (OpenSum(..))
import Effects.ObsReader
import Effects.State
import GHC.Natural
import GHC.TypeLits (Nat)
import qualified GHC.TypeLits as TL
import Unsafe.Coerce
import Util
{-
MH(x0, Ⲭ, LogP):
Perform initial run of MH.

Repeat:
1) Choose the sample site x0 uniformly from all possible sample addresses (keys of Ⲭ)

2) let Ⲭ'    = Map.empty;
       LogP' = Map.empty;

3)  Evaluate the program:
    -# x <- sample d
      If 1) x == x0, or
         2) x is not found in Ⲭ, or
         3) x is found in Ⲭ but the distribution it was sampled from was different
      Then 1) Newly sample x and store it in Ⲭ' (along with its distribution), and
           2) Compute logp = logP(d, x) and store logp in LogP'
      Else reuse the sample for x found in Ⲭ

    -# observe d y
      Compute logp = logP(d, y) and store logp in LogP'

4) Compute acceptance ratio using x0, Ⲭ, Ⲭ', LogP, LogP'
   There are 3 components:
   i)   The relative probability of selecting the initial site x0.
        This is |Ⲭ|/|Ⲭ'|, in other words, the size of Ⲭ over the size of Ⲭ'.
   ii)  The new joint probability of all the variables except:
          a) The newly sampled x0, and
          b) Any sampled variables in Ⲭ' that aren'e in Ⲭ.
   iii) The old joint probability of all the variables except:
          a) The newly sampled x0, and
          b) Any sampled variables in Ⲭ that aren'e in Ⲭ'.
   Components ii) and iii) ensure that we are only comparing the probabilities of variables that they have both had to use. Variable x0 is not included in this because we want to see how it affects the probability of the rest of the program.
   The acceptance ratio is then: i) * ii) / iii)
-}

{-
Currently we are:
  1) Taking a list of model inputs "x_0:xs" and a list of observed values "env_0:envs"
  2) Performing an initial model execution on x_0 and env_0 (with no resample address), to build up an initial trace of a sample map and log probability map
  3) Creating a function "runN" that performs n mhSteps for a single pair of model input "x" and observed value "env", creating a trace of n sample maps and log probability maps for that single data point.
  4) Constructing a list of functions that perform n mhSteps, one function per data point, and then passing in an initial trace.

To instead perform Metropolis-Hastings on a model where we only resample after considering/scoring on all of the data, then the model would have to be designed this way.
For example, instead of:

  linearRegression :: forall env rs .
    Observables env '["y", "m", "c", "σ"] Double =>
    Double -> Model env rs (Double, Double)
  linearRegression x = do
    m <- normal' 0 3 #m
    c <- normal' 0 5 #c
    σ <- uniform' 1 3 #σ
    y <- normal' (m * x + c) σ #y
    return (x, y)

We would do:

  linearRegression :: forall env rs .
    Observables env '["y", "m", "c", "σ"] Double =>
    [Double] -> Model env rs ([Double], [Double])
  linearRegression xs = do
    m <- normal' 0 3 #m
    c <- normal' 0 5 #c
    σ <- uniform' 1 3 #σ
    let loop (x:xs) ys = do
            y <- normal' (m * x + c) σ #y
            loop xs (ys ++ [y])
        loop [] ys     = return ys
    ys <- loop xs []
    return (xs, ys)

This is already natural for models such as a HMM.
-}

type MHTrace p a = [(a, SDTrace, p)]

type MHCtx p a = (a, SDTrace, p)

type Accept p a = Addr                        -- proposal sample address
               -> MHCtx LPTrace a             -- proposed mh ctx, parameterised by log probability map
               -> MHCtx p a                   -- previous mh ctx, parameterised by arbitrary probability representation p
               -> Sampler (MHCtx p a, Double) -- (proposed mh ctx using probability representation p, acceptance ratio)

-- | Compute acceptance probability
-- If the log prob from our new samples is better than our old samples, then we always accept.
-- If the log prob from our new samples is worse than our old samples, then we sometimes accept.
-- Encountering a probability of 0 (i.e. log probability of -infinity) from any individual sample means that the computed probability of the sample map becomes 0. This results in that sample map being rejected. Performing `exp` on `-Infinity` in Haskell produces 0.
acceptMH :: Accept LPTrace a
acceptMH x0 (a, strace', lptrace') (_, strace, lptrace)  = do
  let sampled' = Set.singleton x0 `Set.union` (Map.keysSet strace' \\ Map.keysSet strace)
      sampled  = Set.singleton x0 `Set.union` (Map.keysSet strace \\ Map.keysSet strace')
      dom_logα = log (fromIntegral $ Map.size strace) - log (fromIntegral $ Map.size strace')
      logα     = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace))
                         0 (Map.keysSet lptrace \\ sampled)
      logα'    = foldl (\logα v -> logα + fromJust (Map.lookup v lptrace'))
                         0 (Map.keysSet lptrace' \\ sampled')
  return ((a, strace', lptrace'), exp (dom_logα + logα' - logα))

-- | Run MH for one input and environment
mhTopLevel :: forall es a b env e. (es ~ '[ObsReader env, Dist, Lift Sampler], FromSTrace env)
   => Int                                   -- Number of mhSteps
   -> Model env es a                        -- Model awaiting input
   -> ModelEnv env                          -- List of model observed variables
   -> [Tag]                                 -- Tags indicated sample sites of interest
   -> Sampler [(a, ModelEnv env, LPTrace)]  -- Trace of all accepted outputs, samples, and logps
mhTopLevel n model env tags  = do
  let prog = (runDist . runObsReader env) (runModel model)
  mhTrace <- mh n prog Map.empty tags
  return (map (mapsnd3 (fromSDTrace @env)) mhTrace)

mh :: (es ~ '[Observe, Sample, Lift Sampler])
   => Int                              -- Number of mhSteps
   -> Prog es a                        -- Model consisting of sample and observe operations
   -> SDTrace                          -- Initial sample trace
   -> [Tag]                            -- Tags indicated sample sites of interest
   -> Sampler (MHTrace LPTrace a)      -- Trace of all accepted outputs, samples, and logps
mh n prog strace_0 tags = do
  -- Perform initial run of mh
  let α_0 = ("", 0)
  mhCtx_0 <- runMH strace_0 α_0 prog
  -- A function performing n mhSteps
  let mhs = foldl (>=>) return (replicate n (mhStep prog tags acceptMH))
  -- Perform n mhSteps using initial mhCtx
  l <- mhs [mhCtx_0]
  -- Return mhTrace in correct order of execution (due to mhStep prepending new results onto head of trace)
  return $ reverse l

-- | Perform one step of MH for a single data point
mhStep :: (es ~ '[Observe, Sample, Lift Sampler])
  => Prog es a        -- Model consisting of sample and observe operations
  -> [Tag]            -- Tags indicating sample sites of interest
  -> Accept p a       -- Accept mechanism
  -> MHTrace p a      -- Trace of previous mh outputs
  -> Sampler (MHTrace p a)
mhStep prog tags accepter mhTrace  = do
  let -- get previous mh output
      mhCtx@(_, samples, _) = head mhTrace
      sampleSites = if null tags then samples else Map.filterWithKey (\(tag, i) _ -> tag `elem` tags) samples
  -- select proposal sample address
  α_samp_ind <- sample (DiscrUniformDist 0 (Map.size sampleSites - 1) Nothing Nothing)
  let (α_samp, _) = Map.elemAt α_samp_ind sampleSites
  -- run mh with proposal sample address to get an MHCtx using LPTrace as its probability
  mhLPCtx' <- runMH samples α_samp prog
  -- compute acceptance ratio to see if we use new mhCtx' or retain mhCtx
  (mhCtx', acceptance_ratio) <- accepter α_samp mhLPCtx' mhCtx
  u <- sample (UniformDist 0 1 Nothing Nothing)
  if u < acceptance_ratio
    then return (mhCtx':mhTrace)
    else return mhTrace

-- | Run model once under MH
runMH :: (es ~ '[Observe, Sample, Lift Sampler])
  => SDTrace   -- Previous mh sample set
  -> Addr      -- Sample address
  -> Prog es a -- Model consisting of sample and observe operations
  -> Sampler (a, SDTrace, LPTrace)
runMH strace α_samp prog = do
  ((a, strace'), lptrace') <-
                            ( runLift
                            . runSample α_samp strace
                            . runObserve
                            . traceLPs
                            . traceDSamples) prog
  return (a, strace', lptrace')

-- | Remove Observe occurrences from tree (log p is taken care of by transformMH)
runObserve :: Member Sample es => Prog (Observe : es) a -> Prog es a
runObserve = loop 0
  where
  loop :: Member Sample es => Double -> Prog (Observe : es) a -> Prog es a
  loop p (Val x)   = return x
  loop p (Op u k) = case u of
      ObsPatt d y α ->
        do let p' = prob d y
          --  prinT $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show p'
           loop (p + p') (k y)
      DecompLeft u'  -> Op u' (loop p . k)

-- | Run Sample occurrences
runSample :: Addr -> SDTrace -> Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
runSample α_samp strace = loop
  where
  loop :: Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
  loop (Val x) = return x
  loop (Op u k) = case u of
      PrintPatt s ->
        lift (liftS (putStrLn s)) >> loop (k ())
      SampPatt d α -> do
        let maybe_y = if α == α_samp then Nothing else lookupSample strace d α
        case maybe_y of
             Nothing -> lift (sample d) >>= (loop . k)
             Just x  -> (loop . k) x
      DecompLeft u' ->
         Op u' (loop . k)

lookupSample :: Show a => OpenSum.Member a PrimVal => SDTrace -> Dist a -> Addr -> Maybe a
lookupSample strace d α  = do
    let m = Map.lookup α strace
    case m of
      Just (PrimDist d', x) -> do
        -- printS $ "comparing " ++ show d ++ " and " ++ show d' ++ " is " ++ show (d == unsafeCoerce d')
        if d == unsafeCoerce d'
           then do -- liftS $ print ("retrieving " ++ show x ++ " for " ++ show d')
                   OpenSum.prj x
            else Nothing
      Nothing -> Nothing



-- | Lookup a sample address α's value in Ⲭ.
-- Return Nothing if: 1) it doesn'e exist, 2) the sample address is the same as the current sample site α_samp, or 3) the sample we're supposed to reuse belongs to either a different distribution or the same distribution with different parameters (due to a new sampled value affecting its parameters). These all indicate that a new value should be sampled.
-- lookupSample :: OpenSum.Member a '[Int, Double, Bool] => Ⲭ -> Dist a -> Addr -> Addr -> Maybe a
-- lookupSample samples d α α_samp
--   | α == α_samp = Nothing
--   | otherwise   = Map.lookup α samples >>= \(d_info, x) ->
--                   if toDistInfo d == d_info then OpenSum.prj x else Nothing
