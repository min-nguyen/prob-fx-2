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
import STrace
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

type TraceMH a = [(a, SDTrace, LPTrace)]


-- | Compute acceptance probability
-- If the log prob from our new samples is better than our old samples, then we always accept.
-- If the log prob from our new samples is worse than our old samples, then we sometimes accept.
-- Encountering a probability of 0 (i.e. log probability of -infinity) from any individual sample means that the computed probability of the sample map becomes 0. This results in that sample map being rejected. Performing `exp` on `-Infinity` in Haskell produces 0.
accept :: Addr -> SDTrace -> SDTrace -> LPTrace -> LPTrace -> IO Double
accept x0 _Ⲭ _Ⲭ' logℙ logℙ' = do
  let _X'sampled = Set.singleton x0 `Set.union` (Map.keysSet _Ⲭ' \\ Map.keysSet _Ⲭ)
      _Xsampled  = Set.singleton x0 `Set.union` (Map.keysSet _Ⲭ \\ Map.keysSet _Ⲭ')
  -- putStrLn $ " Xsampled is " ++ show _Xsampled
  -- putStrLn $ " X'sampled is " ++ show  _X'sampled
  let dom_logα   = log (fromIntegral $ Map.size _Ⲭ) - log (fromIntegral $ Map.size _Ⲭ')
  -- putStrLn $ " dom_logα is " ++ show dom_logα
  let _Xlogα     = foldl (\logα v -> logα + fromJust (Map.lookup v logℙ))
                         0 (Map.keysSet logℙ \\ _Xsampled)
  -- putStrLn $ " Xlogα is " ++ show _Xlogα ++ " from " ++ show (logℙ)
  let _X'logα    = foldl (\logα v -> logα + fromJust (Map.lookup v logℙ'))
                         0 (Map.keysSet logℙ' \\ _X'sampled)
  -- putStrLn $ " X'logα is " ++ show _X'logα ++ " from " ++ show (logℙ')
  -- putStrLn $ " (X'logα, Xlogα) is " ++ show (_Xlogα , _X'logα)
  -- print $ "dom_logα + _X'logα - _Xlogα is " ++ (show $ dom_logα + _X'logα - _Xlogα)
  return $ exp (dom_logα + _X'logα - _Xlogα)

-- | Run MH for one input and environment
mh :: forall es a b env e. (es ~ '[ObsReader env, Dist, Lift Sampler], FromSTrace env)
   => Int                                   -- Number of mhSteps
   -> (b -> Model env es a)                 -- Model awaiting input
   -> [Tag]                                 -- Tags indicated sample sites of interest
   -> b                                     -- List of model input variables
   -> ModelEnv env                          -- List of model observed variables
   -> Sampler [(a, ModelEnv env, LPTrace)]  -- Trace of all accepted outputs, samples, and logps
mh n model tags x_0 env_0 = do
  -- Perform initial run of mh
  mhTrace <- mhWithSTrace n (runDist . runObsReader env_0 $ runModel (model x_0)) Map.empty tags
  return (map (mapsnd3 (fromSDTrace @env)) mhTrace)

mhWithSTrace :: (es ~ '[Observe, Sample, Lift Sampler])
   => Int                              -- Number of mhSteps
   -> Prog es a                        -- Model consisting of sample and observe operations
   -> SDTrace
   -> [Tag]                            -- Tags indicated sample sites of interest
   -> Sampler (TraceMH a)              -- Trace of all accepted outputs, samples, and logps
mhWithSTrace n prog samples_0 tags = do
  -- Perform initial run of mh
  let α_0 = ("", 0)
  (y_0, samples_0, logps_0) <- runMH samples_0 α_0 prog
  -- A function performing n mhsteps for one data point
  let mhs  = foldl (>=>) return (replicate n (mhStep prog tags))
  -- Construct list of n mhsteps, one for each data point
  -- Perform mhNstep for each data point, propagating (x, samples, logps) through
  l <- mhs [(y_0, samples_0, logps_0)]
  -- Return mhTrace in correct order of execution (due to mhStep prepending new results onto head of trace)
  return $ reverse l

-- | Perform one step of MH for a single data point
mhStep :: (es ~ '[Observe, Sample, Lift Sampler])
  => Prog es a        -- Model consisting of sample and observe operations
  -> [Tag]            -- Tags indicating sample sites of interest
  -> TraceMH a        -- Trace of previous mh outputs
  -> Sampler (TraceMH a)
mhStep model tags trace = do
  let -- Get previous mh output
      (x, samples, logps) = head trace
  -- α_samp <- sample $ DiscrUniformDist 0 2 Nothing
  let sampleSites = if null tags then samples
                    else  Map.filterWithKey (\(tag, i) _ -> tag `elem` tags) samples
  α_samp_ind <- sample (DiscrUniformDist 0 (Map.size sampleSites - 1) Nothing Nothing)
  -- liftS $ print $ "α_samp_ind is " ++ show α_samp_ind ++ " Map.size samples is " ++ show (Map.size samples)
  -- liftS $ print $ "sample ind is " ++ show α_samp_ind ++ "\n sample sites are " ++ show sampleSites
  let (α_samp, _) = Map.elemAt α_samp_ind sampleSites
  -- run mh with new sample address
  -- liftS $ print $ "sample address is " ++ show α_samp
  (x', samples', logps') <- runMH samples α_samp model
  -- liftS $ print $ "Second run is: " ++ show (x', samples', logps')
  -- do some acceptance ratio to see if we use samples or samples'
  acceptance_ratio <- liftS $ accept α_samp samples samples' logps logps'
  -- liftS $ print $ "acceptance ratio" ++ show acceptance_ratio
  u <- sample (UniformDist 0 1 Nothing Nothing)

  if u < acceptance_ratio
    then do -- liftS $ putStrLn $ "Accepting " -- ++ show (Map.lookup α_samp samples') -- ++ show logps' ++ "\nover      "
            -- ++ show logps
            -- ++ "\nwith α" ++ show α_samp ++ ": "
            -- ++ show acceptance_ratio ++ " > " ++ show u
            -- liftS $ print "accepting"
            return ((x', samples', logps'):trace)
    else do
            -- liftS $ putStrLn $ "Rejecting " ++ show (Map.lookup α_samp samples') -- ++ show logps' ++ "\nover      "
            -- ++ show logps
            --  ++ "\nwith α" ++ show α_samp ++ ": "
            --  ++ show acceptance_ratio ++ " < u: " ++ show u
            return trace

-- | Run model once under MH
runMH :: (es ~ '[Observe, Sample, Lift Sampler])
  => SDTrace   -- Previous mh sample set
  -> Addr      -- Sample address
  -> Prog es a -- Model consisting of sample and observe operations
  -> Sampler (a, SDTrace, LPTrace)
runMH samples α_samp m = do
  ((a, samples'), logps') <-
                            ( -- This is where the previous run's samples are actually reused.
                              -- We do not reuse logps, we simply recompute them.
                              runLift
                            . runSample α_samp samples
                            . runObserve
                            . runState Map.empty
                            . runState Map.empty
                            . transformMH) m
  return (a, samples', logps')

-- | Insert stateful operations for SDTrace and LPTrace when either Sample or Observe occur.
transformMH :: (Member Sample es, Member Observe es) =>
  Prog es a -> Prog (State SDTrace: State LPTrace: es) a
transformMH (Val x) = return x
transformMH (Op u k) = do
  case u of
    SampPatt d α
      -> Op (weaken $ weaken u) (\x -> updateSDTrace α d x >>
                                       updateLPTrace α d x >>
                                       transformMH (k x))
    ObsPatt d y α
      -> Op (weaken $ weaken u) (\x -> updateLPTrace α d x >>
                       transformMH (k x))
    _ -> Op (weaken $ weaken u) (transformMH . k)

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

data SampleMH a where
  SampleMH  :: Dist a -> Addr -> SampleMH a

-- | Run Sample occurrences
runSample :: Addr -> SDTrace -> Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
runSample α_samp samples = loop
  where
  loop :: Prog '[Sample, Lift Sampler] a -> Prog '[Lift Sampler] a
  loop (Val x) = return x
  loop (Op u k) = case u of
      PrintPatt s ->
        lift (liftS (putStrLn s)) >> loop (k ())
      SampPatt d α -> do
        let maybe_y = if α == α_samp then Nothing else lookupSample samples d α
        case maybe_y of
             Nothing -> lift (sample d) >>= (loop . k)
             Just x  -> (loop . k) x
      DecompLeft u' ->
         Op u' (loop . k)

lookupSample :: Show a => OpenSum.Member a PrimVal => SDTrace -> Dist a -> Addr -> Maybe a
lookupSample samples d α  = do
    let m = Map.lookup α samples
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
