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
module Extensible.Inference.MH where

import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Kind (Type)
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import Data.Maybe
-- import Data.Extensible hiding (Member)
import Extensible.OpenProduct
import Control.Monad
import Control.Monad.Trans.Class
import Extensible.Dist
import Extensible.Freer
import Extensible.Model hiding (runModelFree)
import Extensible.Sampler
import qualified Extensible.OpenSum as OpenSum
import Extensible.OpenSum (OpenSum(..))
import Extensible.AffineReader
import Extensible.State
import GHC.Natural
import GHC.TypeLits (Nat)
import qualified GHC.TypeLits as TL
import Unsafe.Coerce

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
          b) Any sampled variables in Ⲭ' that aren't in Ⲭ.
   iii) The old joint probability of all the variables except:
          a) The newly sampled x0, and
          b) Any sampled variables in Ⲭ that aren't in Ⲭ'.
   Components ii) and iii) ensure that we are only comparing the probabilities of variables that they have both had to use. Variable x0 is not included in this because we want to see how it affects the probability of the rest of the program.
   The acceptance ratio is then: i) * ii) / iii)
-}

type Vals = '[Int, Double, Bool]

type LogP = Map Addr Double
type Ⲭ    = Map Addr (DistInfo, OpenSum Vals)
type TraceMH a = [(a, Ⲭ, LogP)]

updateMapⲬ :: OpenSum.Member x Vals => Addr -> Dist x -> x -> Ⲭ -> Ⲭ
updateMapⲬ α d x = Map.insert α (toDistInfo d, OpenSum.inj x) :: Ⲭ -> Ⲭ

updateLogP :: Addr -> Dist x -> x -> LogP -> LogP
updateLogP α d x  = Map.insert α (logProb d x)

-- | Compute acceptance probability
-- If the log prob from our new samples is better than our old samples, then we always accept.
-- If the log prob from our new samples is worse than our old samples, then we sometimes accept.
accept :: Addr -> Ⲭ -> Ⲭ -> LogP -> LogP -> IO Double
accept x0 _Ⲭ _Ⲭ' logℙ logℙ' = do
  let _X'sampled = Set.singleton x0 `Set.union` (Map.keysSet _Ⲭ' \\ Map.keysSet _Ⲭ)
      _Xsampled  = Set.singleton x0 `Set.union` (Map.keysSet _Ⲭ \\ Map.keysSet _Ⲭ')
  -- putStrLn $ " Xsampled is " ++ show _Xsampled
  -- putStrLn $ " X'sampled is " ++ show  _X'sampled
  let dom_logα   = log (fromIntegral $ Map.size _Ⲭ) - log (fromIntegral $ Map.size _Ⲭ')
  -- putStrLn $ " dom_logα is " ++ show dom_logα
  let _Xlogα     = foldl (\logα v -> logα + fromJust (Map.lookup v logℙ))
                         0 (Map.keysSet logℙ \\ _Xsampled)
  -- putStrLn $ " Xlogα is " ++ show _Xlogα ++ " from " ++ show (Map.keysSet logℙ \\ _Xsampled)
  let _X'logα    = foldl (\logα v -> logα + fromJust (Map.lookup v logℙ'))
                         0 (Map.keysSet logℙ' \\ _X'sampled)
  -- putStrLn $ " X'logα is " ++ show _X'logα ++ " from " ++ show (Map.keysSet logℙ' \\ _X'sampled)
  -- putStrLn $ " X'logα - Xlogα is " ++ show (_X'logα - _Xlogα)
  return $ exp (dom_logα + _X'logα - _Xlogα)

-- | Run MH for multiple data points
mh :: (es ~ '[AffReader (AsList env), Dist, Observe, State Ⲭ, State LogP, Sample])
   => Int                              -- Number of mhSteps per data point
   -> (b -> Model env es a)              -- Model awaiting input variable
   -> [b]                              -- List of model input variables
   -> [LRec env]                         -- List of model observed variables
   -> Sampler (TraceMH a)                -- Trace of all accepted outputs, samples, and logps
mh n model xs ys = do
  -- Perform initial run of mh
  (x, samples, logps) <- runMH (head ys) Map.empty 0 (model $ head xs)
  -- Construct list of mhNstep functions, one for each data point
  let mhs  = zipWith (\x y -> mhNsteps n y (model x)) xs ys
  -- Perform mhNstep for each data point, propagating (x, samples, logps) through
  l <- foldl (>=>) return mhs [(x, samples, logps)]
  -- Return mhTrace in correct order of execution (due to mhStep prepending new results onto head of trace)
  return $ reverse l

-- | Perform n steps of MH for a single data point
mhNsteps :: (es ~ '[AffReader (AsList env), Dist, Observe, State Ⲭ, State LogP, Sample])
  => Int              -- Number of mhSteps
  -> LRec env         -- Model observed variable
  -> Model env es a   -- Model
  -> TraceMH a        -- Previous mh output
  -> Sampler (TraceMH a)
mhNsteps n env model trace = do
  foldl (>=>) return (replicate n (mhStep env model)) trace

-- | Perform one step of MH for a single data point
mhStep :: (es ~ '[AffReader (AsList env), Dist, Observe, State Ⲭ, State LogP, Sample])
  => LRec env         -- Model observed variable
  -> Model env es a   -- Model
  -> TraceMH a        -- Trace of previous mh outputs
  -> Sampler (TraceMH a)
mhStep env model trace = do
  let -- Get previous mh output
      (x, samples, logps) = head trace
      sample_size = Map.size samples
  -- liftS $ print $ "samples are " ++ show samples
  -- α_samp <- sample $ DiscrUniformDist 0 2 Nothing
  α_samp <- sample $ DiscreteDist (map (,1.0/fromIntegral sample_size) (Map.keys samples)) Nothing
  -- run mh with new sample address
  -- liftS $ print $ "sample address is " ++ show α_samp
  (x', samples', logps') <- runMH env samples α_samp model
  -- liftS $ print $ "Second run is: " ++ show (x', samples', logps')
  -- do some acceptance ratio to see if we use samples or samples'
  acceptance_ratio <- liftS $ accept α_samp samples samples' logps logps'
  u <- sample (UniformDist 0 1 Nothing)
  if u < acceptance_ratio
    then do liftS $ putStrLn $ "Accepting " -- ++ show logps' ++ "\nover      "
            --  ++ show logps
              ++ "\nwith α" ++ show α_samp ++ ": "
              ++ show acceptance_ratio ++ " > " ++ show u
            return ((x', samples', logps'):trace)
    else do liftS $ putStrLn $ "Rejecting " -- ++ show logps' ++ "\nover      "
            --  ++ show logps
              ++ "\nwith α" ++ show α_samp ++ ": "
              ++ show acceptance_ratio ++ " < u: " ++ show u
            return trace

-- | Run model once under MH
runMH :: (es ~ '[AffReader (AsList env), Dist, Observe, State Ⲭ, State LogP, Sample])
  => LRec env       -- Model observed variable
  -> Ⲭ              -- Previous mh sample set
  -> Addr           -- Sample address
  -> Model env es a -- Model
  -> Sampler (a, Ⲭ, LogP)
runMH env samples α_samp m = do
  (((a, ys), samples'), logps') <-
                            ( -- This is where the previous run's samples are actually reused.
                              -- We do not reuse logps, we simply recompute them.
                              runSample α_samp samples
                            . runState Map.empty
                            . runState Map.empty
                            . runObserve
                            . transformMH
                            . runDist
                            . runAffReader env
                            . runModel) m
  return (a, samples', logps')

-- | Insert stateful operations for Ⲭ and LogP when either Sample or Observe occur.
transformMH :: (Member (State Ⲭ) rs, Member (State LogP) rs, Member Sample rs, Member Observe rs) => Freer rs a -> Freer rs a
transformMH = loop
  where
  loop :: (Member (State Ⲭ) rs, Member (State LogP) rs, Member Sample rs, Member Observe rs) => Freer rs a -> Freer rs a
  loop (Pure x) = return x
  loop (Free u k) = do
    case u of
      Samp d α
        -> case d of
              -- We can unsafe coerce x here, because we've inferred the type of x from the distribution's type
              DistDouble (Just d) -> Free u (\x -> modify (updateMapⲬ α d (unsafeCoerce x)) >>
                                            modify (updateLogP α d (unsafeCoerce x)) >>
                                            loop (k x))
              DistBool (Just d)   -> Free u (\x -> modify (updateMapⲬ α d (unsafeCoerce x)) >>
                                            modify (updateLogP α d (unsafeCoerce x)) >>
                                            loop (k x))
              DistInt (Just d)    -> Free u (\x -> modify (updateMapⲬ α d (unsafeCoerce x)) >>
                                            modify (updateLogP α d (unsafeCoerce x)) >>
                                            loop (k x))
      Obs d y α
        -> Free u (\x -> modify (updateLogP α d y  :: LogP -> LogP) >>
                         loop (k x))
      _ -> Free u (loop . k)

-- | Remove Observe occurrences from tree (log p is taken care of by transformMH)
-- runObserve :: Freer (Observe : rs) a -> Freer rs a
-- runObserve  = loop
--   where
--   loop :: Freer (Observe : rs) a -> Freer rs a
--   loop (Pure x) = return x
--   loop (Free u k) = case decomp u of
--     Right (Observe d y α)
--       -> loop (k y)
--     Left  u'  -> Free u' (loop . k)
runObserve :: Member Sample rs => Freer (Observe : rs) a -> Freer rs a
runObserve = loop 0
  where
  loop :: Member Sample rs => Double -> Freer (Observe : rs) a -> Freer rs a
  loop p (Pure x) = return x
  loop p (Free u k) = do
    case decomp u of
      Right (Observe d y α)
        -> case d of
            DistBool (Just d) ->
              do let p' = prob d (unsafeCoerce y :: Bool)
                --  prinT $ "Prob of observing " ++ show (unsafeCoerce y :: Bool) ++ " from " ++ show d ++ " is " ++ show p'
                 loop (p + p') (k y)
            DistDouble (Just d) ->
              do  let p' = prob d (unsafeCoerce y :: Double)
                  -- prinT $ "Prob of observing " ++ show (unsafeCoerce y :: Double) ++ " from " ++ show d ++ " is " ++ show p'
                  loop (p + p') (k y)
            DistInt (Just d) ->
              do let p' = prob d (unsafeCoerce y :: Int)
                --  prinT $ "Prob of observing " ++ show (unsafeCoerce y :: Int) ++ " from " ++ show d ++ " is " ++ show p'
                 loop (p + p') (k y)
            _ -> undefined
      Left  u'  -> Free u' (loop p . k)

-- | Run Sample occurrences
runSample :: Addr -> Ⲭ -> Freer '[Sample] a -> Sampler a
runSample α_samp samples = loop
  where
  loop :: Freer '[Sample] a -> Sampler a
  loop (Pure x) = return x
  loop (Free u k) = do
    case prj u of
      Just (Printer s) ->
       liftS (putStrLn s) >> loop (k ())
      Just (Sample d α) ->
        case d of
          DistDouble (Just d) ->
            case lookupSample samples d α α_samp of
              Nothing -> do
                x <- sample d
                --liftS (putStrLn $ "Drawing new sample for α" ++ show α ++ " x: " ++ show x)
                (loop . k . unsafeCoerce) x
              Just x  -> do
               -- liftS (putStrLn $ "Using old sample for α" ++ show α ++ " x: " ++ show x)
                (loop . k . unsafeCoerce) x
          DistBool (Just d) ->
            case lookupSample samples d α α_samp of
              Nothing -> sample d >>= loop . k . unsafeCoerce
              Just x  -> (loop . k . unsafeCoerce) x
          DistInt (Just d) ->
            case lookupSample samples d α α_samp of
              Nothing -> sample d >>= loop . k . unsafeCoerce
              Just x  -> (loop . k . unsafeCoerce) x
      _  -> error "Impossible: Nothing cannot occur"

-- | Lookup a sample address α's value in Ⲭ.
-- Return Nothing if: 1) it doesn't exist, 2) the sample address is the same as the current sample site α_samp, or 3) the sample we're supposed to reuse belongs to either a different distribution or the same distribution with different parameters (due to a new sampled value affecting its parameters). These all indicate that a new value should be sampled.
lookupSample :: OpenSum.Member a '[Int, Double, Bool] => Ⲭ -> Dist a -> Addr -> Addr -> Maybe a
lookupSample samples d α α_samp
  | α == α_samp = Nothing
  | otherwise   = Map.lookup α samples >>= \(d_info, x) ->
                  if toDistInfo d == d_info then OpenSum.prj x else Nothing
