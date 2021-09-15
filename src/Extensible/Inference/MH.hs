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


type SMap  = Map Addr (PrimDist, OpenSum PrimVal)
type LPMap = Map Addr Double
type TraceMH a = [(a, SMap, LPMap)]

updateSMap :: Show x => OpenSum.Member x PrimVal
  => Addr -> Dist x -> x -> SMap -> SMap
updateSMap α d x sMap = Map.insert α (PrimDist d, OpenSum.inj x) sMap

updateLPMap :: Addr -> Dist x -> x -> LPMap -> LPMap
updateLPMap α d x  = Map.insert α (logProb d x)

-- | Compute acceptance probability
-- If the log prob from our new samples is better than our old samples, then we always accept.
-- If the log prob from our new samples is worse than our old samples, then we sometimes accept.
accept :: Addr -> SMap -> SMap -> LPMap -> LPMap -> IO Double
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
  putStrLn $ " X'logα - Xlogα is " ++ show (_Xlogα , _X'logα)
  return $ exp (dom_logα + _X'logα - _Xlogα)

-- | Run MH for multiple data points
mh :: (ts ~ '[AffReader env, Dist, State SMap, State LPMap, Observe, Sample])
   => Int                              -- Number of mhSteps per data point
   -> (b -> Model env ts a)            -- Model awaiting input variable
   -> [Tag]                            -- Tags indicated sample sites of interest
   -> [b]                              -- List of model input variables
   -> [LRec env]                       -- List of model observed variables
   -> Sampler (TraceMH a)              -- Trace of all accepted outputs, samples, and logps
mh n model tags xs envs = do
  -- Perform initial run of mh
  let x_0 = head xs
      env_0 = head envs
      α_0 = ("", 0)
  (y_0, samples_0, logps_0) <- runMH env_0 Map.empty α_0 (model x_0)
  -- A function performing n mhsteps for one data point
  let runN x env = foldl (>=>) return (replicate n (mhStep env (model x) tags))
  -- Construct list of n mhsteps, one for each data point
  let mhs  = zipWith runN xs envs
  -- Perform mhNstep for each data point, propagating (x, samples, logps) through
  l <- foldl (>=>) return mhs [(y_0, samples_0, logps_0)]
  -- Return mhTrace in correct order of execution (due to mhStep prepending new results onto head of trace)
  return $ reverse l

-- | Perform one step of MH for a single data point
mhStep :: (ts ~ '[AffReader env, Dist, State SMap, State LPMap, Observe, Sample])
  => LRec env         -- Model observed variable
  -> Model env ts a   -- Model
  -> [Tag]            -- Tags indicating sample sites of interest
  -> TraceMH a        -- Trace of previous mh outputs
  -> Sampler (TraceMH a)
mhStep env model tags trace = do
  let -- Get previous mh output
      (x, samples, logps) = head trace
  -- α_samp <- sample $ DiscrUniformDist 0 2 Nothing
  let sampleSites = if null tags then samples
                    else  Map.filterWithKey (\(tag, i) _ -> tag `elem` tags) samples
  α_samp_ind <- sample $ DiscrUniformDist 0 (Map.size sampleSites - 1) Nothing Nothing
  -- liftS $ print $ "α_samp_ind is " ++ show α_samp_ind ++ " Map.size samples is " ++ show (Map.size samples)
  -- liftS $ print $ "sample ind is " ++ show α_samp_ind ++ "\n sample sites are " ++ show sampleSites
  let (α_samp, _) = Map.elemAt α_samp_ind sampleSites
  -- run mh with new sample address
  liftS $ print $ "sample address is " ++ show α_samp
  (x', samples', logps') <- runMH env samples α_samp model
  -- liftS $ print $ "Second run is: " ++ show (x', samples', logps')
  -- do some acceptance ratio to see if we use samples or samples'
  acceptance_ratio <- liftS $ accept α_samp samples samples' logps logps'
  u <- sample (UniformDist 0 1 Nothing Nothing)
  if u < acceptance_ratio
    then do liftS $ putStrLn $ "Accepting " ++ show (Map.lookup α_samp samples') -- ++ show logps' ++ "\nover      "
            -- ++ show logps
             ++ "\nwith α" ++ show α_samp ++ ": "
             ++ show acceptance_ratio ++ " > " ++ show u
            return ((x', samples', logps'):trace)
    else do
            -- liftS $ putStrLn $ "Rejecting " ++ show (Map.lookup α_samp samples') -- ++ show logps' ++ "\nover      "
            -- ++ show logps
            --  ++ "\nwith α" ++ show α_samp ++ ": "
            --  ++ show acceptance_ratio ++ " < u: " ++ show u
            return trace

-- | Run model once under MH
runMH :: (ts ~ '[AffReader env, Dist, State SMap, State LPMap, Observe, Sample])
  => LRec env       -- Model observed variable
  -> SMap              -- Previous mh sample set
  -> Addr           -- Sample address
  -> Model env ts a -- Model
  -> Sampler (a, SMap, LPMap)
runMH env samples α_samp m = do
  ((a, samples'), logps') <-
                            ( -- This is where the previous run's samples are actually reused.
                              -- We do not reuse logps, we simply recompute them.
                              runSample α_samp samples
                            . runObserve
                            . runState Map.empty
                            . runState Map.empty
                            . transformMH
                            . runDist
                            . runAffReader env
                            . runModel) m
  return (a, samples', logps')

-- transformMH :: (Member Sample ts, Member Observe ts) =>
--   Freer ts a -> Freer (State SMap ': State LPMap ': ts) a
-- transformMH = installN @[State SMap, State LPMap] return
--   (\x tx k ->
--       case tx of
--         Sample d α  -> undefined
--         Observe d y -> undefined)

-- | Insert stateful operations for SMap and LPMap when either Sample or Observe occur.
transformMH :: (Member (State SMap) ts, Member (State LPMap) ts, Member Sample ts, Member Observe ts) => Freer ts a -> Freer ts a
transformMH (Pure x) = return x
transformMH (Free u k) = do
  case u of
    Samp d α
      -> case d of
            -- We can unsafe coerce x here, because we've inferred the type of x from the distribution's type
            d@CategoricalDist{} -> Free u (\x -> modify
                                          (Map.insert α (PrimDist d, OpenSum.inj x :: OpenSum PrimVal)) >>
                                          modify (updateLPMap α d x) >>
                                          transformMH (k x))
            d@DeterministicDist{} -> Free u (\x -> modify
                                          (Map.insert α (PrimDist d, OpenSum.inj x :: OpenSum PrimVal)) >>
                                          modify (updateLPMap α d x) >>
                                          transformMH (k x))
            DistDoubles (Just d) -> Free u (\x -> do
                                          modify (updateSMap α d (unsafeCoerce x))
                                          modify (updateLPMap α d (unsafeCoerce x))
                                          -- prinT $ "Prob of observing " ++ show (unsafeCoerce x :: [Double]) ++ " from " ++ show (toDistInfo d) ++ " is " ++ show (prob d (unsafeCoerce x))
                                          transformMH (k x))
            DistDouble (Just d) -> Free u (\x -> modify (updateSMap α d (unsafeCoerce x)) >>
                                          modify (updateLPMap α d (unsafeCoerce x)) >>
                                          transformMH (k x))
            DistBool (Just d)   -> Free u (\x -> do
                                          modify (updateSMap α d (unsafeCoerce x))
                                          modify (updateLPMap α d (unsafeCoerce x))
                                          transformMH (k x))
            DistInt (Just d)    -> Free u (\x -> modify (updateSMap α d (unsafeCoerce x)) >>
                                          modify (updateLPMap α d (unsafeCoerce x)) >>
                                          transformMH (k x))
            _ -> undefined
    Obs d y α
      -> Free u (\x -> modify (updateLPMap α d y  :: LPMap -> LPMap) >>
                        transformMH (k x))
    _ -> Free u (transformMH . k)

-- | Remove Observe occurrences from tree (log p is taken care of by transformMH)
runObserve :: Member Sample ts => Freer (Observe : ts) a -> Freer ts a
runObserve = loop 0
  where
  loop :: Member Sample ts => Double -> Freer (Observe : ts) a -> Freer ts a
  loop p (Pure x) = return x
  loop p (Free u k) = do
    case decomp u of
      Right (Observe d y α)
        -> case d of
            d@CategoricalDist {} ->
              do let p' = prob d y
                --  prinT $ "Prob of observing " ++ show (unsafeCoerce y :: String) ++ " from " ++ show d ++ " is " ++ show p'
                 loop (p + p') (k y)
            d@DeterministicDist {} ->
              do let p' = prob d y
                --  prinT $ "Prob of observing " ++ show (unsafeCoerce y :: String) ++ " from " ++ show d ++ " is " ++ show p'
                 loop (p + p') (k y)
            DistBool (Just d) ->
              do let p' = prob d (unsafeCoerce y :: Bool)
                --  prinT $ "Prob of observing " ++ show (unsafeCoerce y :: Bool) ++ " from " ++ show d ++ " is " ++ show p'
                 loop (p + p') (k y)
            DistDoubles (Just d) ->
              do  let p' = prob d (unsafeCoerce y :: [Double])
                  -- prinT $ "Prob of observing " ++ show (unsafeCoerce y :: Double) ++ " from " ++ show d ++ " is " ++ show p'
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
runSample :: Addr -> SMap -> Freer '[Sample] a -> Sampler a
runSample α_samp samples = loop
  where
  loop :: Freer '[Sample] a -> Sampler a
  loop (Pure x) = return x
  loop (Free u k) =
    case prj u of
      Just (Printer s) ->
       liftS (putStrLn s) >> loop (k ())
      Just (Sample d α) ->
        case d of
          d@CategoricalDist {} -> do
            x <- fromMaybe <$> sample d
                <*> lookupSample samples d α α_samp
            (loop . k . unsafeCoerce) x
          d@DeterministicDist {} -> do
            x <- fromMaybe <$> sample d <*> lookupSample samples d α α_samp
            (loop . k . unsafeCoerce) x
          DistDoubles (Just d) -> do
            x <- fromMaybe <$> sample d <*> lookupSample samples d α α_samp
            (loop . k . unsafeCoerce) x
          DistDouble (Just d) -> do
            x <- fromMaybe <$> sample d <*> lookupSample samples d α α_samp
            (loop . k . unsafeCoerce) x
          DistBool (Just d) -> do
            x <- fromMaybe <$> sample d <*> lookupSample samples d α α_samp
            (loop . k . unsafeCoerce) x
          DistInt (Just d) -> do
            x <- fromMaybe <$> sample d <*> lookupSample samples d α α_samp
            (loop . k . unsafeCoerce) x
          _ -> undefined
      _  -> error "Impossible: Nothing cannot occur"

lookupSample :: OpenSum.Member a PrimVal => SMap -> Dist a -> Addr -> Addr -> Sampler (Maybe a)
lookupSample samples d α α_samp
  | α == α_samp = return Nothing
  | otherwise   = do
    let m = Map.lookup α samples
    case m of
      Just (PrimDist d', x) -> do
        --liftS $ print $ "Address : " ++ show α ++ " Current dist : " ++ show (toDistInfo d) ++ " Looked up dist : " ++ show d_info ++ " Are they equal? " ++ show (toDistInfo d == d_info)
        return $ if d == unsafeCoerce d' then OpenSum.prj x else Nothing
      Nothing -> return Nothing



-- | Lookup a sample address α's value in Ⲭ.
-- Return Nothing if: 1) it doesn't exist, 2) the sample address is the same as the current sample site α_samp, or 3) the sample we're supposed to reuse belongs to either a different distribution or the same distribution with different parameters (due to a new sampled value affecting its parameters). These all indicate that a new value should be sampled.
-- lookupSample :: OpenSum.Member a '[Int, Double, Bool] => Ⲭ -> Dist a -> Addr -> Addr -> Maybe a
-- lookupSample samples d α α_samp
--   | α == α_samp = Nothing
--   | otherwise   = Map.lookup α samples >>= \(d_info, x) ->
--                   if toDistInfo d == d_info then OpenSum.prj x else Nothing
