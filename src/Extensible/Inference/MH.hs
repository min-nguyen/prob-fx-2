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
import Data.Extensible hiding (Member)
import Control.Monad
import Control.Monad.Trans.Class
import Extensible.Dist
import Extensible.Freer
import Extensible.Model hiding (runModelFree)
import Extensible.Sampler
import qualified Extensible.OpenSum as OpenSum
import Extensible.OpenSum (OpenSum(..))
import Extensible.Reader
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

updateMapⲬ :: OpenSum.Member x Vals => Addr -> Dist x -> x -> Ⲭ -> Ⲭ
updateMapⲬ α d x = Map.insert α (toDistInfo d, OpenSum.inj x) :: Ⲭ -> Ⲭ

updateLogP :: Addr -> Dist x -> x -> LogP -> LogP
updateLogP α d x  = Map.insert α (logProb d x)

accept :: Addr -> Ⲭ -> Ⲭ -> LogP -> LogP -> IO Double
accept x0 _Ⲭ _Ⲭ' logℙ logℙ' = do
  let _X'sampled = Set.singleton x0 `Set.union` (Map.keysSet _Ⲭ' \\ Map.keysSet _Ⲭ)
      _Xsampled  = Set.singleton x0 `Set.union` (Map.keysSet _Ⲭ \\ Map.keysSet _Ⲭ')
  print $ " _X'sampled is " ++ show  _X'sampled
  print $ " _Xsampled is " ++ show _Xsampled
  let logα       = log (fromIntegral $ Map.size _Ⲭ) - log (fromIntegral $ Map.size _Ⲭ')
  -- print $ " logα is " ++ show logα
  let logα'      = foldl (\logα v -> logα + fromJust (Map.lookup v logℙ'))
                         logα (Map.keysSet logℙ' \\ _X'sampled)
  -- print $ " logα' is " ++ show (exp logα') ++ " from " ++ show (Map.keysSet logℙ' \\ _X'sampled)
  let logα''     = foldl (\logα' v -> logα' - fromJust (Map.lookup v logℙ))
                         logα' (Map.keysSet logℙ \\ _Xsampled)
  -- print $ " logα'' is " ++ show (exp logα'') ++ " from " ++ show (Map.keysSet logℙ \\ _Xsampled)
  return $ exp logα''

-- | Need to implement mhNsteps for m number of envs and model inputs
mh :: (es ~ '[Reader (Record (Maybes s)), Dist, Observe, State Ⲭ, State LogP, Sample])
   => (b -> Model s es a)              -- Model
   -> [b]                              -- Model inputs
   -> [MRec s]
   -> Sampler (a, Ⲭ, LogP)
mh model xs ys = do
  (x, samples, logps) <- runMH (head ys) Map.empty 0 (model $ head xs)
  let models = map model xs
      mhs    = mhNsteps 3 -- isn't used. need to fix this.
      nMh    = repeat mhStep
      -- currently this is just doing a single mhStep per data point
      nMh'   = zipWith (\nMh (y, model) -> nMh y model) nMh (zip ys models)
  liftS $ putStrLn $ "length is " ++ show (length nMh')
-- models = map model xs :: [Model s es a]
-- nMh = replicate n mhStep :: [MRec s -> Model s es a -> (a, Ⲭ, LogP) -> Sampler (a, Ⲭ, LogP)]
-- nMh' = zipWith ($) nMh ys :: [Model s es a -> (a, Ⲭ, LogP) -> Sampler (a, Ⲭ, LogP)]
-- nMh'' = zipWith ($) nMh' models :: [(a, Ⲭ, LogP) -> Sampler (a, Ⲭ, LogP)]
  foldr (>=>) return nMh' (x, samples, logps)


-- | Perform n steps of MH for a single observable variable environment
mhNsteps :: Int
  -> MRec env
  -> Model env '[Reader (Record (Maybes env)), Dist, Observe, State Ⲭ, State LogP, Sample] a
  -> Sampler (a, Ⲭ, LogP)
mhNsteps n env model = do
  -- perform initial run of mh
  (x, samples, logps) <- runMH env Map.empty 0 model
  -- liftS $ print $ "First run is: " ++ show (x, samples, logps)
  foldr (>=>) return (replicate n (mhStep env model)) (x, samples, logps)

-- | Perform one step of MH
mhStep :: MRec env
  -> Model env '[Reader (Record (Maybes env)), Dist, Observe, State Ⲭ, State LogP, Sample] a
  -> (a, Ⲭ, LogP)
  -> Sampler (a, Ⲭ, LogP)
mhStep env model (x, samples, logps) = do
  let sample_size = Map.size samples
  α_samp <- sample $ DiscreteDist (map (,1.0/fromIntegral sample_size) (Map.keys samples)) Nothing
  -- run mh with new sample address
  liftS $ print $ "sample address is " ++ show α_samp
  (x', samples', logps') <- runMH env samples α_samp model
  -- liftS $ print $ "Second run is: " ++ show (x', samples', logps')
  -- do some acceptance ratio to see if we use samples or samples'
  acceptance_ratio <- liftS $ accept α_samp samples samples' logps logps'
  u <- sample (UniformDist 0 1 Nothing)
  if u < acceptance_ratio
    then do liftS $ putStrLn $ "Accepting " ++ show logps' ++ "\nover      " ++ show logps ++ "\nwith " ++ show acceptance_ratio ++ " > " ++ show u
            return (x', samples', logps')
    else do liftS $ putStrLn $ "Rejecting " ++ show logps' ++ "\nover      " ++ show logps ++  "\nwith α: " ++ show acceptance_ratio ++ " < u: " ++ show u
            return (x, samples, logps)

-- | Run model once under MH
runMH :: MRec env -> Ⲭ -> Addr
      -> Model env '[Reader (Record (Maybes env)), Dist, Observe, State Ⲭ, State LogP, Sample] a
      -> Sampler (a, Ⲭ, LogP)
runMH env samples n m = do
  ((a, samples'), logps') <- ( -- This is where the previous run's samples are actually reused.
                               -- We do not reuse logps, we simply recompute them.
                              runSample n samples
                            . runState Map.empty
                            . runState Map.empty
                            . runObserve
                            . transformMH
                            . runDist
                            . runReader env
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
              DistDouble d -> Free u (\x -> modify (updateMapⲬ α d (unsafeCoerce x)) >>
                                            modify (updateLogP α d (unsafeCoerce x)) >>
                                            loop (k x))
              DistBool d   -> Free u (\x -> modify (updateMapⲬ α d (unsafeCoerce x)) >>
                                            modify (updateLogP α d (unsafeCoerce x)) >>
                                            loop (k x))
              DistInt d    -> Free u (\x -> modify (updateMapⲬ α d (unsafeCoerce x)) >>
                                            modify (updateLogP α d (unsafeCoerce x)) >>
                                            loop (k x))
      Obs d y α
        -> Free u (\x -> modify (updateLogP α d y  :: LogP -> LogP) >>
                         loop (k x))
      _ -> Free u (loop . k)

-- | Remove Observe occurrences from tree (log p is taken care of by transformMH)
runObserve :: Freer (Observe : rs) a -> Freer rs a
runObserve  = loop
  where
  loop :: Freer (Observe : rs) a -> Freer rs a
  loop (Pure x) = return x
  loop (Free u k) = case decomp u of
    Right (Observe d y α)
      -> loop (k y)
    Left  u'  -> Free u' (loop . k)

-- | Run Sample occurrences
runSample :: Addr -> Ⲭ -> Freer '[Sample] a -> Sampler a
runSample α_samp samples = loop
  where
  loop :: Freer '[Sample] a -> Sampler a
  loop (Pure x) = return x
  loop (Free u k) = do
    case u of
      Samp d α ->
        case d of
          DistDouble d ->
            case lookupSample samples d α α_samp of
              Nothing -> do
                x <- sample d
                liftS (putStrLn $ "Drawing new sample for α" ++ show α ++ " x: " ++ show x)
                (loop . k . unsafeCoerce) x
              Just x  -> do
                liftS (putStrLn $ "Using old sample for α" ++ show α ++ " x: " ++ show x)
                (loop . k . unsafeCoerce) x
          DistBool d ->
            case lookupSample samples d α α_samp of
              Nothing -> sample d >>= loop . k . unsafeCoerce
              Just x  -> (loop . k . unsafeCoerce) x
          DistInt d ->
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
