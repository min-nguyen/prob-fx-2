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
import Extensible.Model hiding (runModel, runModelFree)
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

accept :: Addr -> Ⲭ -> Ⲭ -> LogP -> LogP -> IO Double
accept x0 _Ⲭ _Ⲭ' logℙ logℙ' = do

  let _X'sampled = Set.singleton x0 `Set.union` (Map.keysSet _Ⲭ' \\ Map.keysSet _Ⲭ)
      _Xsampled  = Set.singleton x0 `Set.union` (Map.keysSet _Ⲭ \\ Map.keysSet _Ⲭ')
  -- print $ " _X'sampled is " ++ show  _X'sampled
  -- print $ " _Xsampled is " ++ show _Xsampled

  let logα       = log (fromIntegral $ Map.size _Ⲭ) - log (fromIntegral $ Map.size _Ⲭ')
  -- print $ " logα is " ++ show logα

  let logα'      = foldl (\logα v -> logα + fromJust (Map.lookup v logℙ'))
                         logα (Map.keysSet logℙ' \\ _X'sampled)
  -- print $ " logα' is " ++ show (exp logα') ++ " from " ++ show (Map.keysSet logℙ' \\ _X'sampled)

  let logα''     = foldl (\logα' v -> logα' - fromJust (Map.lookup v logℙ))
                         logα' (Map.keysSet logℙ \\ _Xsampled)
  -- print $ " logα'' is " ++ show (exp logα'') ++ " from " ++ show (Map.keysSet logℙ \\ _Xsampled)

  return $ exp logα''

runMHnsteps ::  
                Int
             -> MRec env
             -> Freer '[Reader (Record (Maybes env)), Dist, Observe, State Ⲭ, Sample] a -- ^ 
             -> Sampler (a, Ⲭ, LogP)
runMHnsteps n env model = do
  -- perform initial run of mh
  (x, samples, logps) <- runMH env Map.empty 0 model
  -- liftS $ print $ "First run is: " ++ show (x, samples, logps)
  -- uniformly select a random sample address to update for
  let loop i (x, samples, logps) = do
        let sample_size = Map.size samples
        α_samp <- sample $ DiscreteDist (map (,1.0/fromIntegral sample_size) (Map.keys samples)) Nothing
        -- run mh with new sample address
        liftS $ print $ "sample address is " ++ show α_samp
        (x', samples', logps') <- runMH env samples α_samp model
        -- liftS $ print $ "Second run is: " ++ show (x', samples', logps')
        -- do some acceptance ratio to see if we use samples or samples'
        acceptance_ratio <- liftS $ accept α_samp samples samples' logps logps'
        u <- sample (UniformDist 0 1 Nothing)
        mhState <- if u < acceptance_ratio
                    then do liftS $ print $ "Accepting with " ++ show acceptance_ratio ++ " > " ++ show u
                            return (x', samples', logps')
                    else do liftS $ print $ "Rejecting with α: " ++ show acceptance_ratio ++ " < u: " ++ show u
                            return (x, samples, logps)
        if i < n
          then loop (i + 1) mhState
          else return mhState
  loop 0 (x, samples, logps)

runMH :: MRec env -> Ⲭ -> Addr
      -> Freer '[Reader (Record (Maybes env)), Dist, Observe, State Ⲭ, Sample] a
      -> Sampler (a, Ⲭ, LogP)
runMH env samples  n m = do
  (((a, logps_obs'), samples'), logps_samp')
    <- (runSample n samples . runState samples . runObserve . transformMH . runDist . runReader env) m
  -- Merge log probability maps
  let logps' = logps_obs' `Map.union` logps_samp'
  -- liftS $ print $ "samples are" ++ show samples
  return (a, samples', logps')

transformMH :: (Member (State Ⲭ) rs, Member Sample rs, Member Observe rs) => Freer rs a -> Freer rs a
transformMH = loop
  where
  loop :: (Member (State Ⲭ) rs, Member Sample rs, Member Observe rs) => Freer rs a -> Freer rs a
  loop (Pure x) = return x
  loop (Free u k) = do
    case prj u of
      Just (Sample d α) 
        -> let 
               updateMapⲬ :: OpenSum.Member x Vals => x -> Ⲭ -> Ⲭ
               updateMapⲬ x = Map.insert α (toDistInfo d, OpenSum.inj x) :: Ⲭ -> Ⲭ
               
               updateLogP :: Dist x -> x -> LogP -> LogP
               updateLogP d x = Map.insert α (logProb d x)
           in case d of
              NormalDist {}    -> Free u (\x -> modify (updateMapⲬ x :: Ⲭ -> Ⲭ) >> loop (k x))
              UniformDist {}   -> Free u (\x -> modify (updateMapⲬ x :: Ⲭ -> Ⲭ) >> loop (k x)) 
              GammaDist {}     -> Free u (\x -> modify (updateMapⲬ x :: Ⲭ -> Ⲭ) >> loop (k x))
              BernoulliDist {} -> Free u (\x -> modify (updateMapⲬ x :: Ⲭ -> Ⲭ) >> loop (k x))
      Nothing
        -> Free u (loop . k)
        -- case prj u of 
        -- Just (Observe d y α) -> undefined

-- let insertMap :: Addr -> Dist a -> OpenSum Vals -> Ⲭ -> Ⲭ
--     insertMap α d x samples = Map.insert α (toDistInfo d, x) samples
-- Free u (\x -> modify 
--  (insertMap α d (OpenSum.inj (unsafeCoerce x :: distType))) 
--  >> loop (k x))

runObserve :: Freer (Observe : rs) a -> Freer rs (a, LogP)
runObserve  = loop Map.empty
  where
  loop :: LogP -> Freer (Observe : rs) a -> Freer rs (a, LogP)
  loop logps' (Pure x) = return (x, logps')
  loop logps' (Free u k) = case decomp u of
    Right (Observe d y α)
      -> let p = prob d y
         in  loop (Map.insert α p logps') (k y) 
    Left  u'  -> Free u' (loop logps' . k)

runSample :: Addr -> Ⲭ -> Freer '[Sample] a -> Sampler (a, LogP)
runSample α_samp samples = loop Map.empty
  where
  loop :: LogP -> Freer '[Sample] a -> Sampler (a, LogP)
  loop logps' (Pure x) = return (x, logps')
  loop logps' (Free u k) = do
    case prj u of
      Just (Sample d α) ->
        case d of
          NormalDist {} ->
            case lookupSample samples d α α_samp of
              Nothing -> do x <- sample d
                            -- liftS $ print $ "prob of address " ++ show α ++ " with value " ++ show x ++ " is " ++ show (prob d x) ++ " dist: " ++ show d
                            loop (Map.insert α (logProb d x) logps') (k x)
              Just x ->     loop (Map.insert α (logProb d x) logps') (k x)
          UniformDist {} ->
            case lookupSample samples d α α_samp of
              Nothing -> do x <- sample d
                            -- liftS $ print $ "prob of address " ++ show α ++ " with value " ++ show x ++ " is " ++ show (prob d x) ++ " dist: " ++ show d
                            loop (Map.insert α (logProb d x) logps') (k x)
              Just x ->     loop (Map.insert α (logProb d x) logps') (k x)
          BernoulliDist {} ->
            case lookupSample samples d α α_samp of
              Nothing -> do x <- sample d
                            -- liftS $ print $ "prob of address " ++ show α ++ " with value " ++ show x ++ " is " ++ show (prob d x) ++ " dist: " ++ show d
                            loop (Map.insert α (logProb d x) logps') (k x)
              Just x ->     loop (Map.insert α (logProb d x) logps') (k x)
          BetaDist {} ->
            case lookupSample samples d α α_samp of
              Nothing -> do x <- sample d
                            -- liftS $ print $ "prob of address " ++ show α ++ " with value " ++ show x ++ " is " ++ show (prob d x) ++ " dist: " ++ show d
                            loop (Map.insert α (logProb d x) logps') (k x)
              Just x ->     loop (Map.insert α (logProb d x) logps') (k x)
          BinomialDist {} ->
            case lookupSample samples d α α_samp of
              Nothing -> do x <- sample d
                            -- liftS $ print $ "prob of address " ++ show α ++ " with value " ++ show x ++ " is " ++ show (prob d x) ++ " dist: " ++ show d
                            loop (Map.insert α (logProb d x) logps') (k x)
              Just x ->     loop (Map.insert α (logProb d x) logps') (k x)
          GammaDist {} ->
            case lookupSample samples d α α_samp of
              Nothing -> do x <- sample d
                            -- liftS $ print $ "prob of address " ++ show α ++ " with value " ++ show x ++ " is " ++ show (prob d x) ++ " dist: " ++ show d
                            loop (Map.insert α (logProb d x) logps') (k x)
              Just x ->     loop (Map.insert α (logProb d x) logps') (k x)
      Nothing         -> error "Impossible: Nothing cannot occur"

-- | Lookup a sample address α's value - 
-- return Nothing if: 1) it doesn't exist, 2) the sample address is the same as the current sample site α_samp, or 3) the sample we're supposed to reuse belongs to either a different distribution or the same distribution with different parameters (due to a new sampled value affecting its parameters). These all indicate that a new value should be sampled.
lookupSample :: OpenSum.Member a '[Int, Double, Bool] => Ⲭ -> Dist a -> Addr -> Addr -> Maybe a
lookupSample samples d α α_samp
  | α == α_samp = Nothing
  | otherwise   = Map.lookup α samples >>= \(d_info, x) ->
                  if toDistInfo d == d_info then OpenSum.prj x else Nothing
