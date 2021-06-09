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
import Extensible.OpenSum as OpenSum (OpenSum(..), Member)
import Extensible.Reader
import qualified Extensible.OpenSum as OpenSum
import GHC.Natural
import GHC.TypeLits (Nat)
import qualified GHC.TypeLits as TL
import Unsafe.Coerce

type Vals = '[Int, Double, Bool]

type LogP = Map Addr Double
type Ⲭ    = Map Addr (OpenSum Vals)

accept :: Addr -> Ⲭ -> Ⲭ -> LogP -> LogP -> IO Double
accept x0 _Ⲭ _Ⲭ' logℙ logℙ' = do
{- The set of sampled variables is given by X'sampled = {x0} ∪ (dom(Ⲭ')\dom(Ⲭ)), i.e.
   the proposal variable x0 that we resample, and also any latent variables in Ⲭ' 
   that aren't in the domain of Ⲭ. Conversely, Xsampled = {x0} ∪ (dom(Ⲭ)\dom(Ⲭ')).  -}
  let _X'sampled = (Set.singleton x0) `Set.union` (Map.keysSet _Ⲭ' \\ Map.keysSet _Ⲭ)  
      _Xsampled  = (Set.singleton x0) `Set.union` (Map.keysSet _Ⲭ \\ Map.keysSet _Ⲭ')
  print $ " _X'sampled is " ++ show  _X'sampled
  print $ " _Xsampled is " ++ show _Xsampled
{- The ratio q(x0 | X'0) / q(x0 | X) accounts for the relative probability of selecting 
   the initial site. Since x0 is chosen at random, this is the size of set X divided 
   by the size of set X'.                       q(x0 | X') / q(x0 | X) = |X| / |X'| -}
  let logα       = log (fromIntegral $ Map.size _Ⲭ) - log (fromIntegral $ Map.size _Ⲭ')
  print $ " logα is " ++ show logα
{- The ratio p(Y', X') / q(X'|X, x0) simplifies to:   Π ℙ'(y) Π ℙ'(x)
                                                     y∈Y'    x∈X'reused  
   This is equivalent to  Π ℙ'(v)
                         v∈V' \ X'sampled -}
  let logα'      = foldl (\logα v -> logα + fromJust (Map.lookup v logℙ')) 
                         logα (Map.keysSet logℙ' \\ _X'sampled)
  print $ " logα' is " ++ show logα' ++ " from " ++ show (Map.keysSet logℙ' \\ _X'sampled)
{- The ratio p(Y, X) / q(X|X, x0) simplifies to:   Π ℙ(y) Π ℙ(x)
                                                  y∈Y    x∈Xreused  
   This is equivalent to  Π ℙ(v)
                         v∈V \ Xsampled.
   To flip the ratio p(Y, X) / q(X|X, x0) to q(X|X, x0) / p(Y, X), we subtract the log form -}
  let logα''     = foldl (\logα' v -> logα' - fromJust (Map.lookup v logℙ))
                         logα' (Map.keysSet logℙ \\ _Xsampled)
  print $ " logα'' is " ++ show logα'' ++ " from " ++ show (Map.keysSet logℙ \\ _Xsampled)
  return $ exp logα''

runMHnsteps ::  Show a => MRec env 
             -> Freer '[Reader (Record (Maybes env)), Dist, Observe, Sample] a -- ^ 
             -> Sampler (a, Ⲭ, LogP)
runMHnsteps env model = do
  -- perform initial run of mh
  (x, samples, logps) <- runMH env Map.empty Map.empty 0 model
  liftS $ print $ "First run is: " ++ show (x, samples, logps)
  -- uniformly select a random sample address to update for
  let sample_size = Map.size samples
  α_samp <- sample $ DiscreteDist (map (,1.0/fromIntegral sample_size) (Map.keys samples)) Nothing
  -- run mh with new sample address
  liftS $ print $ "sample address is " ++ show α_samp
  (x', samples', logps') <- runMH env samples logps α_samp model
  liftS $ print $ "Second run is: " ++ show (x', samples', logps')
  -- do some acceptance ratio to see if we use samples or samples'
  acceptance_ratio <- liftS $ accept α_samp samples samples' logps logps'
  liftS $ print $ "Acceptance ratio is: " ++ show acceptance_ratio
  return (x, samples, logps)

runMH :: MRec env -> Ⲭ -> LogP -> Addr 
      -> Freer '[Reader (Record (Maybes env)), Dist, Observe, Sample] a
      -> Sampler (a,  Ⲭ, LogP)
runMH env samples logps n m = do 
  ((a, logps_obs'), samples', logps_samp') 
    <- (runSample n samples . runObserve . runDist . runReader env) m
  -- Merge log probability maps, with new map entries taking precedence over the map from the previous MH run
  let logps' = logps_obs' `Map.union` logps_samp' `Map.union` logps
  -- liftS $ print $ "samples are" ++ show samples
  return (a, samples', logps')

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

runSample :: Addr -> Ⲭ -> Freer '[Sample] a -> Sampler (a, Ⲭ, LogP)
runSample α_samp samples = loop Map.empty Map.empty
  where
  loop :: Ⲭ -> LogP -> Freer '[Sample] a -> Sampler (a, Ⲭ, LogP)
  loop samples' logps' (Pure x) = return (x, samples', logps')
  loop samples' logps' (Free u k) = do
    case prj u of
      Just (Sample d α) -> 
        case d of
          NormalDist {} -> 
            case lookupSample samples d α α_samp of
              Nothing -> do x <- sample d
                            liftS $ print $ "prob of address " ++ show α ++ " with value " ++ show x ++ " is " ++ show (prob d x) ++ " dist: " ++ show d
                            loop (Map.insert α (OpenSum.inj (x :: Double)) samples') 
                                 (Map.insert α (prob d x) logps') (k x) 
              Just x ->     loop (Map.insert α (OpenSum.inj (x :: Double)) samples') 
                                 (Map.insert α (prob d x) logps') (k x)
          UniformDist {} -> 
            case lookupSample samples d α α_samp of
              Nothing -> do x <- sample d
                            liftS $ print $ "prob of address " ++ show α ++ " with value " ++ show x ++ " is " ++ show (prob d x) ++ " dist: " ++ show d
                            loop (Map.insert α (OpenSum.inj (x :: Double)) samples') 
                                 (Map.insert α (prob d x) logps') (k x) 
              Just x ->     loop (Map.insert α (OpenSum.inj (x :: Double)) samples') 
                                 (Map.insert α (prob d x) logps') (k x)
          BernoulliDist {} -> 
            case lookupSample samples d α α_samp of
              Nothing -> do x <- sample d
                            liftS $ print $ "prob of address " ++ show α ++ " with value " ++ show x ++ " is " ++ show (prob d x) ++ " dist: " ++ show d
                            loop (Map.insert α (OpenSum.inj (x :: Bool)) samples') 
                                 (Map.insert α (prob d x) logps') (k x) 
              Just x ->     loop (Map.insert α (OpenSum.inj (x :: Bool)) samples') 
                                 (Map.insert α (prob d x) logps') (k x)
          BetaDist {} -> 
            case lookupSample samples d α α_samp of
              Nothing -> do x <- sample d
                            liftS $ print $ "prob of address " ++ show α ++ " with value " ++ show x ++ " is " ++ show (prob d x) ++ " dist: " ++ show d
                            loop (Map.insert α (OpenSum.inj (x :: Double)) samples') 
                                 (Map.insert α (prob d x) logps') (k x) 
              Just x ->     loop (Map.insert α (OpenSum.inj (x :: Double)) samples') 
                                 (Map.insert α (prob d x) logps') (k x)
          BinomialDist {} -> 
            case lookupSample samples d α α_samp of
              Nothing -> do x <- sample d
                            liftS $ print $ "prob of address " ++ show α ++ " with value " ++ show x ++ " is " ++ show (prob d x) ++ " dist: " ++ show d
                            loop (Map.insert α (OpenSum.inj (x :: Int)) samples') 
                                 (Map.insert α (prob d x) logps') (k x) 
              Just x ->     loop (Map.insert α (OpenSum.inj (x :: Int)) samples') 
                                 (Map.insert α (prob d x) logps') (k x)
          GammaDist {} -> 
            case lookupSample samples d α α_samp of
              Nothing -> do x <- sample d
                            liftS $ print $ "prob of address " ++ show α ++ " with value " ++ show x ++ " is " ++ show (prob d x) ++ " dist: " ++ show d
                            loop (Map.insert α (OpenSum.inj (x :: Double)) samples') 
                                 (Map.insert α (prob d x) logps') (k x) 
              Just x ->     loop (Map.insert α (OpenSum.inj (x :: Double)) samples') 
                                 (Map.insert α (prob d x) logps') (k x)
      Nothing         -> error "Impossible: Nothing cannot occur"

-- | Lookup a sample address α's value - return Nothing if it doesn't exist or the sample address is the same as the current sample site α_samp, indicating that a new value should be sampled. 
lookupSample :: OpenSum.Member a '[Int, Double, Bool] => Ⲭ -> Dist a -> Addr -> Addr -> Maybe a
lookupSample samples d α α_samp 
  | α == α_samp = Nothing
  | otherwise   = Map.lookup α samples >>= OpenSum.prj
