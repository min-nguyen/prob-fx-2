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

runMHnsteps ::  MRec env 
             -> Freer '[Reader (Record (Maybes env)), Dist, Observe, Sample] a -- ^ 
             -> Sampler (a, Ⲭ, LogP)
runMHnsteps env model = do
  -- perform initial run of mh
  (x, samples, logps) <- runMH env Map.empty Map.empty 0 model
  liftS $ print $ "First run is: " ++ show (samples, logps)
  -- uniformly select a random sample address to update for
  let sample_size = Map.size samples
  α_samp <- sample $ DiscreteDist (map (,1.0/fromIntegral sample_size) (Map.keys samples)) Nothing
  -- run mh with new sample address
  liftS $ print $ "sample address is " ++ show α_samp
  (x', samples', logps') <- runMH env samples logps α_samp model
  liftS $ print $ "Second run is: " ++ show (samples', logps')
  -- do some acceptance ratio to see if we use samples or samples'
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
      -> let p = logProb d y
         in  loop (Map.insert α p logps') (k y) 
    Left  u'  -> Free u' (loop logps' . k)

runSample :: Addr -> Ⲭ -> Freer '[Sample] a -> Sampler (a, Ⲭ, LogP)
runSample α_samp samples = loop samples Map.empty
  where
  loop :: Ⲭ -> LogP -> Freer '[Sample] a -> Sampler (a, Ⲭ, LogP)
  loop samples' logps' (Pure x) = return (x, samples', logps')
  loop samples' logps' (Free u k) = do
    case prj u of
      Just (Sample d α) -> 
        case d of
          NormalDist {} -> 
            case lookupSample samples' d α α_samp of
              Nothing -> do x <- sample d
                            loop (Map.insert α (OpenSum.inj (x :: Double)) samples') 
                                 (Map.insert α (logProb d x) logps') (k x) 
              Just x ->  loop samples' logps' (k x)
          UniformDist {} -> 
            case lookupSample samples' d α α_samp of
              Nothing -> do x <- sample d
                            loop (Map.insert α (OpenSum.inj x) samples') 
                                 (Map.insert α (logProb d x) logps') (k x) 
              Just x ->  loop samples' logps' (k x)
          BernoulliDist {} -> 
            case lookupSample samples' d α α_samp of
              Nothing -> do x <- sample d
                            loop (Map.insert α (OpenSum.inj x) samples') 
                                 (Map.insert α (logProb d x) logps') (k x) 
              Just x ->  loop samples' logps' (k x)
          BetaDist {} -> 
            case lookupSample samples' d α α_samp of
              Nothing -> do x <- sample d
                            loop (Map.insert α (OpenSum.inj x) samples') 
                                 (Map.insert α (logProb d x) logps') (k x) 
              Just x ->  loop samples' logps' (k x)
          BinomialDist {} -> 
            case lookupSample samples' d α α_samp of
              Nothing -> do x <- sample d
                            loop (Map.insert α (OpenSum.inj x) samples') 
                                 (Map.insert α (logProb d x) logps') (k x) 
              Just x ->  loop samples' logps' (k x)
          GammaDist {} -> 
            case lookupSample samples' d α α_samp of
              Nothing -> do x <- sample d
                            loop (Map.insert α (OpenSum.inj x) samples') 
                                 (Map.insert α (logProb d x) logps') (k x) 
              Just x ->  loop samples' logps' (k x)
      Nothing         -> error "Impossible: Nothing cannot occur"

-- | Lookup a sample address α's value - return Nothing if it doesn't exist or the sample address is the same as the current sample site α_samp, indicating that a new value should be sampled. 
lookupSample :: OpenSum.Member a '[Int, Double, Bool] => Ⲭ -> Dist a -> Addr -> Addr -> Maybe a
lookupSample samples  d α α_samp 
  | α == α_samp = Nothing
  | otherwise   = Map.lookup α samples >>= OpenSum.prj
