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
module Extensible.Inference.MH where

import Data.Functor.Identity
import Data.Map as Map
import Data.Kind (Type)
import Data.Extensible hiding (Member)
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
             -> IO ((a, LogP), Ⲭ, LogP)
runMHnsteps env model = do
  -- perform initial run of mh
  ((x, p), samples, logps) <- runMH env Map.empty 0 model
  -- do some acceptance ratio to see if we use samples or samples'
  return ((x, p), samples, logps)

runMH :: MRec env -> Ⲭ -> Addr 
      -> Freer '[Reader (Record (Maybes env)), Dist, Observe, Sample] a
      -> IO ((a, LogP), Ⲭ, LogP)
runMH env samples n = runSample n Map.empty Map.empty . runObserve Map.empty . runDist . runReader env

runObserve :: LogP -> Freer (Observe : rs) a -> Freer rs (a, LogP)
runObserve logps = loop logps
  where
  loop :: LogP -> Freer (Observe : rs) a -> Freer rs (a, LogP)
  loop logps' (Pure x) = return (x, logps')
  loop logps' (Free u k) = case decomp u of 
    Right (Observe d y α)  
      -> let p = logProb d y
         in  loop (Map.insert α p logps') (k y) 
    Left  u'  -> Free u' (loop logps' . k)

runSample :: Addr -> Ⲭ -> LogP -> Freer '[Sample] a -> IO (a, Ⲭ, LogP)
runSample α_samp samples logps = sampleIO . loop samples logps
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
                            loop (Map.insert α (OpenSum.inj x) samples') 
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
