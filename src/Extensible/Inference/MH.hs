{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
 
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module Extensible.Inference.MH where

import Data.Functor.Identity
import Data.Map as Map
import Data.Kind (Type)
import Data.Extensible
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Extensible.Dist
import Extensible.Freer
import Extensible.Model hiding (runModel, runModelFree)
import Extensible.Sampler
import Extensible.OpenSum (OpenSum)
import qualified Extensible.OpenSum as OpenSum
import GHC.Natural
import GHC.TypeLits (Nat)
import qualified GHC.TypeLits as TL
import Unsafe.Coerce

data Val  where
  VInt    :: Int -> Val 
  VDouble :: Double -> Val 
  VBool   :: Bool -> Val  

asInt :: Val -> Maybe Int 
asInt (VInt i) = Just i
asInt _ = Nothing

asDouble :: Val -> Maybe Double
asDouble (VDouble d) = Just d
asDouble _ = Nothing 

asBool :: Val -> Maybe Bool
asBool (VBool b) = Just b
asBool _ = Nothing

type Vals = '[Int, Double, Bool]

type Ⲭ = Map Addr (OpenSum Vals)

-- runLW :: Freer '[Observe, Sample] a -> IO (a, Double)
-- runLW = runSample . runObserve

-- runObserve :: Freer (Observe : rs) a -> Freer rs (a, Double)
-- runObserve = loop 0
--   where
--   loop :: Double -> Freer (Observe : rs) a -> Freer rs (a, Double)
--   loop p (Pure x) = return (x, p)
--   loop p (Free u k) = case decomp u of 
--     Right (Observe d y α)  
--       -> let p' = logProb d y
--          in  loop (p + p') (k y) 
--     Left  u'  -> Free u' (loop p . k)

runSample :: Addr -> Ⲭ -> Freer '[Sample] a -> IO (a, Ⲭ)
runSample α_samp samples = sampleIO . loop samples
  where
  loop :: Ⲭ -> Freer '[Sample] a -> Sampler (a, Ⲭ)
  loop samples' (Pure x) = return (x, samples')
  loop samples' (Free u k) = do
    case prj u of
      Just (Sample d α) -> 
        case d of
          NormalDist {} -> 
            case lookupSample samples' d α α_samp of
              Nothing -> do x <- sample d
                            loop (Map.insert α (OpenSum.inj x) samples') (k x) 
              Just x ->  loop samples' (k x)
          BernoulliDist {} -> 
            case lookupSample samples' d α α_samp of
              Nothing -> do x <- sample d  
                            loop (Map.insert α (OpenSum.inj x) samples') (k x) 
              Just x ->  loop samples' (k x)
          BinomialDist {} -> 
            case lookupSample samples' d α α_samp of
              Nothing -> do x <- sample d  
                            loop (Map.insert α (OpenSum.inj x) samples') (k x) 
              Just x ->  loop samples' (k x)
      Nothing         -> error "Impossible: Nothing cannot occur"

-- | Lookup a sample address α's value - return Nothing if it doesn't exist or the sample address is the same as the current sample site α_samp, indicating that a new value should be sampled. 
lookupSample :: OpenSum.Member a '[Int, Double, Bool] => Ⲭ -> Dist a -> Addr -> Addr -> Maybe a
lookupSample samples  d α α_samp 
  | α == α_samp = Nothing
  | otherwise   = Map.lookup α samples >>= OpenSum.prj
