{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE TypeOperators #-}
module Extensible.Inference.LW where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Extensible hiding (Member)
import Extensible.RecordReader
import Control.Monad
import Control.Monad.Trans.Class
import Unsafe.Coerce
import Extensible.Dist
import qualified Extensible.Example as Example
import Extensible.Freer
import Extensible.Model hiding (runModelFree)
import Extensible.Sampler
import Extensible.State
import qualified Extensible.OpenSum as OpenSum
import Extensible.OpenSum (OpenSum(..))

type Vals = '[Int, Double, Bool]

type Ⲭ = Map Addr (OpenSum Vals)

type TraceLW a = [(a, Ⲭ, Double)]

updateMapⲬ :: OpenSum.Member x Vals => Addr -> x -> Ⲭ -> Ⲭ
updateMapⲬ α x = Map.insert α (OpenSum.inj x) :: Ⲭ -> Ⲭ

-- | Run LW n times for multiple data points
lw :: (es ~ '[RecReader (AsList env), State (LRec env), Dist, State Ⲭ, Observe, Sample])
   => Int                              -- Number of lw iterations per data point
   -> (b -> Model env es a)            -- Model awaiting input variable
   -> [b]                              -- List of model input variables
   -> [LRec env]                       -- List of model observed variables
   -> Sampler (TraceLW a)              -- List of n likelihood weightings for each data point
lw n model xs ys = do
  concat <$> zipWithM (\x y -> lwNsteps n y (model x)) xs ys

-- | Run LW n times for a single data point
lwNsteps :: (es ~ '[RecReader (AsList env), State (LRec env), Dist, State Ⲭ, Observe, Sample])
  => Int
  -> LRec env
  -> Model env es a
  -> Sampler (TraceLW a)
lwNsteps n env model = replicateM n (runLW env model)

-- | Run LW once for single data point
runLW :: es ~ '[RecReader (AsList env), State (LRec env), Dist, State Ⲭ, Observe, Sample]
  => LRec env -> Model env es a
  -> Sampler (a, Ⲭ, Double)
runLW env model = do
  (((x, ys), samples), p) <- (runSample
                            . runObserve
                            . runState Map.empty
                            . transformLW
                            . runDist
                            . runState env
                            . runRecReader env
                            . runModel) model
  return (x, samples, p)

transformLW :: (Member (State Ⲭ) rs, Member Sample rs)
  => Freer rs a -> Freer rs a
transformLW = loop
  where
  loop :: (Member (State Ⲭ) rs, Member Sample rs)
       => Freer rs a -> Freer rs a
  loop (Pure x) = return x
  loop (Free u k) = do
    case u of
      Samp d α
        -> case d of
              -- We can unsafe coerce x here, because we've inferred the type of x from the distribution's type
              DistDouble d -> Free u (\x -> modify (updateMapⲬ α (unsafeCoerce x :: Double)) >>
                                            loop (k x))
              DistBool d   -> Free u (\x -> modify (updateMapⲬ α (unsafeCoerce x :: Bool)) >>
                                            loop (k x))
              DistInt d    -> Free u (\x -> modify (updateMapⲬ α (unsafeCoerce x :: Int)) >>
                                            loop (k x))
      _ -> Free u (loop . k)

runObserve :: Freer (Observe : rs) a -> Freer rs (a, Double)
runObserve = loop 0
  where
  loop :: Double -> Freer (Observe : rs) a -> Freer rs (a, Double)
  loop p (Pure x) = return (x, p)
  loop p (Free u k) = case decomp u of
    Right (Observe d y α)
      -> let p' = prob d y
         in  loop (p + p') (k y)
    Left  u'  -> Free u' (loop p . k)

runSample :: Freer '[Sample] a -> Sampler a
runSample = loop
  where
  loop :: Freer '[Sample] a -> Sampler a
  loop (Pure x) = return x
  loop (Free u k) = case prj u of
    Just (Sample d α) ->
       liftS (putStrLn $ ">> : " ++ show α) >> sample d >>= loop . k
    Nothing         -> error "Impossible: Nothing cannot occur"
