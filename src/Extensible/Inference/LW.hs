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
import Extensible.OpenProduct
import Extensible.AffineReader
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

type Ⲭ = Map Addr (OpenSum PrimVal)

type TraceLW a = [(a, Ⲭ, Double)]

updateMapⲬ :: OpenSum.Member x PrimVal => Addr -> x -> Ⲭ -> Ⲭ
updateMapⲬ α x = Map.insert α (OpenSum.inj x) :: Ⲭ -> Ⲭ



-- | Run LW n times for multiple data points
lw :: (es ~ '[AffReader (AsList env), Dist, State Ⲭ, Observe, Sample])
   => Int                              -- Number of lw iterations per data point
   -> (b -> Model env es a)            -- Model awaiting input variable
   -> [b]                              -- List of model input variables
   -> [LRec env]                       -- List of model observed variables
   -> Sampler (TraceLW a)              -- List of n likelihood weightings for each data point
lw n model xs ys = do
  concat <$> zipWithM (\x y -> lwNsteps n y (model x)) xs ys

-- | Run LW n times for a single data point
lwNsteps :: (es ~ '[AffReader (AsList env), Dist, State Ⲭ, Observe, Sample])
  => Int
  -> LRec env
  -> Model env es a
  -> Sampler (TraceLW a)
lwNsteps n env model = replicateM n (runLW env model)

-- | Run LW once for single data point
runLW :: es ~ '[AffReader (AsList env), Dist, State Ⲭ, Observe, Sample]
  => LRec env -> Model env es a
  -> Sampler (a, Ⲭ, Double)
runLW env model = do
  (((x, ys), samples), p) <- (runSample
                            . runObserve
                            . runState Map.empty
                            . transformLW
                            . runDist
                            . runAffReader env
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
              DistInt (Just d)    -> Free u (\x -> do modify (updateMapⲬ α (unsafeCoerce x :: Int))
                                                      loop (k x))
              DistDouble (Just d) -> Free u (\x -> do modify (updateMapⲬ α (unsafeCoerce x :: Double))
                                                      loop (k x))
              DistBool (Just d)   -> Free u (\x -> do modify (updateMapⲬ α (unsafeCoerce x :: Bool))
                                                      loop (k x))
              DistDoubles (Just d) -> Free u (\x -> do modify (updateMapⲬ α (unsafeCoerce x :: [Double]))
                                                       loop (k x))
              d@CategoricalDist {} -> Free u (\x ->  do modify (Map.insert α (OpenSum.inj x :: OpenSum PrimVal))
                                                        loop (k x))
      _ -> Free u (loop . k)

runObserve :: Member Sample rs => Freer (Observe : rs) a -> Freer rs (a, Double)
runObserve = loop 0
  where
  loop :: Member Sample rs => Double -> Freer (Observe : rs) a -> Freer rs (a, Double)
  loop p (Pure x) = return (x, p)
  loop p (Free u k) = do
    case decomp u of
      Right (Observe d y α)
        -> case d of
            DistDoubles (Just d) ->
              do let p' = prob d (unsafeCoerce y :: [Double])
                 prinT $ "Prob of observing " ++ show (unsafeCoerce y :: [Double]) ++ " from " ++ show d ++ " is " ++ show p'
                 loop (p + p') (k y)
            DistBool (Just d) ->
              do let p' = prob d (unsafeCoerce y :: Bool)
                 prinT $ "Prob of observing " ++ show (unsafeCoerce y :: Bool) ++ " from " ++ show d ++ " is " ++ show p'
                 loop (p + p') (k y)
            DistDouble (Just d) ->
              do  let p' = prob d (unsafeCoerce y :: Double)
                  prinT $ "Prob of observing " ++ show (unsafeCoerce y :: Double) ++ " from " ++ show d ++ " is " ++ show p'
                  loop (p + p') (k y)
            DistInt (Just d) ->
              do let p' = prob d (unsafeCoerce y :: Int)
                 prinT $ "Prob of observing " ++ show (unsafeCoerce y :: Int) ++ " from " ++ show d ++ " is " ++ show p'
                 loop (p + p') (k y)
            d@CategoricalDist {} ->
              do let p' = prob d y
                 prinT $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show p'
                 loop (p + p') (k y)
            _ -> undefined
      Left  u'  -> Free u' (loop p . k)

runSample :: Freer '[Sample] a -> Sampler a
runSample = loop
  where
  loop :: Freer '[Sample] a -> Sampler a
  loop (Pure x) = return x
  loop (Free u k) =
    let x = prj u
    in case x of
      (Just (Printer s) :: Maybe (Sample x)) ->
        liftS (putStrLn s) >> loop (k ())
      Just (Sample d α) ->
        liftS (putStrLn $ ">> : " ++ show α) >> sample d >>= loop . k
      _         -> error "Impossible: Nothing cannot occur"
