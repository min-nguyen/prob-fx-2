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

updateTrace :: forall ts x. (Member (State Ⲭ) ts, OpenSum.Member x PrimVal) => Addr -> x -> Freer ts ()
updateTrace α x = modify (Map.insert α (OpenSum.inj x) :: Ⲭ -> Ⲭ)

-- | Run LW n times for multiple data points
lw :: (ts ~ '[AffReader env, Dist, Observe, Sample])
   => Int                              -- Number of lw iterations per data point
   -> (b -> Model env ts a)            -- Model awaiting input variable
   -> [b]                              -- List of model input variables
   -> [LRec env]                       -- List of model observed variables
   -> Sampler (TraceLW a)              -- List of n likelihood weightings for each data point
lw n model xs envs = do
  let runN (x, env) = replicateM n (runLW env (model x))
  concat <$> mapM runN (zip xs envs)

-- | Run LW once for single data point
runLW :: ts ~ '[AffReader env, Dist, Observe, Sample]
  => LRec env -> Model env ts a
  -> Sampler (a, Ⲭ, Double)
runLW env model = do
  ((x, samples), p) <- (runSample
                            . runObserve
                            . runState Map.empty
                            . transformLW
                            . runDist
                            . runAffReader env)
                            (runModel model)
  return (x, samples, p)

runLWpaper :: ts ~ '[AffReader env, Dist,  Observe, Sample]
  => LRec env -> Model env ts a
  -> Sampler ((a, Ⲭ), Double)
runLWpaper env m =
  (runSample . runObserve . runState Map.empty
   . transformLW . runDist . runAffReader env) (runModel m)

transformLW' :: (Member (State Ⲭ) ts, Member Sample ts)
  => Freer ts a -> Freer ts a
transformLW' (Pure x) = return x
transformLW' (Free u k) = case u  of
    Samp d α -> case d of
      DistInt d    ->
        Free u (\x -> do  updateTrace α (unsafeCoerce x :: Int)
                          transformLW' (k x))
      DistDouble d -> Free u (\x -> do updateTrace α (unsafeCoerce x :: Double)
                                       transformLW' (k x))
      DistBool  d  -> Free u (\x -> do updateTrace α (unsafeCoerce x :: Bool)
                                       transformLW' (k x))
      DistDoubles d -> Free u (\x -> do updateTrace α (unsafeCoerce x :: [Double])
                                        transformLW' (k x))
      d@CategoricalDist {} -> Free u (\x ->  do modify (Map.insert α (OpenSum.inj x :: OpenSum PrimVal))
                                                transformLW' (k x))
      d@DeterministicDist {} -> Free u (\x ->  do modify (Map.insert α (OpenSum.inj x :: OpenSum PrimVal))
                                                  transformLW' (k x))
      _ -> error "error"
    _ -> Free u (transformLW' . k)

transformLW :: (Member Sample ts) => Freer ts a -> Freer (State Ⲭ ': ts) a
transformLW = install return
  (\x tx k -> case tx of
      Sample d α -> case d of
        DistInt  d        -> do updateTrace α (unsafeCoerce x :: Int)
                                k x
        DistDouble  d     -> do updateTrace α (unsafeCoerce x :: Double)
                                k x
        DistBool  d       -> do updateTrace α (unsafeCoerce x :: Bool)
                                k x
        DistDoubles  d    -> do updateTrace α (unsafeCoerce x :: [Double])
                                k x
        d@CategoricalDist {}    -> do modify (Map.insert α (OpenSum.inj x :: OpenSum PrimVal))
                                      k x
        d@DeterministicDist {}  -> do modify (Map.insert α (OpenSum.inj x :: OpenSum PrimVal))
                                      k x
        _ -> error ""
      Printer s  -> k ()
  )

-- transformLW' :: (Member (State Ⲭ) ts, Member Sample ts)
--   => Freer ts a -> Freer ts a
-- transformLW' = replaceRelay return undefined

runObserve :: Member Sample ts => Freer (Observe : ts) a -> Freer ts (a, Double)
runObserve = loop 0
  where
  loop :: Member Sample ts => Double -> Freer (Observe : ts) a -> Freer ts (a, Double)
  loop logp (Pure x) = return (x, exp logp)
  loop logp (Free u k) = case decomp u of
      Right (Observe d y α)
        -> let r = logProb d y
           in case d of
            DistDoubles  d ->
              do let logp' = logProb d (unsafeCoerce y :: [Double])
                 prinT $ "Prob of observing " ++ show (unsafeCoerce y :: [Double]) ++ " from " ++ show d ++ " is " ++ show logp'
                 loop (logp + logp') (k y)
            DistBool  d ->
              do let logp' = logProb d (unsafeCoerce y :: Bool)
                 prinT $ "Prob of observing " ++ show (unsafeCoerce y :: Bool) ++ " from " ++ show d ++ " is " ++ show logp'
                 loop (logp + logp') (k y)
            DistDouble  d ->
              do  let logp' = logProb d (unsafeCoerce y :: Double)
                  prinT $ "Prob of observing " ++ show (unsafeCoerce y :: Double) ++ " from " ++ show d ++ " is " ++ show logp'
                  loop (logp + logp') (k y)
            DistInt  d ->
              do let logp' = logProb d (unsafeCoerce y :: Int)
                 prinT $ "Prob of observing " ++ show (unsafeCoerce y :: Int) ++ " from " ++ show d ++ " is " ++ show logp'
                 loop (logp + logp') (k y)
            d@CategoricalDist {} ->
              do let logp' = logProb d y
                 prinT $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show logp'
                 loop (logp + logp') (k y)
            d@DeterministicDist {} ->
              do let logp' = logProb d y
                 prinT $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show logp'
                 loop (logp + logp') (k y)
            _ -> undefined
      Left  u'  -> Free u' (loop logp . k)

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
