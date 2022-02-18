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
import Extensible.ModelEnv
import Extensible.ObsReader
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

type SampleMap = Map Addr (OpenSum PrimVal)

type TraceLW a = [(a, SampleMap, Double)]

updateTrace :: forall ts x. (Member (State SampleMap) ts, OpenSum.Member x PrimVal) => Addr -> x -> Freer ts ()
updateTrace α x = modify (Map.insert α (OpenSum.inj x) :: SampleMap -> SampleMap)

-- | Run LW n times for multiple data points
lw :: (ts ~ '[ObsReader env, Dist, Observe, Sample])
   => Int                              -- Number of lw iterations per data point
   -> (b -> Model env ts a)            -- Model awaiting input variable
   -> [b]                              -- List of model input variables
   -> [ModelEnv env]                       -- List of model observed variables
   -> Sampler (TraceLW a)              -- List of n likelihood weightings for each data point
lw n model xs envs = do
  let runN (x, env) = replicateM n (runLW env (model x))
  concat <$> mapM runN (zip xs envs)

-- | Run LW once for single data point
runLW :: ts ~ '[ObsReader env, Dist, Observe, Sample]
  => ModelEnv env -> Model env ts a
  -> Sampler (a, SampleMap, Double)
runLW env model = do
  ((x, samples), p) <- (runSample
                            . runObserve
                            . runState Map.empty
                            . transformLW
                            . runDist
                            . runObsReader env)
                            (runModel model)
  return (x, samples, p)

runLWpaper :: ts ~ '[ObsReader env, Dist,  Observe, Sample]
  => ModelEnv env -> Model env ts a
  -> Sampler ((a, SampleMap), Double)
runLWpaper env m =
  (runSample . runObserve . runState Map.empty
   . transformLW . runDist . runObsReader env) (runModel m)

transformLW :: (Member Sample ts) => Freer ts a -> Freer (State SampleMap ': ts) a
transformLW = install return
  (\x tx k -> case tx of
      Sample d α -> case distDict d of
        Dict -> do updateTrace α x
                   k x
      Printer s  -> k ()
  )

transformLW' :: (Member (State SampleMap) ts, Member Sample ts)
  => Freer ts a -> Freer ts a
transformLW' (Pure x) = return x
transformLW' (Free u k) = case u  of
    SampPatt d α -> Free u (\x -> do  updateTrace α x
                                      transformLW' (k x))
    _ -> Free u (transformLW' . k)

-- transformLW' :: (Member (State SampleMap) ts, Member Sample ts)
--   => Freer ts a -> Freer ts a
-- transformLW' = replaceRelay return undefined

runObserve :: Member Sample ts => Freer (Observe : ts) a -> Freer ts (a, Double)
runObserve = loop 0
  where
  loop :: Member Sample ts => Double -> Freer (Observe : ts) a -> Freer ts (a, Double)
  loop logp (Pure x) = return (x, exp logp)
  loop logp (Free u k) = case  u of
      ObsPatt d y α -> do
        let logp' = logProb d y
        -- prinT $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show logp'
        loop (logp + logp') (k y)
      DecompLeft u' -> Free u' (loop logp . k)

runSample :: Freer '[Sample] a -> Sampler a
runSample = loop
  where
  loop :: Freer '[Sample] a -> Sampler a
  loop (Pure x) = return x
  loop (Free u k) =
    case u of
      SampPatt d α ->
        -- liftS (putStrLn $ ">> : " ++ show α) >>
        sample d >>= loop . k
      PrintPatt s  ->
        liftS (putStrLn s) >>= loop . k
      _         -> error "Impossible: Nothing cannot occur"
