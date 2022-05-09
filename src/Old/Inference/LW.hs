{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE TypeOperators #-}
module Old.Inference.LW where

import qualified Data.Map as Map
import Data.Map (Map)
import Env
import Effects.ObsReader
import Control.Monad
import Control.Monad.Trans.Class
import Unsafe.Coerce
import Effects.Dist
import qualified Example as Example
import Prog
import Model hiding (runModelFree)
import Sampler
import Effects.State ( modify, handleState, State )
import Old.Trace
import qualified OpenSum as OpenSum
import OpenSum (OpenSum(..))

type TraceLW a = [(a, STrace, Double)]

-- | Run LW n times for multiple data points
lw :: (es ~ '[ObsReader env, Dist])
   => Int                              -- Number of lw iterations per data point
   -> (b -> Model env es a)            -- Model awaiting input variable
   -> [b]                              -- List of model input variables
   -> [Env env]                       -- List of model observed variables
   -> Sampler (TraceLW a)              -- List of n likelihood weightings for each data point
lw n model xs envs = do
  let runN (x, env) = replicateM n (runLW env (model x))
  concat <$> mapM runN (zip xs envs)

-- | Run LW once for single data point
runLW :: es ~ '[ObsReader env, Dist]
  => Env env -> Model env es a
  -> Sampler (a, STrace, Double)
runLW env model = do
  ((x, samples), p) <- (runSample
                            . runObserve
                            . handleState Map.empty
                            . transformLW
                            . handleDist
                            . handleObsRead env)
                            (runModel model)
  return (x, samples, p)

runLWpaper :: es ~ '[ObsReader env, Dist]
  => Env env -> Model env es a
  -> Sampler ((a, STrace), Double)
runLWpaper env m =
  (runSample . runObserve . handleState Map.empty
   . transformLW . handleDist . handleObsRead env) (runModel m)

transformLW :: (Member Sample es) => Prog es a -> Prog (State STrace ': es) a
transformLW = install return
  (\x tx k -> case tx of
      Sample d α -> case distDict d of
        Dict -> do updateSTrace α x
                   k x
      Printer s  -> k ()
  )

transformLW' :: (Member (State STrace) es, Member Sample es)
  => Prog es a -> Prog es a
transformLW' (Val x) = return x
transformLW' (Op u k) = case u  of
    SampPatt d α -> Op u (\x -> do  updateSTrace α x
                                    transformLW' (k x))
    _ -> Op u (transformLW' . k)

-- transformLW' :: (Member (State Trace) es, Member Sample es)
--   => Prog es a -> Prog es a
-- transformLW' = replaceRelay return undefined

runObserve :: Member Sample es => Prog (Observe : es) a -> Prog es (a, Double)
runObserve = loop 0
  where
  loop :: Member Sample es => Double -> Prog (Observe : es) a -> Prog es (a, Double)
  loop logp (Val x) = return (x, exp logp)
  loop logp (Op u k) = case  u of
      ObsPatt d y α -> do
        let logp' = logProb d y
        -- prinT $ "Prob of observing " ++ show y ++ " from " ++ show d ++ " is " ++ show logp'
        loop (logp + logp') (k y)
      DecompLeft u' -> Op u' (loop logp . k)

runSample :: Prog '[Sample] a -> Sampler a
runSample = loop
  where
  loop :: Prog '[Sample] a -> Sampler a
  loop (Val x) = return x
  loop (Op u k) =
    case u of
      SampPatt d α ->
        -- liftS (putStrLn $ ">> : " ++ show α) >>
        sample d >>= loop . k
      PrintPatt s  ->
        liftS (putStrLn s) >>= loop . k
      _         -> error "Impossible: Nothing cannot occur"
