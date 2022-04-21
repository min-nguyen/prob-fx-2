{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
module Old.Inference.Simulate where

-- import Data.Extensible hiding (Member)
import ModelEnv
import Control.Monad
-- import Control.Monad.Trans.Class
import Dist
import Freer
import Model
import Sampler
import ObsReader
import State
import Example as Example
import IO

simulate :: (es ~ '[ObsReader env, Dist])
  => Int                             -- Number of iterations per data point
  -> (b -> Model env es a)           -- Model awaiting input variable
  -> [b]                             -- List of model input variables
  -> [ModelEnv env]                      -- List of model observed variables
  -> Sampler [a]
simulate n model xs envs = do
  let runN (x, env) = replicateM n (runSimulate env (model x))
  concat <$> mapM runN (zip xs envs)

runSimulate :: (es ~ '[ObsReader env, Dist])
 => ModelEnv env -> Model env es a -> Sampler a
runSimulate ys m
  = (runLift . runSample . runObserve . runDist. runObsReader ys ) (runModel m)

runObserve :: Prog (Observe : es) a -> Prog es  a
runObserve (Val x) = return x
runObserve (Op u k) = case u of
  ObsPatt d y α ->
    let p = logProb d y
    in  runObserve (k y)
  DecompLeft u'  ->
    Op u' (runObserve . k)

runSample :: Prog (Sample : es) a -> Prog (Lift Sampler : es) a
runSample (Val x) = return x
runSample (Op u k) = case u of
    PrintPatt s -> do
      (lift . liftS) (putStrLn s)
      runSample (k ())
    SampPatt d α -> do
      y <- lift (sample d )
      runSample (k y)
    _        -> error "Impossible: Nothing cannot occur"