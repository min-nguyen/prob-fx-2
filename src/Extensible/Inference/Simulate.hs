{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
module Extensible.Inference.Simulate where

-- import Data.Extensible hiding (Member)
import Extensible.ModelEnv
import Control.Monad
-- import Control.Monad.Trans.Class
import Extensible.Dist
import Extensible.Freer
import Extensible.Model
import Extensible.Sampler
import Extensible.ObsReader
import Extensible.State
import Extensible.Example as Example
import Extensible.IO

simulate :: (es ~ '[ObsReader env, Dist])
  => Int                             -- Number of iterations per data point
  -> (b -> Model env es a)           -- Model awaiting input variable
  -> [b]                             -- List of model input variables
  -> [ModelEnv env]                      -- List of model observed variables
  -> Sampler [a]
simulate n model xs envs = do
  let runN (x, env) = replicateM n (runSimulate env (model x))
  concat <$> mapM runN (zip xs envs)

-- Simulate multiple model inputs under same environment
simulateSameEnv :: (es ~ '[ObsReader env, Dist])
  => Int                             -- Number of iterations per data point
  -> (b -> Model env es a)           -- Model awaiting input variable
  -> [b]                             -- List of model input variables
  -> ModelEnv env                        -- List of model observed variables
  -> Sampler [a]
simulateSameEnv n model xs env =
  concat <$> mapM (replicateM n . runSimulate env . model) xs

runSimulate :: (es ~ '[ObsReader env, Dist])
 => ModelEnv env -> Model env es a -> Sampler a
runSimulate ys m
  = (runLift . runSample . runObserve . runDist. runObsReader ys ) (runModel m)

simulateWith :: (es ~ '[ObsReader env, Dist])
  => Int                             -- Number of iterations per data point
  -> (b -> Model env (e:es) a)       -- Model awaiting input variable
  -> [b]                             -- List of model input variables
  -> [ModelEnv env]                      -- List of model observed variables
  -> (Model env (e:es) a -> Model env es c)
  -> Sampler [c]
simulateWith n model xs envs h = do
  let runN (x, env) = replicateM n (runSimulateWith env (model x) h)
  concat <$> mapM runN (zip xs envs)

runSimulateWith :: (es ~ '[ObsReader env, Dist])
 => ModelEnv env
 -> Model env (e:es) a
 -> (Model env (e:es) a -> Model env es c)
 -> Sampler c
runSimulateWith ys m h
  = (runLift . runSample . runObserve . runDist . runObsReader ys) (runModel $ h m)

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

-- runReader' :: forall env es a.
--   (Member (State (LRec env)) es) =>
--   LRec env -> Prog (RecReader (AsList env) ': es) a -> Prog es a
-- runReader' env = loop where
--   loop :: Prog (RecReader (AsList env) ': es) a -> Prog es a
--   loop (Val x) = return x
--   loop (Op u k) = case decomp u of
--     Right Ask -> do
--       env' :: LRec env <- get
--       loop (k env)
--     Left  u'  -> Op u' (loop . k)

-- why doesn'e this version work?
-- runBasic ::
--  LRec env
--   -> Model env '[Dist, Observe, RecReader (AsList env), Sample] a -> Sampler a
-- runBasic ys m
--   = runSample $ runReader ys $ runObserve $ runDist $ runModel m
-- but this does:
-- runBasic :: (e ~ AsList env) =>
--  LRec env
--   -> Model env '[Dist, Observe, RecReader (AsList env), Sample] a -> Sampler a
-- runBasic ys m
--   = runSample $ runReader ys $ runObserve $ runDist $ runModel m
-- or this:
-- runBasic3 :: (Member Observe es, Member Sample es) =>
--  LRec env -> Model env (Dist : ObsReader (AsList env) : es) a -> Prog es a
-- runBasic3 env
--   =  fmap fst . runObsReader env . runDist . runModel

runInit :: ModelEnv env -> Model env (ObsReader env : Dist : es) a -> Prog (Observe : Sample : es) a
runInit env m = (runDist . runObsReader env)   (runModel m)

newtype F es a = F { runF :: Member Observe es => Prog es a }

runInit' :: forall es a. (Member Observe es, Member Sample es)
          => F es a -> (Prog es a)
            -- Prog es a
runInit' f = runF f

-- runInit2 ::
--             F es a -> Member Observe rs => Prog rs a
--             -- Prog es a
-- runInit2  = runF

-- runInit3 :: F (Observe : es) a -> Prog es a
-- runInit3  = runObserve . runF