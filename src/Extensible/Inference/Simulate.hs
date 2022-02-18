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
import Extensible.OpenProduct
import Control.Monad
import Control.Monad.Trans.Class
import Extensible.Dist
import Extensible.Freer
import Extensible.Model
import Extensible.Sampler
import Extensible.ObsReader
import Extensible.State
import Extensible.Example as Example

simulate :: (ts ~ '[Dist, Observe, ObsReader env, Sample])
  => Int                             -- Number of iterations per data point
  -> (b -> Model env ts a)           -- Model awaiting input variable
  -> [b]                             -- List of model input variables
  -> [LRec env]                      -- List of model observed variables
  -> Sampler [a]
simulate n model xs envs = do
  let runN (x, env) = replicateM n (runSimulate env (model x))
  concat <$> mapM runN (zip xs envs)

-- Simulate multiple model inputs under same environment
simulateSameEnv :: (ts ~ '[Dist, Observe, ObsReader env, Sample])
  => Int                             -- Number of iterations per data point
  -> (b -> Model env ts a)           -- Model awaiting input variable
  -> [b]                             -- List of model input variables
  -> LRec env                        -- List of model observed variables
  -> Sampler [a]
simulateSameEnv n model xs env =
  concat <$> mapM (replicateM n . runSimulate env . model) xs

runSimulate :: (ts ~ '[Dist, Observe, ObsReader env, Sample])
 => LRec env -> Model env ts a -> Sampler a
runSimulate ys m
  = (runSample . runObsReader ys . runObserve . runDist) (runModel m)

simulateWith :: (ts ~ '[Dist, Observe, ObsReader env, Sample])
  => Int                             -- Number of iterations per data point
  -> (b -> Model env (t:ts) a)       -- Model awaiting input variable
  -> [b]                             -- List of model input variables
  -> [LRec env]                      -- List of model observed variables
  -> (Model env (t:ts) a -> Model env ts c)
  -> Sampler [c]
simulateWith n model xs envs h = do
  let runN (x, env) = replicateM n (runSimulateWith env (model x) h)
  concat <$> mapM runN (zip xs envs)

runSimulateWith :: (ts ~ '[Dist, Observe, ObsReader env, Sample])
 => LRec env
 -> Model env (t:ts) a
 -> (Model env (t:ts) a -> Model env ts c)
 -> Sampler c
runSimulateWith ys m h
  = (runSample . runObsReader ys . runObserve . runDist) (runModel $ h m)

runObserve :: Freer (Observe : ts) a -> Freer ts  a
runObserve (Pure x) = return x
runObserve (Free u k) = case u of
  ObsPatt d y α ->
    let p = logProb d y
    in  runObserve (k y)
  DecompLeft u'  ->
    Free u' (runObserve . k)

runSample :: Freer '[Sample] a -> Sampler a
runSample (Pure x) = return x
runSample (Free u k) = case u of
    PrintPatt s ->
      liftS (putStrLn s) >> runSample (k ())
    SampPatt d α ->
      sample d >>= runSample . k
    _        -> error "Impossible: Nothing cannot occur"

-- runReader' :: forall env ts a.
--   (Member (State (LRec env)) ts) =>
--   LRec env -> Freer (RecReader (AsList env) ': ts) a -> Freer ts a
-- runReader' env = loop where
--   loop :: Freer (RecReader (AsList env) ': ts) a -> Freer ts a
--   loop (Pure x) = return x
--   loop (Free u k) = case decomp u of
--     Right Ask -> do
--       env' :: LRec env <- get
--       loop (k env)
--     Left  u'  -> Free u' (loop . k)

-- why doesn't this version work?
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
-- runBasic3 :: (Member Observe ts, Member Sample ts) =>
--  LRec env -> Model env (Dist : ObsReader (AsList env) : ts) a -> Freer ts a
-- runBasic3 env
--   =  fmap fst . runObsReader env . runDist . runModel

runInit :: (Member Observe ts, Member Sample ts)
          => LRec env -> Model env (Dist : ObsReader env : ts) a -> Freer ts a
runInit env m = (runObsReader env . runDist)  (runModel m)

newtype F ts a = F { runF :: Member Observe ts => Freer ts a }

runInit' :: forall ts a. (Member Observe ts, Member Sample ts)
          => F ts a -> (Freer ts a)
            -- Freer ts a
runInit' f = runF f

-- runInit2 ::
--             F ts a -> Member Observe rs => Freer rs a
--             -- Freer ts a
-- runInit2  = runF

-- runInit3 :: F (Observe : ts) a -> Freer ts a
-- runInit3  = runObserve . runF