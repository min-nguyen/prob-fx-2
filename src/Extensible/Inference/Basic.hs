{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
module Extensible.Inference.Basic where

-- import Data.Extensible hiding (Member)
import Extensible.OpenProduct
import Control.Monad
import Control.Monad.Trans.Class
import Extensible.Dist
import Extensible.Freer
import Extensible.Model
import Extensible.Sampler
import Extensible.AffineReader
import Extensible.State
import Extensible.Example as Example

basic :: (ts ~ '[Dist, Observe, AffReader env, Sample])
  => Int                             -- Number of iterations per data point
  -> (b -> Model env ts a)           -- Model awaiting input variable
  -> [b]                             -- List of model input variables
  -> [LRec env]                      -- List of model observed variables
  -> Sampler [a]
basic n model xs ys = do
  concat <$> zipWithM (\x y -> basicNsteps n y (model x)) xs ys

basicNsteps :: (ts ~ '[Dist, Observe, AffReader env, Sample])
  => Int
  -> LRec env
  -> Model env ts a
  -> Sampler [a]
basicNsteps n ys model = replicateM n (runBasic ys model)

runBasic :: (ts ~ '[Dist, Observe, AffReader env, Sample])
 => LRec env -> Model env ts a -> Sampler a
runBasic ys
  = runSample . runAffReader ys . runObserve . runDist . runModel

runObserve :: Freer (Observe : ts) a -> Freer ts  a
runObserve (Pure x) = return x
runObserve (Free u k) = case decomp u of
  Right (Observe d y α)
    -> let p = logProb d y
        in  runObserve (k y)
  Left  u'  -> Free u' (runObserve . k)

runSample :: Freer '[Sample] a -> Sampler a
runSample (Pure x) = return x
runSample (Free u k) = case prj u of
    Just (Printer s) ->
      liftS (putStrLn s) >> runSample (k ())
    Just (Sample d α) -> sample d >>= runSample . k
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
--  LRec env -> Model env (Dist : AffReader (AsList env) : ts) a -> Freer ts a
-- runBasic3 env
--   =  fmap fst . runAffReader env . runDist . runModel

runInit :: (Member Observe ts, Member Sample ts)
          => LRec env -> Model env (Dist : AffReader env : ts) a -> Freer ts a
runInit env = runAffReader env . runDist . runModel