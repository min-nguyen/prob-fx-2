{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
module Old.EffExample where

import Prog
import Effects.Reader
import Control.Monad.State

data WriterE w a where
  TellE :: w -> WriterE w ()

runWriterE :: Monoid w => Prog (WriterE w ': es) a -> Prog es (a, w)
runWriterE = handleRelaySt mempty (\w a -> return (a, w))
  (\w (TellE w') k -> k (w <> w') ())

data ReaderE env a where
  AskE :: ReaderE env env

runReaderE :: env -> Prog (ReaderE env ': es) a -> Prog es a
runReaderE env = handleRelay return (\AskE k -> k env)

runFrontPrependRW ::
  Prog (ReaderE Int ': es) a -> Prog (WriterE [Int] ': ReaderE Int ': es) a
runFrontPrependRW = installFront return
  (\x tx k ->
      case tx of AskE -> do call (TellE [x])
                            k x)

runInstallPrependRW :: forall es a . Member (ReaderE Int) es =>
  Prog es a -> Prog (WriterE [Int] ': es) a
runInstallPrependRW = install @(ReaderE Int) return
  (\x tx k ->
      case tx of AskE -> do call (TellE [x])
                            k x)

runInstallRW :: Members '[ReaderE Int, WriterE [Int]] es =>
  Prog es a -> Prog es a
runInstallRW = installExisting @(ReaderE Int) @(WriterE [Int]) return
  (\x tx k ->
      case tx of AskE -> do call (TellE [x])
                            k x)

runReplaceRW :: forall env es a. env -> Prog (ReaderE env ': es) a -> Prog (WriterE [env] ': es) a
runReplaceRW env = replaceRelayN @('[WriterE [env]]) return
  (\tx k ->
      case tx of AskE -> do call (TellE [env])
                            k env
                            )

-- Just experiments with freer
program :: Prog '[Reader Int, State Int, IO] ()
program = do
  x :: Int <- ask
  y :: Int <- ask
  return ()

testProgram :: IO ()
testProgram = (runM . handleReader 5 . runReader'' (5 :: Int) . runReader'' (5 :: Int)) program

programRW :: Member (ReaderE Int) es => Prog es ()
programRW = do
  x :: Int <- call AskE
  y :: Int <- call AskE
  return ()

runProgramRW :: ((), [Int])
runProgramRW = run $ runWriterE @[Int] $ runReaderE @Int 1 $ runInstallRW programRW

runProgramReplaceRW :: ((), [Int])
runProgramReplaceRW = run $ runWriterE @[Int] $ runReplaceRW 1 programRW