{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
module Extensible.EffExample where

import Extensible.Freer
import Extensible.Reader

data WriterE w a where
  TellE :: w -> WriterE w ()

runWriterE :: Monoid w => Freer (WriterE w ': ts) a -> Freer ts (a, w)
runWriterE = handleRelaySt mempty (\w a -> return (a, w))
  (\w (TellE w') k -> k (w <> w') ())

data ReaderE env a where
  AskE :: ReaderE env env

runReaderE :: env -> Freer (ReaderE env ': ts) a -> Freer ts a
runReaderE env = handleRelay return (\AskE k -> k env)

runFrontPrependRW ::
  Freer (ReaderE Int ': ts) a -> Freer (WriterE [Int] ': ReaderE Int ': ts) a
runFrontPrependRW = installFront return
  (\x tx k ->
      case tx of AskE -> do send (TellE [x])
                            k x)

runInstallPrependRW :: forall ts a . Member (ReaderE Int) ts =>
  Freer ts a -> Freer (WriterE [Int] ': ts) a
runInstallPrependRW = installPrepend @(ReaderE Int) return
  (\x tx k ->
      case tx of AskE -> do send (TellE [x])
                            k x)

runInstallRW :: Members '[ReaderE Int, WriterE [Int]] ts =>
  Freer ts a -> Freer ts a
runInstallRW = install @(ReaderE Int) @(WriterE [Int]) return
  (\x tx k ->
      case tx of AskE -> do send (TellE [x])
                            k x)

runReplaceRW :: env -> Freer (ReaderE env ': ts) a -> Freer (WriterE [env] ': ts) a
runReplaceRW env = replaceRelay return
  (\tx k ->
      case tx of AskE -> do send (TellE [env])
                            k env
                            )

-- Just experiments with freer
program :: Freer '[Reader Int, IO] ()
program = do
  x :: Int <- ask
  y :: Int <- ask
  return ()

testProgram :: IO ()
testProgram = (runM . runReader 5 . runReader'' (5 :: Int) . runReader'' (5 :: Int)) program

programRW :: Member (ReaderE Int) ts => Freer ts ()
programRW = do
  x :: Int <- send AskE
  y :: Int <- send AskE
  return ()

runProgramRW :: ((), [Int])
runProgramRW = run $ runWriterE @[Int] $ runReaderE @Int 1 $ runInstallRW programRW

runProgramReplaceRW :: ((), [Int])
runProgramReplaceRW = run $ runWriterE @[Int] $ runReplaceRW 1 programRW