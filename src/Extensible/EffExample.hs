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

runRWFront ::
  Freer (ReaderE Int ': ts) a -> Freer (WriterE [Int] ': ReaderE Int ': ts) a
runRWFront = installFront return
  (\x tx k ->
      case tx of AskE -> do send (TellE [x])
                            k x)

runRWPrepend :: forall ts a . Member (ReaderE Int) ts =>
  Freer ts a -> Freer (WriterE [Int] ': ts) a
runRWPrepend = installPrepend @(ReaderE Int) return
  (\x tx k ->
      case tx of AskE -> do send (TellE [x])
                            k x)

runRW :: Members '[ReaderE Int, WriterE [Int]] ts =>
  Freer ts a -> Freer ts a
runRW = install @(ReaderE Int) @(WriterE [Int]) return
  (\x tx k ->
      case tx of AskE -> do send (TellE [x])
                            k x)

-- Just experiments with freer
program :: Freer '[Reader Int, IO] ()
program = do
  x :: Int <- ask
  return ()

testProgram :: IO ()
testProgram = (runM . runReader 5 . runReader'' (5 :: Int) . runReader'' (5 :: Int)) program

programRW :: Member (WriterE [Int]) ts => Member (ReaderE Int) ts => Freer ts ()
programRW = do
  x :: Int <- send AskE
  return ()

runProgramRW :: ((), [Int])
runProgramRW = run $ runWriterE @[Int] $ runReaderE @Int 1 $ runRW programRW