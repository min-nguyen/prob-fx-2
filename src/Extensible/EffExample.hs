{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Extensible.EffExample where

import Extensible.Freer
import Extensible.Reader

-- Just experiments with freer

program :: Freer '[Reader Int, IO] ()
program = do
  x :: Int <- ask
  return ()

-- testProgram = (runM . runReader 5 . runReader'' (5 :: Int) . runReader'' (5 :: Int)) program

programRW :: Member (WriterE [Int]) ts => Member (ReaderE Int) ts => Freer ts ()
programRW = do
  x :: Int <- send AskE
  return ()

-- runProgramRW ::
runProgramRW :: ((), [Int])
runProgramRW = run $  runWriterE @[Int] $ runReaderE @Int 1 $ runRW programRW