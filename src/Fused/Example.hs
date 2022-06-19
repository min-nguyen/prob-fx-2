{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications, UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
{-# LANGUAGE TupleSections #-}

module Fused.Example where

import Fused.Algebra
import Fused.Sum
import Fused.Writer
import Fused.Reader
import Data.Functor.Identity
import Fused.Lift

prog :: (Monad m, Has (ReaderEff Int) sig m, Has (WriterEff String) sig m) => m Int
prog = do
  send (Tell "hi")
  x  :: Int <- send Ask
  send (Tell "bye") 
  pure x

hdlr1 :: WriterT String (Reader Int) a -> (a, String)
hdlr1 = (`runReader` (5 :: Int)) . runWriterT @String

prog' :: (Monad m, Has (ReaderEff Int) sig m, Has (WriterEff String) sig m) => m Int
prog' = do
  send (Tell "hi")
  x  :: Int <- send Ask
  send (Tell "bye") 
  pure x

hdlr2 :: WriterT String (ReaderT Int Identity) a -> (a, String)
hdlr2 = run . (`runReaderT` (5 :: Int)) . runWriterT @String

-- f = run prog'

example :: String
example = run $ runReaderT (ask) "hello" 
