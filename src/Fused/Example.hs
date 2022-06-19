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

import Data.Functor.Identity
import Fused.Algebra ( Algebra(..), Has, send, run )
import Fused.Sum
import Fused.Writer
import Fused.Reader
import Fused.Lift

prog :: (Monad m, Has (ReaderEff Int) sig m, Has (WriterEff String) sig m) => m Int
prog = do
  send (Tell "hi")
  x  :: Int <- send Ask
  send (Tell "bye") 
  pure x

hdlr1 :: WriterT String (Reader Int) a -> (a, String)
hdlr1 = (`runReader` (5 :: Int)) . runWriterT @String

apply :: (Algebra sig m, Member (ReaderEff Int) sig, Member (WriterEff String) sig) =>
         (m Int -> t) -> t
apply runProg = runProg prog

prog' :: forall m sig. (Algebra sig m) => m (Int, String)
prog' = hdlr2 $ do
  send (Tell "hi")
  x  :: Int <- send Ask
  send (Tell "bye") 
  pure x

hdlr2 :: Monad m => WriterT String (ReaderT Int m) a -> m (a, String)
hdlr2 = (`runReaderT` (5 :: Int)) . runWriterT @String
