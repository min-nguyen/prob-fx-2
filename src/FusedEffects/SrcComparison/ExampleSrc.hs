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

module FusedEffects.SrcComparison.ExampleSrc where

import qualified Control.Algebra 
import qualified Control.Carrier.Reader

import FusedEffects.SrcComparison.LiftSrc
import FusedEffects.SrcComparison.AlgebraSrc ( Algebra(..), Has, send, run )
import FusedEffects.SrcComparison.ReaderSrc

exampleSrc :: String
exampleSrc = Control.Algebra.run . Control.Carrier.Reader.runReader "hello" $ Control.Carrier.Reader.ask

example :: String
example = run $ runReader "hello" $ ask
