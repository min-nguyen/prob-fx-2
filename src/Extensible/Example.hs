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
{-# LANGUAGE OverlappingInstances #-}

module Extensible.Example where

import Extensible.Freer
import Extensible.Reader
import Extensible.Writer
import Extensible.Model
import Extensible.Dist
import Extensible.IO
import Control.Monad
import Unsafe.Coerce
import Data.Kind (Constraint)
import GHC.TypeLits
import Data.Typeable
import Data.Extensible ( Record, Assoc((:>)) )
import Sample
{- Probabilistic programs -}

type LinRegrEnv =     
    '[  "y"    ':>  Double
     ]

-- linearRegression :: 
--   Double -> Double -> Double -> Model s rs Double
-- linearRegression μ σ x = do
--   normal (μ + x) σ Nothing

-- linearRegression :: (KnownNat n, FindElem (Reader (Record (Maybes s))) rs,
--  FindElem (Lift Sampler) rs, HasVar s "y" Double, 
--  FindElem (Reader (Record (Maybes s))) rs) =>
--  Double -> Double -> Double -> Freer rs Double
-- linearRegression μ σ x = do
--   normal' (μ + x) σ y


{- Non probabilistic programs-}

example :: (Member (Reader Int) rs, Member (Writer String) rs) 
        => Freer rs Int
example = do
  put "hi"
  x :: Int <- get
  put "hi"
  return 5

prog :: (Member (Reader Int) rs, Member (Writer String) rs) 
        => Freer rs Int
prog = Free (inj $ Put "hi") Pure >>= \() -> 
         Free (inj Get) Pure >>= \(x :: Int) -> 
           Free (inj $ Put (show x)) Pure >>= \() -> 
             Pure 5

prog' :: (Member (Reader Int) rs, Member (Writer String) rs) => Freer rs Int
prog' = Free (inj $ Put "hi") (Pure >=> \() -> 
          Free (inj Get) (Pure >=> \(x :: Int) -> 
            Free (inj $ Put (show x)) (Pure >=> \() -> 
              Pure 5)))
 
prog'' :: (Member (Reader Int) rs, Member (Writer String) rs) => Freer rs Int
prog'' = Free (inj $ Put "hi") (\() -> 
          Free (inj Get) (\(x :: Int) -> 
            Free (inj $ Put (show x)) (\() -> 
              Pure 5)))

example' :: forall env. Freer '[Reader env, Writer String] Int
example' = do
  put "hi"
  x :: env <- get
  return 5

exampleR :: forall rs . (Member (Reader Int) rs) 
        => Freer rs Int
exampleR = do
  x :: Int <- get
  return 5

exampleW :: forall rs. (Member (Writer String) rs) 
        => Freer rs Int
exampleW = do
  put "hi"
  return 5

exampleIO :: forall rs. (SetMember Lift (Lift IO) rs) => Freer rs ()
exampleIO = do
  lift $ print "hi" 
  return ()

runEx :: (Int, String)
runEx = (run . runWriter . runReader (5 :: Int)) example