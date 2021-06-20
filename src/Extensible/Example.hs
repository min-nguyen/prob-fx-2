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

import Statistics.Distribution
import Extensible.Freer
import Extensible.Reader
import Extensible.Writer
import Extensible.Model
import Extensible.Dist
import Extensible.IO
import Extensible.Sampler
import Control.Monad
import Unsafe.Coerce
import Data.Kind (Constraint)
import GHC.TypeLits
import Data.Typeable
import Data.Extensible hiding (Member)


{- Probabilistic programs -}

-- Bayesian network
data NN = NN { biases  :: [Double],
               weights :: [Double],
               sigm   :: Double }

data Data = Data { xVal :: Double,
                   yVal :: Double }

dot :: [Double] -> [Double] -> Double
dot [] _ = 0
dot _ [] = 0
dot (x:xs) (y:ys) = x * y + dot xs ys

forwardNN :: NN -> Double -> Double
forwardNN (NN bs ws _) x =
  ws `dot` fmap activation (map (x -) bs)
  where activation x = if x < 0 then 0 else 1

likelihood :: NN -> Data -> Double
likelihood nn (Data xObs yObs) =
  let ySigma = sigm nn
      yMean  = forwardNN nn xObs
  in  prob (NormalDist yMean ySigma (Just yObs)) yObs

nn :: NN
nn = NN { biases = [1,5,8], weights = [2, -5, 1], sigm = 2.0}

points :: [(Double, Double, Double)]
points = [ (x, y, exp . log $ likelihood nn (Data x y)) | x <- [0 .. 10], y <- [-10 .. 10]]

-- uniformList :: (Double, Double) -> Int -> [Double]
-- uniformList (min, max) n = replicateM n (uniform)

-- priorNN :: Int -> Model s es NN
-- priorNN n_nodes = do
--   bias   <-

-- Logistic regression
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp((-1) * x))

logisticRegression :: forall rs s. HasVar s "label" Bool =>
 Double -> Model s rs Bool
logisticRegression x = do
  m     <- normal 0 1
  b     <- normal 0 1
  sigma <- gamma 1 1
  y     <- normal (m * x + b) sigma
  bernoulli' (sigmoid y) label

type LinRegrEnv =
    '[  "y"    ':>  Double
     ]

-- Liniear regression
linearRegression :: forall rs s.
  Double -> Double -> Double -> Model s rs Double
linearRegression μ σ x =  --do
  normal (μ + x) σ

linearRegression' :: forall rs s. HasVar s "y" Double =>
  Double -> Double -> Double -> Model s rs Double
linearRegression' μ σ x = do
  normal' (μ + x) σ y

arbitraryModel :: forall es s. HasVar s "y" Double =>
  Double -> Double -> Double -> Model s es Double
arbitraryModel μ σ x = do
  normal'  (μ + x) σ y
  replicateM 4 $ normal (μ + x) σ
  normal'  (μ + x) σ y

ifModel :: forall rs s. Double -> Model s rs Double
ifModel p = do
  x1 <- bernoulli p
  x2 <- if x1 then normal 0 1 else pure 0
  x3 <- bernoulli p
  return 0

ifModel' :: Double -> Model s rs Double
ifModel' p = Model $
  Free (inj $ BernoulliDist p Nothing) Pure >>= \x1 ->
    if x1 then Free (inj $ NormalDist 0 1 Nothing) Pure else return 0

{- Non probabilistic programs-}

example :: (Member (Reader Int) rs, Member (Writer String) rs)
        => Freer rs Int
example = do
  tell "hi"
  x :: Int <- ask
  tell "hi"
  return 5

prog :: (Member (Reader Int) rs, Member (Writer String) rs)
        => Freer rs Int
prog = Free (inj $ Tell "hi") Pure >>= \() ->
         Free (inj Ask) Pure >>= \(x :: Int) ->
           Free (inj $ Tell (show x)) Pure >>= \() ->
             Pure 5

prog' :: (Member (Reader Int) rs, Member (Writer String) rs) => Freer rs Int
prog' = Free (inj $ Tell "hi") (Pure >=> \() ->
          Free (inj Ask) (Pure >=> \(x :: Int) ->
            Free (inj $ Tell (show x)) (Pure >=> \() ->
              Pure 5)))

prog'' :: (Member (Reader Int) rs, Member (Writer String) rs) => Freer rs Int
prog'' = Free (inj $ Tell "hi") (\() ->
          Free (inj Ask) (\(x :: Int) ->
            Free (inj $ Tell (show x)) (\() ->
              Pure 5)))

example' :: forall env. Freer '[Reader env, Writer String] Int
example' = do
  tell "hi"
  x :: env <- ask
  return 5

exampleR :: forall rs . (Member (Reader Int) rs)
        => Freer rs Int
exampleR = do
  x :: Int <- ask
  return 5

exampleW :: forall rs. (Member (Writer String) rs)
        => Freer rs Int
exampleW = do
  tell "hi"
  return 5

exampleIO :: forall rs. (SetMember Lift (Lift IO) rs) => Freer rs ()
exampleIO = do
  lift $ print "hi"
  return ()

runEx :: (Int, String)
runEx = (run . runWriter . runReader (5 :: Int)) example