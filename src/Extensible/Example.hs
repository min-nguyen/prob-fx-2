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
import Util

{- Probabilistic programs -}

-- | Hidden Markov Model
-- transitionModel ::  Double -> Int -> Model s es Int
-- transitionModel transition_p x_prev = do
--   dX <- boolToInt <$> bernoulli transition_p
--   let x = x_prev + dX
--   return (dX + x)

-- observationModel :: (HasVar s "y" Int)
--   => Double -> Int -> Model s es Int
-- observationModel observation_p x = do
--   binomial' x observation_p y

-- hmm :: (HasVar s "y" Int)
--   => Double -> Double -> Int -> Model s es Int
-- hmm transition_p observation_p x_prev = do
--   x_n <- transitionModel transition_p x_prev
--   y_n <- observationModel observation_p x_n
--   return x_n

-- hmm' :: HasVar s "y" Int => Double -> Double -> Int -> Model s es Int
-- hmm' transition_p observation_p =
--   observationModel observation_p <=< transitionModel transition_p

-- hmmNSteps :: (HasVar s "y" Int)
--   => Double -> Double -> Int -> (Int -> Model s es Int)
-- hmmNSteps transition_p observation_p n =
--   foldl (>=>) return (replicate n (hmm transition_p observation_p))

-- | Bayesian network
type NNEnv =
    '[  "yObs"     ':> Double,
        "weight"   ':> Double,
        "bias"     ':> Double,
        "sigma"    ':> Double
     ]

data NN = NN { biases  :: [Double],
               weights :: [Double],
               sigm    :: Double } deriving Show

dot :: [Double] -> [Double] -> Double
dot [] _ = 0
dot _ [] = 0
dot (x:xs) (y:ys) = x * y + dot xs ys

forwardNN :: NN -> Double -> Double
forwardNN (NN bs ws _) x =
  (ws `dot` (map (x -) bs)) / 20
  where activation x = if x < 0 then 0 else 1

{- Next step: plot lw with nn's using color map-}
likelihoodNN :: HasVar s "yObs" Double => NN -> Double -> Model s es Double
likelihoodNN nn x = do
  let ySigma = sigm nn
      yMean  = forwardNN nn x
  normal' yMean ySigma yObs

priorNN :: (HasVar s "weight" Double, HasVar s "bias" Double, HasVar s "sigma" Double)
  => Int -> Model s es NN
priorNN n_nodes = do
  bias   <- replicateM n_nodes (uniform' 0 10 bias)
  weight <- replicateM n_nodes (uniform' (-10) 10 weight)
  sigma  <- uniform' 0.5 1.5 sigma
  return $ NN bias weight sigma

nnModel :: (HasVar s "weight" Double, HasVar s "bias" Double, HasVar s "sigma" Double, HasVar s "yObs" Double) => Int -> Double -> Model s es (Double, Double)
nnModel n x = do
  -- Sample neural network parameters
  nn <- priorNN n
  -- Compute output y
  y <- likelihoodNN nn x
  return (x, y)

-- points :: [(Double, Double, Double)]
-- points = [ (x, y, exp . log $ likelihood nn x y) | x <- [0 .. 10], y <- [-10 .. 10]]

-- | Logistic regression
type LogRegrEnv =
    '[  "label" ':> Bool,
        "m"     ':> Double,
        "b"     ':> Double
     ]

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp((-1) * x))

logisticRegression :: forall rs s.
 (HasVar s "label" Bool, HasVar s "m" Double, HasVar s "b" Double) =>
 Double -> Model s rs (Double, Bool)
logisticRegression x = do
  m     <- normal' 0 1 m
  b     <- normal' 0 1 b
  sigma <- gamma 1 1
  y     <- normal (m * x + b) sigma
  l     <- bernoulli' (sigmoid y) label
  return (x, l)

-- | Linear regression
type LinRegrEnv =
    '[  "y"   ':>  Double,
        "m"   ':>  Double,
        "c"   ':>  Double,
        "σ"   ':>  Double
     ]

linearRegression :: forall s rs .
  (HasVar s "y" Double, HasVar s "m" Double, HasVar s "c" Double, HasVar s "σ" Double) =>
  Double -> Model s rs (Double, Double)
linearRegression x = do
  m1 <- normal' 0 2 m
  m2 <- normal' 0 2 m
  c <- normal' 0 2 c
  σ <- uniform' 1 3 σ
  y <- normal' (m1 * x + c) σ y
  return (x, y)

-- --
-- arbitraryModel :: forall es s. HasVar s "y" Double =>
--   Double -> Double -> Double -> Model s es Double
-- arbitraryModel μ σ x = do
--   normal'  (μ + x) σ y
--   replicateM 4 $ normal (μ + x) σ
--   normal'  (μ + x) σ y

-- ifModel :: forall rs s. Double -> Model s rs Double
-- ifModel p = do
--   x1 <- bernoulli p
--   x2 <- if x1 then normal 0 1 else pure 0
--   x3 <- bernoulli p
--   return 0

-- ifModel' :: Double -> Model s rs Double
-- ifModel' p = Model $
--   Free (inj $ BernoulliDist p Nothing) Pure >>= \x1 ->
--     if x1 then Free (inj $ NormalDist 0 1 Nothing) Pure else return 0

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