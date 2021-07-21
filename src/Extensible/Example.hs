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
{-# LANGUAGE OverloadedLabels #-}
module Extensible.Example where

import Statistics.Distribution
import GHC.OverloadedLabels
import Extensible.Freer
import Extensible.Reader
import Extensible.State
import Extensible.Writer
import Extensible.Model
import Extensible.Dist
import Extensible.IO
import Extensible.Sampler
import Control.Monad
import Control.Lens hiding ((:>))
import Unsafe.Coerce
import Data.Kind (Constraint)
import GHC.TypeLits
import Data.Typeable
import Extensible.OpenProduct
import Util

{- Probabilistic programs -}

-- | Linear regression
type LinRegrEnv =
    '[  "y" ':> Double,
        "m" ':>  Double,
        "c" ':>  Double,
        "σ" ':>  Double
     ]

linearRegression :: forall s rs .
  (HasVar s "y" Double, HasVar s "m" Double, Lookup (AsList s) "c" [Double], HasVar s "σ" Double) =>
  Double -> Model s rs (Double, Double)
linearRegression x = do
  m1 <- normal' 0 4 #m
  c <- normal' 0 2 #c
  σ <- uniform' 1 3 #σ
  y <- normal' (m1 * x + c) σ #y
  return (x, y)

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
  m     <- normal' 0 5 #m
  b     <- normal' 0 1 #b
  sigma <- gamma 1 1
  y     <- normal (m * x + b) sigma
  l     <- bernoulli' (sigmoid y) #label
  return (x, l)

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

-- | Neural network formulation for linear regression
forwardNNLin :: NN -> Double -> Double
forwardNNLin (NN bs ws _) x =
  (ws `dot` map (x -) bs) / 20

likelihoodNNLin :: HasVar s "yObs" Double => NN -> Double -> Model s es Double
likelihoodNNLin nn x = do
  let ySigma = sigm nn
      yMean  = forwardNNLin nn x
  normal' yMean ySigma #yObs

priorNN :: (HasVar s "weight" Double, HasVar s "bias" Double, HasVar s "sigma" Double)
  => Int -> Model s es NN
priorNN n_nodes = do
  bias   <- replicateM n_nodes (uniform' 0 10 #bias)
  weight <- replicateM n_nodes (uniform' (-10) 10 #weight)
  sigma  <- uniform' 0.5 1.5 #sigma
  return $ NN bias weight sigma

nnLinModel :: (HasVar s "weight" Double, HasVar s "bias" Double, HasVar s "sigma" Double, HasVar s "yObs" Double) => Int -> Double -> Model s es (Double, Double)
nnLinModel n x = do
  nn <- priorNN n
  y  <- likelihoodNNLin nn x
  return (x, y)

-- | Alternative neural network formulation using activation
forwardNNStep :: NN -> Double -> Double
forwardNNStep (NN bs ws _) x =
  ws `dot` map (activation . (x -)) bs
  where activation x = if x < 0 then 0 else 1

likelihoodNNStep :: HasVar s "yObs" Double => NN -> Double -> Model s es Double
likelihoodNNStep nn x = do
  let ySigma = sigm nn
      yMean  = forwardNNStep nn x
  normal' yMean ySigma #yObs

nnStepModel :: (HasVar s "weight" Double, HasVar s "bias" Double, HasVar s "sigma" Double, HasVar s "yObs" Double) => Int -> Double -> Model s es (Double, Double)
nnStepModel n x = do
  nn <- priorNN n
  y <- likelihoodNNStep nn x
  return (x, y)

-- | Another neural network formulation

type NNLogEnv =
    '[  "yObs"     ':> Bool,
        "weight"   ':> Double
     ]

nnLogModel :: (HasVar s "weight" Double, HasVar s "yObs" Bool) => Int -> (Double, Double) -> Model s es ((Double, Double), Bool)
nnLogModel n_nodes (x, y)  = do
  let xs = [x, y]
  weight1 <- replicateM (length xs) (replicateM n_nodes (normal' 0 1 #weight))
--  Model $ prinT $ "xs is " ++ show [xs]
--  Model $ prinT $ "weight1 is " ++ show weight1
  let output1 = map2 tanh (dotProd [xs] weight1)
--  Model $ prinT $ "output1 is " ++ show output1
  weight2 <- replicateM n_nodes (replicateM n_nodes (normal' 0 1 #weight))
--  Model $ prinT $ "weight2 is " ++ show weight2
  let output2 = map2 tanh (dotProd output1 weight2)
--  Model $ prinT $ "output2 is " ++ show output2
  weight3 <- replicateM n_nodes (replicateM 1 (normal' 0 1 #weight))
--  Model $ prinT $ "weight3 is " ++ show weight3
  let output3 =  sigmoid . head . head $ dotProd output2 weight3
--  Model $ prinT $ "output3 is " ++ show output3
  label <- bernoulli' output3 #yObs
  return ((x, y), label)

-- | Sine model

sineModel :: forall s rs .
  (HasVar s "y" Double, HasVar s "m" Double, HasVar s "c" Double, HasVar s "σ" Double) =>
  Double -> Model s rs (Double, Double)
sineModel x = do
  m <- normal' 0 4 #m
  c <- normal' 0 2 #c
  σ <- uniform' 1 3 #σ
  Model $ prinT $ "mean is " ++ (show $ sin $ m * x + c)
  y <- normal' (sin $ m * x + c) σ #y
  return (x, y)

-- | Hidden Markov Model
type HMMEnv =
  '[ "y"       ':> Int,
     "trans_p" ':> Double,
     "obs_p"   ':> Double
   ]

transitionModel ::  Double -> Int -> Model s es Int
transitionModel transition_p x_prev = do
  dX <- boolToInt <$> bernoulli transition_p
  return (dX + x_prev)

observationModel :: (HasVar s "y" Int)
  => Double -> Int -> Model s es Int
observationModel observation_p x = do
  binomial' x observation_p #y

hmm :: (HasVar s "y" Int)
  => Double -> Double -> Int -> Model s es (Int, Int)
hmm transition_p observation_p x_prev = do
  x_n <- transitionModel  transition_p x_prev
  y_n <- observationModel observation_p x_n
  return (x_n, y_n)

hmm' :: HasVar s "y" Int => Double -> Double -> Int -> Model s es Int
hmm' transition_p observation_p =
  observationModel observation_p <=< transitionModel transition_p

hmmNSteps :: (HasVar s "y" Int, HasVar s "obs_p" Double, HasVar s "trans_p" Double)
  => Int -> (Int -> Model s es ([Int], [Int]))
hmmNSteps n x = do
  trans_p <- uniform' 0 1 #trans_p
  obs_p   <- uniform' 0 1 #obs_p
  (xs, ys) <- foldl (>=>) return
                (replicate n (\(xs, ys) -> do
                  (x_n, y_n) <- hmm trans_p obs_p (head xs)
                  return (x_n:xs, y_n:ys))) ([x], [])
  return (reverse xs, reverse ys)

-- | Hidden Markov Model using State effect
transitionModelSt ::  Double -> Int -> Model s es Int
transitionModelSt transition_p x_prev = do
  dX <- boolToInt <$> bernoulli transition_p
  return (dX + x_prev)

observationModelSt :: (HasVar s "y" Int, Member (State [Int]) es)
  => Double -> Int -> Model s es Int
observationModelSt observation_p x = do
  y_n <- binomial' x observation_p #y
  modifyM (y_n:)
  return y_n

hmmSt :: (HasVar s "y" Int, Member (State [Int]) es)
  => Double -> Double -> [Int] -> Model s es [Int]
hmmSt transition_p observation_p xs = do
  x_n <- transitionModelSt transition_p (head xs)
  y_n <- observationModelSt observation_p x_n
  return (x_n:xs)

hmmNStepsSt :: (HasVar s "y" Int, Member (State [Int]) es)
  => Double -> Double -> Int -> (Int -> Model s es [Int])
hmmNStepsSt transition_p observation_p n x =
  foldl (>=>) return
    (replicate n (hmmSt transition_p observation_p)) [x]

-- | Hidden Markov Model - SIR

data FixedParams = FixedParams {
    numPop :: Int,
    timeSlices :: Int
}
data Params = Params {
    rhoP   :: Double, -- ^ Rate of detection
    betaP  :: Double, -- ^ Mean contact rate between susceptible and infected people
    gammaP :: Double -- ^ Mean recovery rate
}
data LatentState = LatentState {
    sus :: Int, -- ^ Number of people susceptible to infection
    inf :: Int, -- ^ Number of people currently infected
    recov :: Int -- ^ Number of people recovered from infection
} deriving Show

type SIREnv =
  '[ "infobs" ':> Int,
     "ρ" ':> Double,
     "β" ':> Double,
     "γ" ':> Double
   ]

type InfectionCount = Int

observeSIR :: HasVar s "infobs" Int => Params -> LatentState -> Model s es Int
observeSIR (Params rho _ _) (LatentState _ inf _) = do
  -- Model $ prinT $ "inf is " ++ show inf
  poisson' (rho * fromIntegral inf) #infobs

transitionSIR :: FixedParams -> Params -> LatentState -> Model s es LatentState
transitionSIR (FixedParams numPop timeSlices) (Params rho beta gamma) (LatentState sus inf recov)  = do
  let dt   = 1 / fromIntegral timeSlices
      si_p = 1 - exp ((-beta * dt * fromIntegral inf) / fromIntegral numPop)
  dN_SI <- binomial sus si_p
  let ir_p = 1 - exp (-gamma * dt)
  dN_IR <- binomial inf ir_p
  let sus'   = sus - dN_SI
      inf'   = inf + dN_SI - dN_IR
      recov' = recov + dN_IR
  return (LatentState sus' inf' recov')

hmmSIR :: HasVar s "infobs" Int => FixedParams -> Params -> LatentState -> Model s es (LatentState, Int)
hmmSIR fixedParams params latentState = do
  latentState'   <- transitionSIR fixedParams params latentState
  infectionCount <- observeSIR params latentState
  return (latentState', infectionCount)

paramsPrior :: (HasVar s "ρ" Double, HasVar s "β" Double, HasVar s "γ" Double) =>
  Model s es Params
paramsPrior = do
  pRho   <- beta' 2 7 #ρ
  pBeta  <- gamma' 2 1 #β
  pGamma <- gamma' 1 (1/8) #γ
  Model $ prinT $ "Using params: " ++ show (pRho, pBeta, pGamma)
  return (Params pRho pBeta pGamma)

hmmSIRNsteps :: (HasVar s "infobs" Int, HasVar s "ρ" Double, HasVar s "β" Double, HasVar s "γ" Double) =>
  FixedParams -> Int -> LatentState -> Model s es ([LatentState], [Int])
hmmSIRNsteps fixedParams n latentState  = do
  params <- paramsPrior
  (xs, ys) <- foldl (>=>) return
          (replicate n (\(xs, ys) -> do
                  (x_n, y_n) <- hmmSIR fixedParams params (head xs)
                  return (x_n:xs, y_n:ys))) ([latentState], [])
  return (reverse xs, reverse ys)

-- | Random test model
type DirEnv =
  '[ "xs" ':> [Double]
   ]

halfNorm :: HasVar s "xs" [Double] => Int -> Model s es [Double]
halfNorm n = do
  -- s <- halfNormal 1
  -- x <- cauchy 0 1
  xs <- dirichlet' [0.5, 0.5] #xs
  -- xs <- categorical [("hi", 1), ("bye", 0.5)]
  -- topics   <- replicateM 2 $ dirichlet (replicate 4 1)
  return xs

-- | Topic model

type TopicEnv =
  '[ "topic_ps" ':> [Double],
     "word_ps" ':> [Double],
     "word" ':> String
   ]

-- Probability of each word in a topic
wordDist :: HasVar s "word" String => [String] -> [Double] -> Model s es String
wordDist vocab ps = categorical' (zip vocab ps) #word

-- Probability of each topic in a document
topicDist :: HasVar s "topic_ps" [Double] => Int -> Model s es [Double]
topicDist n_topics = dirichlet' (replicate n_topics 1) #topic_ps

-- Learns topic model for a single document
documentDist :: (HasVar s "word_ps" [Double], HasVar s "topic_ps" [Double], HasVar s "word" String) => [String] -> Int -> Int -> Model s es [String]
documentDist vocab n_topics n_words = do
  -- Generate distribution over words for each topic
  topic_word_ps <- replicateM n_topics $ dirichlet' (replicate (length vocab) 1) #word_ps
  -- Distribution over topics for a given document
  topic_ps <- topicDist n_topics
  replicateM n_words (do  z <- discrete topic_ps
                          let word_ps = topic_word_ps !! z
                          wordDist vocab word_ps)

-- Learns topic models for multiple documents
topicModel :: (HasVar s "word_ps" [Double], HasVar s "topic_ps" [Double], HasVar s "word" String) =>
  [String] ->
  Int      ->
  [Int]    -> -- Number of words per document
  Model s es [[String]]
topicModel vocab n_topics doc_words = do
  mapM (documentDist vocab n_topics) doc_words

-- | Hierarchical Linear Regression

type HLREnv =
  '[ "mu_a" ':> Double, "mu_b" ':> Double, "sigma_a" ':> Double, "sigma_b" ':> Double,
     "a" ':> Double, "b" ':> Double, "log_radon" ':> Double]

hlrPrior :: (HasVar s "mu_a" Double, HasVar s "mu_b" Double, HasVar s "sigma_a" Double, HasVar s "sigma_b" Double) => Model s es (Double, Double, Double, Double)
hlrPrior = do
  mu_a    <- normal' 0 10 #mu_a
  sigma_a <- halfNormal' 5 #sigma_a
  mu_b    <- normal' 0 10 #mu_b
  sigma_b <- halfNormal' 5 #sigma_b
  return (mu_a, sigma_a, mu_b, sigma_b)

-- n counties = 85, len(floor_x) = 919, len(county_idx) = 919
hierarchicalLinRegr :: (HasVar s "mu_a" Double, HasVar s "mu_b" Double, HasVar s "sigma_a" Double, HasVar s "sigma_b" Double, HasVar s "a" Double, HasVar s "b" Double, HasVar s "log_radon" Double)
  => Int -> [Int] -> [Int] -> () -> Model s es [Double]
hierarchicalLinRegr n_counties floor_x county_idx _ = do
  (mu_a, sigma_a, mu_b, sigma_b) <- hlrPrior
  -- Intercept for each county
  a <- replicateM n_counties (normal' mu_a sigma_a #a)  -- length = 85
  -- Gradient for each county
  b <- replicateM n_counties (normal' mu_b sigma_b #b)  -- length = 85
  -- Model error
  eps <- halfCauchy 5
  let -- Get county intercept for each datapoint
      a_county_idx = map (a !!) county_idx
      -- Get county gradient for each datapoint
      b_county_idx = map (b !!) county_idx
      floor_values = map fromIntegral floor_x
      -- Get radon estimate for each data point
      radon_est = zipWith (+) a_county_idx (zipWith (*) b_county_idx floor_values)
  -- Sample radon amount for each data point
  radon_like <- mapM (\rad_est -> normal' rad_est eps #log_radon) radon_est
  let f = ""
  return radon_like

-- | Gaussian Mixture Model
gmm :: HasVar s "y" Double => Int -> Model s es ()
gmm k = do
  cluster_ps <- dirichlet (replicate k 1)
  mus        <- replicateM k (normal 0 15)
  -- categorical cluster_ps

  undefined