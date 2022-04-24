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
module Old.Example where

import Statistics.Distribution
import GHC.OverloadedLabels
import Freer
import Effects.Reader
import Effects.State
import Effects.Writer
import Model
import Effects.Dist
import Effects.Lift
import Sampler
import OpenSum (OpenSum)
import qualified OpenSum as OpenSum
import Control.Monad
import Data.List as List
import Unsafe.Coerce
import Data.Maybe
import Data.Kind (Constraint)
import GHC.TypeLits
import Data.Typeable
import ModelEnv
import Util
import Data.Vector.Fusion.Bundle (findIndex)
import GHC.Show (Show)
import qualified Data.Map as Map
{- Probabilistic programs -}

-- | Linear regression
type LinRegrEnv =
    '[  "y" ':= Double,
        "m" ':=  Double,
        "c" ':=  Double,
        "σ" ':=  Double
     ]

linearRegression :: forall env rs .
  Observables env '["y", "m", "c", "σ"] Double =>
  Double -> Model env rs (Double, Double)
linearRegression x = do
  m <- normal' 0 3 #m
  c <- normal' 0 5 #c
  σ <- uniform' 1 3 #σ
  -- printM $ "(m * x + c) is " ++ show (m * x + c)
  y <- normal' (m * x + c) σ #y
  return (x, y)

-- | Logistic regression
type LogRegrEnv =
    '[  "label" ':= Bool,
        "m"     ':= Double,
        "b"     ':= Double
     ]

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp((-1) * x))

logisticRegression :: forall rs env.
 (Observable env "label" Bool, Observables env '["m", "b"] Double) =>
 Double -> Model env rs (Double, Bool)
logisticRegression x = do
  m     <- normal' 0 5 #m
  b     <- normal' 0 1 #b
  sigma <- gamma 1 1
  y     <- normal (m * x + b) sigma
  l     <- bernoulli' (sigmoid y) #label
  return (x, l)

-- | Bayesian network
type NNEnv =
    '[  "yObs"     ':= Double,
        "weight"   ':= Double,
        "bias"     ':= Double,
        "sigma"    ':= Double
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

likelihoodNNLin :: Observable env "yObs" Double => NN -> Double -> Model env es Double
likelihoodNNLin nn x = do
  let ySigma = sigm nn
      yMean  = forwardNNLin nn x
  normal' yMean ySigma #yObs

priorNN :: (Observables env '["weight", "bias", "sigma"] Double)
  => Int -> Model env es NN
priorNN n_nodes = do
  bias   <- replicateM n_nodes (uniform' 0 10 #bias)
  weight <- replicateM n_nodes (uniform' (-10) 10 #weight)
  sigma  <- uniform' 0.5 1.5 #sigma
  return $ NN bias weight sigma

nnLinModel :: (Observables env '["weight", "bias", "sigma", "yObs"] Double)
  => Int -> Double -> Model env es (Double, Double)
nnLinModel n x = do
  nn <- priorNN n
  y  <- likelihoodNNLin nn x
  return (x, y)

-- | Alternative neural network formulation using activation
forwardNNStep :: NN -> Double -> Double
forwardNNStep (NN bs ws _) x =
  ws `dot` map (activation . (x -)) bs
  where activation x = if x < 0 then 0 else 1

likelihoodNNStep :: Observable env "yObs" Double
 => NN -> Double -> Model env es Double
likelihoodNNStep nn x = do
  let ySigma = sigm nn
      yMean  = forwardNNStep nn x
  normal' yMean ySigma #yObs

nnStepModel :: (Observables env '["weight", "bias", "sigma", "yObs"] Double)
 => Int -> Double -> Model env es (Double, Double)
nnStepModel n x = do
  nn <- priorNN n
  y <- likelihoodNNStep nn x
  return (x, y)

-- | Another neural network formulation

type NNLogEnv =
    '[  "yObs"     ':= Bool,
        "weight"   ':= Double
     ]

nnLogModel :: (Observable env "weight" Double, Observable env "yObs" Bool)
  => Int -> (Double, Double) -> Model env es ((Double, Double), Bool)
nnLogModel n_nodes (x, y)  = do
  let xs = [x, y]
  weightsA <- replicateM2 (length xs) n_nodes (normal' 0 1 #weight)
  let outputA = map2 tanh (dotProd [xs] weightsA)
  weightsB <- replicateM2 n_nodes n_nodes (normal' 0 1 #weight)
  let outputB = map2 tanh (dotProd outputA weightsB)
  weightsC <- replicateM2 n_nodes 1 (normal' 0 1 #weight)
  let outputC =  sigmoid . head . head $ dotProd outputB weightsC
  label <- bernoulli' outputC #yObs
  return ((x, y), label)

-- | Sine model

sineModel :: forall env rs .
  Observables env '["y", "m", "c", "σ"] Double =>
  Double -> Model env rs (Double, Double)
sineModel x = do
  m <- normal' 0 4 #m
  c <- normal' 0 2 #c
  σ <- uniform' 1 3 #σ
  -- Model $ prinT $ "mean is " ++ (show $ sin $ m * x + c)
  y <- normal' (sin $ m * x + c) σ #y
  return (x, y)

-- | Hidden Markov Model
type HMMEnv =
  '[ "y"       ':= Int,
     "trans_p" ':= Double,
     "obs_p"   ':= Double
   ]

transitionModel ::  Double -> Int -> Model env es Int
transitionModel transition_p x_prev = do
  dX <- boolToInt <$> bernoulli transition_p
  return (dX + x_prev)

observationModel :: (Observable env "y" Int)
  => Double -> Int -> Model env es Int
observationModel observation_p x = do
  binomial' x observation_p #y

hmm :: (Observable env "y" Int)
  => Double -> Double -> Int -> Model env es (Int, Int)
hmm transition_p observation_p x_prev = do
  x_n <- transitionModel  transition_p x_prev
  y_n <- observationModel observation_p x_n
  return (x_n, y_n)

hmm' :: Observable env "y" Int => Double -> Double -> Int -> Model env es Int
hmm' transition_p observation_p =
  observationModel observation_p <=< transitionModel transition_p

hmmNSteps :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double)
  => Int -> (Int -> Model env es ([Int], [Int]))
hmmNSteps n x = do
  trans_p <- uniform' 0 1 #trans_p
  obs_p   <- uniform' 0 1 #obs_p
  (xs, ys) <- foldl (>=>) return
                (replicate n (\(xs, ys) -> do
                  (x_n, y_n) <- hmm trans_p obs_p (head xs)
                  return (x_n:xs, y_n:ys))) ([x], [])
  return (reverse xs, reverse ys)

-- | Hidden Markov Model using State effect
transitionModelSt ::  Double -> Int -> Model env es Int
transitionModelSt transition_p x_prev = do
  dX <- boolToInt <$> bernoulli transition_p
  return (dX + x_prev)

observationModelSt :: (Observable env "y" Int, Member (State [Int]) es)
  => Double -> Int -> Model env es Int
observationModelSt observation_p x = do
  y_n <- binomial' x observation_p #y
  modifyM (y_n:)
  return y_n

hmmSt :: (Observable env "y" Int, Member (State [Int]) es)
  => Double -> Double -> [Int] -> Model env es [Int]
hmmSt transition_p observation_p xs = do
  x_n <- transitionModelSt transition_p (head xs)
  y_n <- observationModelSt observation_p x_n
  return (x_n:xs)

hmmNStepsSt :: (Observable env "y" Int, Member (State [Int]) es)
  => Double -> Double -> Int -> (Int -> Model env es [Int])
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
  '[ "infobs" ':= Int,
     "ρ" ':= Double,
     "β" ':= Double,
     "γ" ':= Double
   ]

type InfectionCount = Int

observeSIR :: Observable env "infobs" Int
  => Params -> LatentState -> Model env es Int
observeSIR (Params rho _ _) (LatentState _ inf _) = do
  poisson' (rho * fromIntegral inf) #infobs

transitionSIR :: FixedParams -> Params -> LatentState -> Model env es LatentState
transitionSIR (FixedParams numPop timeSlices) (Params rho beta gamma) (LatentState sus inf recov)  = do
  let dt   = 1 / fromIntegral timeSlices
      si_p = 1 - exp ((-beta * dt * fromIntegral inf) / fromIntegral numPop)
  -- printM $ "si_p is " ++ show si_p
  dN_SI <- binomial sus si_p
  let ir_p = 1 - exp (-gamma * dt)

  dN_IR <- binomial inf ir_p
  let sus'   = sus - dN_SI
      inf'   = inf + dN_SI - dN_IR
      recov' = recov + dN_IR
  -- printM $ "(s,i,r) = " ++ show (LatentState sus' inf' recov') ++ "\n(dN_SI, dN_IR) = " ++ show (dN_SI, dN_IR)
  return (LatentState sus' inf' recov')

hmmSIR :: Observable env "infobs" Int
  => FixedParams -> Params -> LatentState -> Model env es (LatentState, Int)
hmmSIR fixedParams params latentState = do
  latentState'   <- transitionSIR fixedParams params latentState
  infectionCount <- observeSIR params latentState
  return (latentState', infectionCount)

paramsPrior :: Observables env '["ρ", "β", "γ"] Double
  => Model env es Params
paramsPrior = do
  pRho   <- beta' 2 7 #ρ
  pBeta  <- gamma' 2 1 #β
  pGamma <- gamma' 1 (1/8) #γ
  return (Params pRho pBeta pGamma)

hmmSIRNsteps :: (Observable env "infobs" Int, Observables env '["ρ", "β", "γ"] Double)
  => FixedParams -> Int -> LatentState -> Model env es ([LatentState], [Int])
hmmSIRNsteps fixedParams n latentState  = do
  params <- paramsPrior
  (xs, ys) <- foldl (>=>) return
          (replicate n (\(xs, ys) -> do
                  (x_n, y_n) <- hmmSIR fixedParams params (head xs)
                  return (x_n:xs, y_n:ys))) ([latentState], [])
  return (reverse xs, reverse ys)

-- | Random test model
type DirEnv =
  '[ "xs" ':= [Double]
   ]

halfNorm :: Observable env "xs" [Double]
  => Int -> Model env es [Double]
halfNorm n = do
  -- env <- halfNormal 1
  -- x <- cauchy 0 1
  xs <- dirichlet' [0.5, 0.5] #xs
  -- xs <- categorical [("hi", 1), ("bye", 0.5)]
  -- topics   <- replicateM 2 $ dirichlet (replicate 4 1)
  return xs

-- | Topic model

type TopicEnv =
  '[ "θ" ':= [Double],
     "φ" ':= [Double],
     "w" ':= String
   ]

-- Assignment of word probabilities to a topic
wordDist :: Observable env "w" String =>
  [String] -> [Double] -> Model env es String
wordDist vocab ps =
  categorical' (zip vocab ps) #w

-- Probability of each word in a topic
topicWordPrior :: Observable env "φ" [Double]
  => [String] -> Model env es [Double]
topicWordPrior vocab
  = dirichlet' (replicate (length vocab) 1) #φ

-- Probability of each topic in a document
docTopicPrior :: Observable env "θ" [Double]
  => Int -> Model env es [Double]
docTopicPrior n_topics = dirichlet' (replicate n_topics 1) #θ

-- Learns topic model for a single document
documentDist :: (Observables env '["φ", "θ"] [Double],
                 Observable env "w" String)
  => [String] -> Int -> Int -> Model env es [String]
documentDist vocab n_topics n_words = do
  -- Generate distribution over words for each topic
  topic_word_ps <- replicateM n_topics $ topicWordPrior vocab
  -- Distribution over topics for a given document
  doc_topic_ps  <- docTopicPrior n_topics
  replicateM n_words (do  z <- discrete doc_topic_ps
                          let word_ps = topic_word_ps !! z
                          wordDist vocab word_ps)

-- Learns topic models for multiple documents
topicModel :: (Observables env '["φ", "θ"] [Double],
               Observable env "w" String)
  => [String] ->
     Int      ->
     [Int]    -> -- Number of words per document
     Model env es [[String]]
topicModel vocab n_topics doc_words = do
  mapM (documentDist vocab n_topics) doc_words

-- | Hierarchical Linear Regression

type HLREnv =
  '[ "mu_a" ':= Double, "mu_b" ':= Double, "sigma_a" ':= Double, "sigma_b" ':= Double,
     "a" ':= Double, "b" ':= Double, "log_radon" ':= Double]

hlrPrior :: Observables env '["mu_a", "mu_b", "sigma_a", "sigma_b"] Double
  => Model env es (Double, Double, Double, Double)
hlrPrior = do
  mu_a    <- normal' 0 100 #mu_a
  sigma_a <- halfNormal' 5 #sigma_a
  mu_b    <- normal' 0 100 #mu_b
  sigma_b <- halfNormal' 5 #sigma_b
  return (mu_a, sigma_a, mu_b, sigma_b)

-- n counties = 85, len(floor_x) = 919, len(county_idx) = 919
hierarchicalLinRegr :: Observables env '["mu_a", "mu_b", "sigma_a", "sigma_b", "a", "b", "log_radon"] Double
  => Int -> [Int] -> [Int] -> () -> Model env es [Double]
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

type GMMEnv = '[
    "mu" ':= Double,
    "mu_k" ':= Double,
    "x"  ':= Double,
    "y"  ':= Double
  ]

gmm :: Observables env '["mu", "mu_k", "x", "y"] Double
  => Int -- num clusters
  -> Int -- num data points
  -> Model env es [((Double, Double), Int)]
gmm k n = do
  cluster_ps <- dirichlet (replicate k 1)
  mus        <- replicateM k (normal' 0 5 #mu)
  replicateM n (do mu_k <- categorical' (zip mus cluster_ps) #mu_k
                   let i = fromJust $ List.findIndex (==mu_k) mus
                   x    <- normal' mu_k 1 #x
                   y    <- normal' mu_k 1 #y
                   return ((x, y), i))

-- | Hierarchical School Model
type SchEnv = '[
    "mu"    ':= Double,
    "theta" ':= [Double],
    "y"     ':= Double
  ]

schoolModel :: (Observables env '["mu", "y"] Double, Observable env "theta" [Double])
  => Int -> [Double] -> Model env es [Double]
schoolModel n_schools σs = do
  μ   <- normal' 0 10 #mu
  τ   <- halfNormal 10
  ηs  <- replicateM n_schools (normal 0 1)
  θs  <- deterministic' (map ((μ +) . (τ *)) ηs) #theta
  ys  <- mapM (\(θ, σ) -> normal' θ σ #y) (zip θs σs)
  let h = ""
  return θs

