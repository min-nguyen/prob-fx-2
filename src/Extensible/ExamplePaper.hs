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
module Extensible.ExamplePaper where

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
import Extensible.OpenSum (OpenSum)
import qualified Extensible.OpenSum as OpenSum
import Control.Monad
import Data.List as List
import Unsafe.Coerce
import Data.Maybe
import Data.Kind (Constraint)
import GHC.TypeLits
import Data.Typeable
import Extensible.OpenProduct
import Util
import Data.Vector.Fusion.Bundle (findIndex)
import GHC.Show (Show)
import qualified Data.Map as Map

{- Linear regression -}
type LinRegrEnv =
    '[  "y" ':= Double,
        "m" ':=  Double,
        "c" ':=  Double,
        "σ" ':=  Double
     ]

linearRegression :: forall env rs .
  Observables env '["y", "m", "c", "σ"] Double =>
  [Double] -> Model env rs [Double]
linearRegression xs = do
  m <- normal' 0 3 #m
  c <- normal' 0 5 #c
  σ <- uniform' 1 3 #σ
  ys <- foldM (\ys x -> do
                    y <- normal' (m * x + c) σ #y
                    return (y:ys)) [] xs
  return ys

{- HMM -}
type HMMEnv =
  '[ "y"       ':= Int,
     "trans_p" ':= Double,
     "obs_p"   ':= Double
   ]

transitionModel ::  Double -> Int -> Model env ts Int
transitionModel transition_p x_prev = do
  dX <- boolToInt <$> bernoulli transition_p
  return (dX + x_prev)

observationModel :: (Observable env "y" Int)
  => Double -> Int -> Model env ts Int
observationModel observation_p x = do
  binomial' x observation_p #y

hmm :: (Observable env "y" Int)
  => Double -> Double -> Int -> Model env ts Int
hmm transition_p observation_p x_prev = do
  x_n <- transitionModel  transition_p x_prev
  y_n <- observationModel observation_p x_n
  return x_n

hmmNSteps :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double)
  => Int -> (Int -> Model env ts Int)
hmmNSteps n x = do
  trans_p <- uniform' 0 1 #trans_p
  obs_p   <- uniform' 0 1 #obs_p
  foldr (<=<) return  (replicate n (hmm trans_p obs_p)) x

{- HMM Loop-}
hmmForM :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double) =>
  Int -> Int -> Model env ts Int
hmmForM n x = do
  trans_p <- uniform' 0 1 #trans_p
  obs_p   <- uniform' 0 1 #obs_p
  let hmmLoop :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double) => Int -> Int -> Model env ts Int
      hmmLoop 0 x_prev = return x_prev
      hmmLoop i x_prev = do
        dX <- boolToInt <$> bernoulli trans_p
        let x = x_prev + dX
        binomial' x obs_p #y
        hmmLoop (i - 1) x
  hmmLoop n x

{- Topic Model -}
type TopicEnv =
  '[ "θ" ':= [Double],
     "φ" ':= [Double],
     "w" ':= String
   ]

-- Assignment of word probabilities to a topic
wordDist :: Observable env "w" String =>
  [String] -> [Double] -> Model env ts String
wordDist vocab ps =
  categorical' (zip vocab ps) #w

-- Probability of each word in a topic
topicWordPrior :: Observable env "φ" [Double]
  => [String] -> Model env ts [Double]
topicWordPrior vocab
  = dirichlet' (replicate (length vocab) 1) #φ

-- Probability of each topic in a document
docTopicPrior :: Observable env "θ" [Double]
  => Int -> Model env ts [Double]
docTopicPrior n_topics = dirichlet' (replicate n_topics 1) #θ

-- Learns topic model for a single document
documentDist :: (Observables env '["φ", "θ"] [Double],
                 Observable env "w" String)
  => [String] -> Int -> Int -> Model env ts [String]
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
     Model env ts [[String]]
topicModel vocab n_topics doc_words = do
  mapM (documentDist vocab n_topics) doc_words

{- SIR -}
data Params = Params {
    betaP  :: Double, -- ^ Mean contact rate between susceptible and infected people
    gammaP :: Double, -- ^ Mean recovery rate
    rhoP   :: Double -- ^ Rate of detection
}

data LatState = LatState {
    sus   :: Int, -- ^ Number of people susceptible to infection
    inf   :: Int, -- ^ Number of people currently infected
    recov :: Int -- ^ Number of people recovered from infection
} deriving Show

type SIREnv =
  '[
     "β" ':= Double,
     "γ" ':= Double,
      "ρ" ':= Double,
      "infobs" ':= Int
   ]

type InfectionCount = Int

obsSIR :: Observable env "infobs" Int
  => Double -> LatState -> Model env ts Int
obsSIR rho (LatState _ inf _)  = do
  i <- poisson' (rho * fromIntegral inf) #infobs
  return i

transSI :: Double -> LatState -> Model env ts LatState
transSI beta (LatState sus inf rec) = do
  let pop = sus + inf + rec
  dN_SI <- binomial sus (1 - exp ((-beta * fromIntegral inf) / fromIntegral pop))
  let sus' = sus - dN_SI
      inf' = inf + dN_SI
  return $ LatState sus' inf' rec

transIR :: Double -> LatState -> Model env ts LatState
transIR gamma (LatState sus inf rec)  = do
  dN_IR <- binomial inf (1 - exp (-gamma))
  let inf' = inf - dN_IR
      rec' = rec + dN_IR
  return $ LatState sus inf' rec'

transSIR :: Member (Writer [LatState]) ts
  => Double -> Double -> LatState -> Model env ts LatState
transSIR beta gamma latentSt = do
  latentSt' <- (transSI beta >=> transIR gamma) latentSt
  tellM [latentSt']
  return latentSt'

hmmSIR :: Member (Writer [LatState]) ts
  => Observable env "infobs" Int
  => Params -> LatState -> Model env ts LatState
hmmSIR  (Params beta gamma rho) latentState = do
  latentState'   <- transSIR beta gamma latentState
  infectionCount <- obsSIR rho latentState'
  return latentState'

paramsPrior :: Observables env '["ρ", "β", "γ"] Double
  => Model env ts Params
paramsPrior = do
  pBeta  <- gamma' 2 1 #β
  pGamma <- gamma' 1 (1/8) #γ
  pRho   <- beta' 2 7 #ρ
  return (Params pBeta pGamma pRho)

hmmSIRNsteps ::
     Member (Writer [LatState]) ts
  => (Observable env "infobs" Int, Observables env '["ρ", "β", "γ"] Double)
  => Int -> LatState -> Model env ts LatState
hmmSIRNsteps n latentState  = do
  params       <- paramsPrior
  latentState' <- foldl (>=>) return (replicate n $ hmmSIR params) latentState
  return latentState'
