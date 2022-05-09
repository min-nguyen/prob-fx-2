{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module BenchmarkPrograms where

import GHC.OverloadedLabels
-- import Effects.Reader
import Effects.ObsReader
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
import Env
import Util
import GHC.Show (Show)
import qualified Data.Map as Map
import qualified Inference.SIM as Simulate
import qualified Inference.LW as LW
import qualified Inference.MH as MH

{- Models for benchmarking purposes only. -}

{- Log regression -}
type LogRegrEnv =
    '[  "y" ':= Double,
        "m" ':=  Double,
        "c" ':=  Double,
        "σ" ':=  Double
     ]

linRegr :: forall env rs .
  Observables env '["y", "m", "c", "σ"] Double =>
  [Double] -> Model env rs [Double]
linRegr xs = do
  m <- normal 0 3 #m
  c <- normal 0 5 #c
  σ <- uniform 1 3 #σ
  foldM (\ys x -> do y <- normal (m * x + c) σ #y
                     return (y:ys)) [] xs

mkRecordLogRegr :: ([Double],  [Double],  [Double],  [Double]) -> Env LogRegrEnv
mkRecordLogRegr (y_vals, m_vals, c_vals, σ_vals) =
  (#y := y_vals) <:> (#m := m_vals) <:> (#c := c_vals) <:> (#σ := σ_vals) <:> eNil

mkRecordLogRegrY :: [Double] -> Env LogRegrEnv
mkRecordLogRegrY y_vals =
  (#y := y_vals) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:> eNil

-- Execute linear regression
simLogRegr :: Int -> Int -> Sampler ()
simLogRegr n_datapoints n_samples = do
  let n_datapoints' = fromIntegral n_datapoints
      xs            = [0 .. n_datapoints']
  Simulate.simulateMany n_samples (linRegr xs) (mkRecordLogRegr ([], [1.0], [0.0], [1.0]))
  return ()

lwLogRegr :: Int -> Int -> Sampler ()
lwLogRegr n_datapoints n_samples = do
  let n_datapoints' = fromIntegral n_datapoints
      xs            = [0 .. n_datapoints']
      env           = (#y := [3*x | x <- xs]) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  eNil
  LW.lwTopLevel n_samples (linRegr xs) env
  return ()

mhLogRegr :: Int -> Int -> Sampler ()
mhLogRegr n_datapoints n_samples = do
  let n_datapoints' = fromIntegral n_datapoints
      xs            = [0 .. n_datapoints']
      env           = (#y := [3*x | x <- xs]) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  eNil
  MH.mhTopLevel n_samples (linRegr xs) env ONil
  return ()

{- HMM -}
type HMMEnv =
  '[ "y"       ':= Int,
     "trans_p" ':= Double,
     "obs_p"   ':= Double
   ]

transitionModel ::  Double -> Int -> Model env ts Int
transitionModel transition_p x_prev = do
  dX <- boolToInt <$> bernoulli' transition_p
  return (dX + x_prev)

observationModel :: (Observable env "y" Int)
  => Double -> Int -> Model env ts Int
observationModel observation_p x = do
  binomial x observation_p #y

hmmNode :: (Observable env "y" Int)
  => Double -> Double -> Int -> Model env ts Int
hmmNode transition_p observation_p x_prev = do
  x_n <- transitionModel  transition_p x_prev
  y_n <- observationModel observation_p x_n
  return x_n

hmmNSteps :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double)
  => Int -> (Int -> Model env ts Int)
hmmNSteps n x = do
  trans_p <- uniform 0 1 #trans_p
  obs_p   <- uniform 0 1 #obs_p
  foldr (<=<) return  (replicate n (hmmNode trans_p obs_p)) x

-- Execute HMM
mkRecordHMM :: ([Int], Double, Double) -> Env HMMEnv
mkRecordHMM (ys, transp, obsp) = #y := ys <:> #trans_p := [transp] <:> #obs_p := [obsp] <:>  eNil

mkRecordHMMy :: [Int] -> Env HMMEnv
mkRecordHMMy ys = #y := ys <:> #trans_p := [] <:> #obs_p := [] <:>  eNil

hmm_data :: [Int]
hmm_data = [0,1,1,3,4,5,5,5,6,5,6,8,8,9,7,8,9,8,10,10,7,8,10,9,10,10,14,14,14,15,14,15,14,17,17,17,16,17,14,15,16,18,17,19,20,20,20,22,23,22,23,25,21,21,23,25,24,26,28,23,25,23,27,28,28,25,28,29,28,24,27,28,28,32,32,32,33,31,33,34,32,31,33,36,37,39,36,36,32,38,38,38,38,37,40,38,38,39,40,42]

simHMM :: Int -> Int -> Sampler [Int]
simHMM hmm_length n_samples = do
  map fst <$> Simulate.simulateMany n_samples (hmmNSteps hmm_length 0) (mkRecordHMM ([], 0.5, 0.9))

lwHMM :: Int -> Int -> Sampler ()
lwHMM hmm_length n_samples = do
  LW.lwTopLevel n_samples (hmmNSteps hmm_length 0) (mkRecordHMMy hmm_data)
  return ()

mhHMM :: Int -> Int -> Sampler ()
mhHMM hmm_length n_samples = do
  MH.mhTopLevel n_samples (hmmNSteps hmm_length 0) (mkRecordHMMy hmm_data) ONil
  return ()

{- Latent dirichlet allocation (topic model) -}
type TopicEnv =
  '[ "θ" ':= [Double],
     "φ" ':= [Double],
     "w" ':= String
   ]

wordDist :: Observable env "w" String =>
  [String] -> [Double] -> Model env ts String
wordDist vocab ps =
  categorical (zip vocab ps) #w

topicWordPrior :: Observable env "φ" [Double]
  => [String] -> Model env ts [Double]
topicWordPrior vocab
  = dirichlet (replicate (length vocab) 1) #φ

docTopicPrior :: Observable env "θ" [Double]
  => Int -> Model env ts [Double]
docTopicPrior n_topics = dirichlet (replicate n_topics 1) #θ

documentDist :: (Observables env '["φ", "θ"] [Double],
                 Observable env "w" String)
  => [String] -> Int -> Int -> Model env ts [String]
documentDist vocab n_topics n_words = do
  -- Generate distribution over words for each topic
  topic_word_ps <- replicateM n_topics $ topicWordPrior vocab
  -- Distribution over topics for a given document
  doc_topic_ps  <- docTopicPrior n_topics
  replicateM n_words (do  z <- discrete' doc_topic_ps
                          let word_ps = topic_word_ps !! z
                          wordDist vocab word_ps)

topicModel :: (Observables env '["φ", "θ"] [Double],
               Observable env "w" String)
  => [String] ->
     Int      ->
     [Int]    ->
     Model env ts [[String]]
topicModel vocab n_topics doc_words = do
  mapM (documentDist vocab n_topics) doc_words

-- Execute latent dirichlet allocation
vocab :: [String]
vocab = ["DNA", "evolution", "parsing", "phonology"]

topic_data :: [String]
topic_data     = ["DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA"]

mkRecordTopic :: ([[Double]], [[Double]], [String]) -> Env TopicEnv
mkRecordTopic (tps, wps, ys) =  #θ := tps <:>  #φ := wps <:> #w := ys <:> eNil

simLDA :: Int -> Int -> Sampler ()
simLDA n_words n_samples = do
  let params = #θ := [[0.5, 0.5]] <:>
               #φ := [[0.12491280814569208,1.9941599739151505e-2,0.5385152817942926,0.3166303103208638],
                      [1.72605174564027e-2,2.9475900240868515e-2,9.906011619752661e-2,0.8542034661052021]] <:>
               #w := [] <:> eNil
  Simulate.simulateMany n_samples (documentDist vocab 2 n_words) params
  return ()

lwLDA :: Int -> Int -> Sampler ()
lwLDA n_words n_samples = do
  let env = #θ := [] <:>  #φ := [] <:> #w := topic_data <:> eNil
  LW.lwTopLevel n_samples (documentDist vocab 2 n_words) env
  return ()

mhLDA :: Int -> Int -> Sampler ()
mhLDA n_words n_samples = do
  let xs_envs = (n_words, #θ := [] <:>  #φ := [] <:> #w := topic_data <:> eNil)
  MH.mhTopLevel n_samples (documentDist vocab 2 n_words) (mkRecordTopic ([], [], topic_data)) ONil
  return ()
