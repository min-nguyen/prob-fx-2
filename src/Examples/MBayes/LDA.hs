
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use camelCase" #-}

module Examples.MBayes.LDA where

import Effects.ObsReader
import Effects.Writer
import Model
import Inference.MBAYES
import Effects.Dist
import Effects.Lift
import Control.Monad
import Env
import Trace
import Util
import Control.Monad.Bayes.Class (MonadInfer)
import Control.Monad.Bayes.Weighted ( prior, runWeighted )
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced
import Numeric.Log

type TopicEnv =
  '[ "θ" ':= [Double],
     "φ" ':= [Double],
     "w" ':= String
   ]

wordDist :: Observable env "w" String =>
  [String] -> [Double] -> Model env es String
wordDist vocab ps =
  categorical (zip vocab ps) #w

topicWordPrior :: Observable env "φ" [Double]
  => [String] -> Model env es [Double]
topicWordPrior vocab
  = dirichlet (replicate (length vocab) 1) #φ

docTopicPrior :: Observable env "θ" [Double]
  => Int -> Model env es [Double]
docTopicPrior n_topics = dirichlet (replicate n_topics 1) #θ

documentDist :: (Observables env '["φ", "θ"] [Double], Observable env "w" String)
  => [String] -> Int -> Int -> Model env es [String]
documentDist vocab n_topics n_words = do
  -- Generate distribution over words for each topic
  topic_word_ps <- replicateM n_topics $ topicWordPrior vocab
  -- Distribution over topics for a given document
  doc_topic_ps  <- docTopicPrior n_topics
  replicateM n_words (do  z <- discrete' doc_topic_ps
                          let word_ps = topic_word_ps !! z
                          wordDist vocab word_ps)

mbayesLDA :: (FromSTrace env, MonadInfer m, Observables env '["φ", "θ"] [Double], Observable env "w" String)
  => [String] -> Int -> Int -> Env env -> m ([String], Env env)
mbayesLDA vocab n_topics n_words = toMBayes (documentDist vocab n_topics n_words)

{- Executing LDA -}

vocab :: [String]
vocab = ["DNA", "evolution", "parsing", "phonology"]

topic_data :: [String]
topic_data = ["DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA"]

simLDA :: Int -> Int -> IO [([String], Env TopicEnv)]
simLDA n_samples n_words  = do
  let env = #θ := [[0.5, 0.5]] <:>
            #φ := [[0.12491280814569208,1.9941599739151505e-2,0.5385152817942926,0.3166303103208638],
                   [1.72605174564027e-2,2.9475900240868515e-2,9.906011619752661e-2,0.8542034661052021]] <:>
            #w := [] <:> ENil
  sampleIO $ prior $ replicateM n_samples (mbayesLDA vocab 2 n_words env) 

lwLDA :: Int -> Int -> IO [(([String], Env TopicEnv), Log Double)]
lwLDA n_samples n_words  = do
  let env = #θ := [] <:>  #φ := [] <:> #w := topic_data <:> ENil
  sampleIO $ replicateM n_samples (runWeighted $ mbayesLDA vocab 2 n_words env) 

mhLDA :: Int -> Int -> IO [([String], Env TopicEnv)]
mhLDA n_samples n_words  = do
  let env = #θ := [] <:>  #φ := [] <:> #w := topic_data <:> ENil
  sampleIO $ prior $ mh n_samples (mbayesLDA vocab 2 n_words env) 
