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
module LDA where

import Model
import Sampler
import Control.Monad
import Data.Kind (Constraint)
import Env
import Inference.SIM as SIM ( simulate )
import Inference.MH as MH
import Inference.MBAYES as MBAYES
import Trace
import Numeric.Log
import qualified Control.Monad.Bayes.Class as MB
import qualified Control.Monad.Bayes.Weighted as MB
import qualified Control.Monad.Bayes.Traced as MB
import qualified Control.Monad.Bayes.Sampler as MB


-- ||| Latent dirichlet allocation environment (topic model)
type TopicEnv =
  '[ "θ" ':= [Double],
     "φ" ':= [Double],
     "w" ':= String
   ]

vocab :: [String] -- Possible vocabulary
vocab = ["DNA", "evolution", "parsing", "phonology"]

document :: [String] -- Document of words to perform inference over
document = ["DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA"]

-- ||| Latent dirichlet allocation

-- | Prior distributions
topicWordPrior :: Observable env "φ" [Double]
  => [String] -> Model env ts [Double]
topicWordPrior vocab
  = dirichlet (replicate (length vocab) 1) #φ

docTopicPrior :: Observable env "θ" [Double]
  => Int -> Model env ts [Double]
docTopicPrior n_topics = dirichlet (replicate n_topics 1) #θ

-- | Distribution over likely words
wordDist :: Observable env "w" String =>
  [String] -> [Double] -> Model env ts String
wordDist vocab ps =
  discrete (zip vocab ps) #w

-- | Distribution over the topics in a document, over the distribution of words in a topic
topicModel :: (Observables env '["φ", "θ"] [Double],
                 Observable env "w" String)
  => [String]
  -> Int
  -> Int
  -> Model env ts [String]
topicModel vocab n_topics n_words = do
  -- Generate distribution over words for each topic
  topic_word_ps <- replicateM n_topics $ topicWordPrior vocab
  -- Generate distribution over topics for a given document
  doc_topic_ps  <- docTopicPrior n_topics
  replicateM n_words (do  z <- categorical' doc_topic_ps
                          let word_ps = topic_word_ps !! z
                          wordDist vocab word_ps)

-- | Topic distribution over many topics
topicModels :: (Observables env '["φ", "θ"] [Double],
               Observable env "w" String)
  => [String]  -- Possible vocabulary in a document
  -> Int       -- Assumed number of topics in a document
  -> [Int]     -- Number of words in a document
  -> Model env ts [[String]]
topicModels vocab n_topics doc_words = do
  mapM (topicModel vocab n_topics) doc_words

-- | Simulating from topic model
simLDA :: Int -> Sampler [String]
simLDA n_words = do
  let env_in = #θ := [[0.5, 0.5]] <:>
               #φ := [[0.12491280814569208,1.9941599739151505e-2,0.5385152817942926,0.3166303103208638],
                      [1.72605174564027e-2,2.9475900240868515e-2,9.906011619752661e-2,0.8542034661052021]] <:>
               #w := [] <:> enil
  (words, env_out) <- SIM.simulate (topicModel vocab 2 n_words) env_in 
  return words

-- | Inference from topic model
mhPredLDA :: Int -> Int -> Sampler ([[Double]], [[Double]])
mhPredLDA n_mhsteps n_words = do
  -- Do MH inference over the topic model using the above data
  let n_topics  = 2
      env_mh_in = #θ := [] <:>  #φ := [] <:> #w := document <:> enil
  env_mh_outs <- MH.mh n_mhsteps (topicModel vocab n_topics n_words) env_mh_in (#φ <#> #θ <#> vnil)
  -- Draw the most recent sampled parameters 
  let env_pred   = head env_mh_outs
      θs         = get #θ env_pred
      φs         = get #φ env_pred
  return (θs, φs)

-- ||| Topic model using monad-bayes inference
mbayesLDA :: (FromSTrace env, MB.MonadInfer m, Observables env '["φ", "θ"] [Double], Observable env "w" String)
  => [String] -> Int -> Int -> Env env -> m ([String], Env env)
mbayesLDA vocab n_topics n_words = toMBayes (topicModel vocab n_topics n_words)

simLDAMB :: Int -> IO ([String], Env TopicEnv)
simLDAMB n_words  = do
  let env = #θ := [[0.5, 0.5]] <:>
            #φ := [[0.12491280814569208,1.9941599739151505e-2,0.5385152817942926,0.3166303103208638],
                   [1.72605174564027e-2,2.9475900240868515e-2,9.906011619752661e-2,0.8542034661052021]] <:>
            #w := [] <:> enil
  MB.sampleIO $ MB.prior (mbayesLDA vocab 2 n_words env) 

lwLDA :: Int -> Int -> IO [(([String], Env TopicEnv), Log Double)]
lwLDA n_samples n_words  = do
  let env = #θ := [] <:>  #φ := [] <:> #w := document <:> enil
  MB.sampleIO $ replicateM n_samples (MB.runWeighted $ mbayesLDA vocab 2 n_words env) 

mhLDA :: Int -> Int -> IO [([String], Env TopicEnv)]
mhLDA n_mhsteps n_words  = do
  let env = #θ := [] <:>  #φ := [] <:> #w := document <:> enil
  MB.sampleIO $ MB.prior $ MB.mh n_mhsteps (mbayesLDA vocab 2 n_words env) 
