

{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

{- | A [Latent Dirichlet Allocation (LDA)](https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation) model
     (or topic model) for learning the distribution over words and topics in a text document.
-}

module LDA where

import Model ( Model, dirichlet, discrete, categorical' )
import Sampler ( Sampler, sampleUniformD, liftIO )
import Control.Monad ( replicateM, replicateM_ )
import Data.Kind (Constraint)
import Data.Type.Nat
import Effects.Dist (Addr(..))
import Env ( Observables, Observable(..), Assign((:=)), Env, enil, (<:>), vnil, (<#>), Vars (VCons) )
import qualified Trace
import           Trace (Key(..))
import PrimDist
import Vec (Vec(..), TypeableSNatI)
import qualified Vec
import Inference.MC.SIM as SIM ( simulate )
import Inference.MC.LW as LW ( lw )
import Inference.MC.MH as MH ( mh )
import Inference.MC.SMC as SMC ( smc )
import Inference.MC.RMSMC as RMSMC ( rmsmc )
import Inference.MC.PMMH as PMMH ( pmmh )
import Inference.VI.BBVI as BBVI
import Inference.VI.INVI as INVI
import Inference.VI.Extra.BBVI_Combined as BBVI_Combined
import Data.Maybe
import Data.Typeable
{-
import Numeric.Log ( Log )
import Inference.MB as MB ( handleMBayes )
import qualified Control.Monad.Bayes.Class as Bayes
import qualified Control.Monad.Bayes.Sampler.Strict as Bayes
import qualified Control.Monad.Bayes.Traced as Bayes
import qualified Control.Monad.Bayes.Weighted as Bayes
-}

{- | An LDA environment.

     Assuming 1 document with 2 topics and a vocabulary of 4 words,
     the parameters of the model environment would have the following shape:

      θ would be [[prob_topic_1, prob_topic_2]                         -- probabilities of topics in document 1
                 ]

      φ would be [[prob_word_1, prob_word_2, prob_word_3, prob_word_4] -- probabilities of words in topic 1
                  [prob_word_1, prob_word_2, prob_word_3, prob_word_4] -- probabilities of words in topic 2
                 ]
-}
type TopicEnv =
  '[ "θ" ':= [Double],  -- ^ probabilities of each topic in a document
     "φ" ':= [Double],  -- ^ probabilities of each word in a topic
     "w" ':= String     -- ^ word
   ]

-- | Prior distribution for topics in a document
docTopicPrior :: (TypeableSNatI n, Observable env "θ" (Vec n Double))
  -- | number of topics
  => SNat n
  -- | probability of each topic
  -> Model env ts (Vec n Double)
docTopicPrior n_topics = dirichlet (Vec.replicate n_topics 1) #θ

-- | Prior distribution for words in a topic
topicWordPrior :: (TypeableSNatI m, Observable env "φ" (Vec m Double))
  -- | vocabulary
  => Vec m String
  -- | probability of each word
  -> Model env ts (Vec m Double)
topicWordPrior vocab
  = dirichlet (Vec.replicate snat 1) #φ

-- | A distribution generating words according to their probabilities
wordDist :: (SNatI m, Observable env "w" String)
  -- | vocabulary
  => Vec m String
  -- | probability of each word
  -> [Double]
  -- | generated word
  -> Model env ts String
wordDist vocab ps =
  discrete (zip (Vec.toList vocab) ps) #w

-- | Distribution over the topics in a document, over the distribution of words in a topic
topicModel :: (TypeableSNatI m, TypeableSNatI n,
               Observable env "φ" (Vec m Double),
               Observable env "θ" (Vec n Double),
               Observable env "w" String)
  -- | vocabulary
  => Vec m String
  -- | number of topics
  -> SNat n
  -- | number of words
  -> Int
  -- | generated words
  -> Model env ts [String]
topicModel vocab n_topics n_words = do
  -- Generate distribution over words for each topic
  topic_word_ps <- (Vec.replicateM n_topics . topicWordPrior) vocab
  let topic_word_ps' = (map Vec.toList . Vec.toList) topic_word_ps
  -- Generate distribution over topics for a given document
  doc_topic_ps  <- docTopicPrior n_topics
  replicateM n_words (do  z <- categorical' (Vec.toList doc_topic_ps)
                          let word_ps = topic_word_ps' !! z
                          wordDist vocab word_ps)

topicGuide :: (TypeableSNatI m, TypeableSNatI n,
               Observable env "φ" (Vec m Double),
               Observable env "θ" (Vec n Double))
  => Vec m String -> SNat n -> Int -> Model env es ()
topicGuide vocab n_topics n_words = do
  topic_word_ps <- (Vec.replicateM n_topics . topicWordPrior) vocab
  doc_topic_ps  <- docTopicPrior n_topics
  replicateM_ n_words (categorical' (Vec.toList doc_topic_ps))

-- | Topic distribution over many topics
topicModels :: (TypeableSNatI n, TypeableSNatI m,
                Observable env "φ" (Vec m Double),
                Observable env "θ" (Vec n Double),
                Observable env "w" String)
  -- | vocabulary
  => Vec m String
  -- | number of topics
  -> SNat n
  -- | number of words for each document
  -> [Int]
  -- | generated words for each document
  -> Model env ts [[String]]
topicModels vocab n_topics doc_words = do
  mapM (topicModel vocab n_topics) doc_words

-- | Example possible vocabulary
vocab :: Vec (FromGHC 4) String
vocab = "DNA" ::: "evolution" ::: "parsing"::: "phonology"::: VNil

-- | Simulating from topic model
simLDA :: Int -> Sampler [String]
simLDA n_words = do
  -- Specify model inputs
  let n_topics = snat @(FromGHC 2)

  -- Specify model environment
      env_in = #θ := [0.5 ::: 0.5 ::: VNil] <:>
               #φ := [0.12491280814569208:::1.9941599739151505e-2:::0.5385152817942926:::0.3166303103208638:::VNil,
                      1.72605174564027e-2:::2.9475900240868515e-2:::9.906011619752661e-2:::0.8542034661052021:::VNil] <:>
               #w := [] <:> enil
  -- Simulate from topic model
  (words, env_out) <- SIM.simulate (topicModel vocab n_topics n_words) env_in
  return words

-- | Example document of words to perform inference over
document :: [String]
document = concat $ repeat ["DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA"]

-- | LW inference on topic model
lwLDA :: Int -> Int -> Sampler ([[Double]], [Double])
lwLDA n_lwsteps n_words = do
  -- Do MH inference over the topic model using the above data
  let n_topics  = snat @(FromGHC 2)
      env_in = #θ := [] <:>  #φ := [] <:> #w := document <:> enil
  (env_outs, ws) <- unzip <$> LW.lw n_lwsteps (topicModel vocab n_topics n_words) env_in
  -- Draw the most recent sampled parameters
  let env_pred   = head env_outs
      θs         = get #θ env_pred
  return (map Vec.toList θs, ws)

-- | MH inference on topic model (predictive)
mhLDA :: Int -> Int -> Sampler ([[Double]], [[Double]])
mhLDA n_mhsteps n_words = do
  -- Do MH inference over the topic model using the above data
  let n_topics  = snat @(FromGHC 2)
      env_in = #θ := [] <:>  #φ := [] <:> #w := take n_words document <:> enil
  env_outs <- MH.mh n_mhsteps (topicModel vocab n_topics n_words) env_in (#φ <#> #θ <#> vnil)
  -- Draw the most recent sampled parameters
  let env_pred   = head env_outs
      θs         = get #θ env_pred
      φs         = get #φ env_pred
  return (map Vec.toList θs, map Vec.toList φs)

-- | SMC inference on topic model (predictive)
smcLDA :: Int -> Int -> Sampler ([[Double]], [[Double]])
smcLDA n_particles n_words = do
  let n_topics  = snat @(FromGHC 2)
      env_in = #θ := [] <:>  #φ := [] <:> #w := take n_words document  <:> enil
  env_outs <- SMC.smc n_particles (topicModel vocab n_topics n_words) env_in
  -- Draw a random particle's environment
  env_pred_idx <- sampleUniformD 0 (length env_outs - 1)
  let env_pred   = env_outs !! env_pred_idx
      θs         = get #θ env_pred
      φs         = get #φ env_pred
  return (map Vec.toList θs, map Vec.toList φs)

-- | RMSMC inference on topic model (predictive)
rmsmcLDA :: Int -> Int -> Int -> Sampler ([[Double]], [[Double]])
rmsmcLDA n_particles n_mhsteps n_words = do

  let n_topics  = snat @(FromGHC 2)
      env_in = #θ := [] <:>  #φ := [] <:> #w := take n_words document  <:> enil

  env_outs     <- RMSMC.rmsmc n_particles n_mhsteps (topicModel vocab n_topics n_words) env_in vnil
  -- Draw a random particle's environment
  env_pred_idx <- sampleUniformD 0 (length env_outs - 1)
  let env_pred   = env_outs !! env_pred_idx
      θs         = get #θ env_pred
      φs         = get #φ env_pred
  return (map Vec.toList θs, map Vec.toList φs)

-- | PMMH inference on topic model (predictive)
pmmhLDA :: Int -> Int -> Int -> Sampler ([[Double]], [[Double]])
pmmhLDA n_mhsteps n_particles n_words = do

  let n_topics  = snat @(FromGHC 2)
      env_in = #θ := [] <:>  #φ := [] <:> #w := take n_words document  <:> enil

  env_outs     <- PMMH.pmmh n_mhsteps n_particles  (topicModel vocab n_topics n_words) env_in (#φ <#> #θ <#> vnil)
  -- Draw the most recent sampled parameters
  let env_pred   = head env_outs
      θs         = get #θ env_pred
      φs         = get #φ env_pred
  return (map Vec.toList θs, map Vec.toList φs)

-- | BBVI inference on topic model, using a custom guide
bbviLDA :: Int -> Int -> Int -> Sampler ([Double], [Double], [Double])
bbviLDA t_steps l_samples n_words = do

  let n_topics  = snat @(FromGHC 2)
      env_in = #θ := [] <:>  #φ := [] <:> #w := take n_words document  <:> enil

  traceQ <- BBVI.bbvi t_steps l_samples (topicGuide vocab n_topics n_words) (topicModel vocab n_topics n_words) env_in
  -- Draw the most recent sampled parameters
  let θ_dist     = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 2)) ((== "θ") . tag) traceQ
      φ0_dist    = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 4)) (\(Addr _ t i) -> (t, i) == ("φ", 0)) traceQ
      φ1_dist    = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 4)) (\(Addr _ t i) -> (t, i) == ("φ", 1)) traceQ
  return (θ_dist, φ0_dist, φ1_dist)

-- | BBVI inference on topic model, using the model to generate a default guide
bbviDefaultLDA :: Int -> Int -> Int -> Sampler ([Double], [Double], [Double])
bbviDefaultLDA t_steps l_samples n_words = do

  let n_topics  = snat @(FromGHC 2)
      env_in = #θ := [] <:>  #φ := [] <:> #w := take n_words document  <:> enil

  traceQ <- BBVI.bbvi t_steps l_samples (topicModel vocab n_topics n_words) (topicModel vocab n_topics n_words) env_in
  -- Draw the most recent sampled parameters
  let θ_dist     = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 2))  ((== "θ") . tag) traceQ
      φ0_dist    = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 4)) (\(Addr _ t i) -> (t, i) == ("φ", 0)) traceQ
      φ1_dist    = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 4)) (\(Addr _ t i) -> (t, i) == ("φ", 1)) traceQ
  return (θ_dist, φ0_dist, φ1_dist)

-- | BBVI inference on topic model, using the model to generate a default guide
bbviDefaultCombinedLDA :: Int -> Int -> Int -> Sampler ([Double], [Double], [Double])
bbviDefaultCombinedLDA t_steps l_samples n_words = do

  let n_topics  = snat @(FromGHC 2)
      env_in = #θ := [] <:>  #φ := [] <:> #w := take n_words document  <:> enil

  traceQ <- BBVI_Combined.bbvi t_steps l_samples (topicModel vocab n_topics n_words) env_in
  -- Draw the most recent sampled parameters
  let θ_dist     = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 2))  ((== "θ") . tag) traceQ
      φ0_dist    = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 4)) (\(Addr _ t i) -> (t, i) == ("φ", 0)) traceQ
      φ1_dist    = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 4)) (\(Addr _ t i) -> (t, i) == ("φ", 1)) traceQ
  return (θ_dist, φ0_dist, φ1_dist)

-- | BBVI inference on topic model, using a custom guide
inviLDA :: Int -> Int -> Int -> Sampler ([Double], [Double], [Double])
inviLDA t_steps l_samples n_words = do

  let n_topics  = snat @(FromGHC 2)
      env_in = #θ := [] <:>  #φ := [] <:> #w := take n_words document  <:> enil

  traceQ <- INVI.invi t_steps l_samples  (topicGuide vocab n_topics n_words) (topicModel vocab n_topics n_words) env_in
  -- Draw the most recent sampled parameters
  let θ_dist     = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 2))  ((== "θ") . tag) traceQ
      φ0_dist    = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 4)) (\(Addr _ t i) -> (t, i) == ("φ", 0)) traceQ
      φ1_dist    = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 4)) (\(Addr _ t i) -> (t, i) == ("φ", 1)) traceQ
  return (θ_dist, φ0_dist, φ1_dist)

{- | Executing the topic model using monad-bayes.

mbayesLDA :: (Bayes.MonadInfer m, Observables env '["φ", "θ"] [Double], Observable env "w" String)
  => [String] -> Int -> Int -> Env env -> m ([String], Env env)
mbayesLDA vocab n_topics n_words = MB.handleMBayes (topicModel vocab n_topics n_words)

simLDAMB :: Int -> IO ([String], Env TopicEnv)
simLDAMB n_words  = do
  let env_in = #θ := [[0.5, 0.5]] <:>
            #φ := [[0.12491280814569208,1.9941599739151505e-2,0.5385152817942926,0.3166303103208638],
                   [1.72605174564027e-2,2.9475900240868515e-2,9.906011619752661e-2,0.8542034661052021]] <:>
            #w := [] <:> enil
  Bayes.sampleIO $ Bayes.unweighted (mbayesLDA vocab 2 n_words env_in)

lwLDAMB :: Int -> Int -> IO [(([String], Env TopicEnv), Log Double)]
lwLDAMB n_samples n_words  = do
  let env_in = #θ := [] <:>  #φ := [] <:> #w := document <:> enil
  Bayes.sampleIO $ replicateM n_samples (Bayes.runWeighted $ mbayesLDA vocab 2 n_words env_in)

mhLDAMB :: Int -> Int -> IO [([String], Env TopicEnv)]
mhLDAMB n_mhsteps n_words  = do
  let env_in = #θ := [] <:>  #φ := [] <:> #w := document <:> enil
  Bayes.sampleIO $ Bayes.unweighted $ Bayes.mh n_mhsteps (mbayesLDA vocab 2 n_words env_in)
-}