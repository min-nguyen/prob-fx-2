

{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

{- | A [Latent Dirichlet Allocation (LDA)](https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation) model
     (or topic model) for learning the distribution over words and topics in a text document.
-}

module LDA where

import Model ( MulModel, dirichlet, discrete, categorical' )
import Sampler ( Sampler,  liftIO, uniformD )
import Control.Monad ( replicateM, replicateM_ )
import Data.Kind (Constraint)
import Data.Type.Nat
import Effects.MulDist (Addr(..))
import Env ( Observables, Observable(..), Assign((:=)), Env, enil, (<:>), vnil, (<#>), Vars (VCons) )
import qualified Trace
import           Trace (Key(..))
import Dist
import Vec (Vec(..), TypeableSNatI)
import qualified Vec
import Inference.MC.SIM as SIM ( simulateWith )
import Inference.MC.LW as LW ( lwWith )
import Inference.MC.SSMH as SSMH ( ssmhWith )
import Inference.MC.SMC as SMC ( mulpfilterWith )
import Inference.MC.RMPF as RMPF ( rmpfWith )
import Inference.MC.PMH as PMH ( pmhWith )
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
  -> MulModel env ts (Vec n Double)
docTopicPrior n_topics = dirichlet (Vec.replicate n_topics 1) #θ

-- | Prior distribution for words in a topic
topicWordPrior :: (TypeableSNatI m, Observable env "φ" (Vec m Double))
  -- | vocabulary
  => Vec m String
  -- | probability of each word
  -> MulModel env ts (Vec m Double)
topicWordPrior vocab
  = dirichlet (Vec.replicate snat 1) #φ

-- | A distribution generating words according to their probabilities
wordDist :: (SNatI m, Observable env "w" String)
  -- | vocabulary
  => Vec m String
  -- | probability of each word
  -> [Double]
  -- | generated word
  -> MulModel env ts String
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
  -> MulModel env ts [String]
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
  => Vec m String -> SNat n -> Int -> MulModel env es ()
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
  -> MulModel env ts [[String]]
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
  (words, env_out) <- SIM.simulateWith (topicModel vocab n_topics n_words) env_in
  return words

-- | Example document of words to perform inference over
document :: [String]
document = concat $ repeat ["DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA"]

-- | LW inference on topic model
lwLDA :: Int -> Int -> Sampler ([[Double]], [Double])
lwLDA n_lwsteps n_words = do
  -- Do SSMH inference over the topic model using the above data
  let n_topics  = snat @(FromGHC 2)
      env_in = #θ := [] <:>  #φ := [] <:> #w := document <:> enil
  (env_outs, ws) <- unzip <$> LW.lwWith n_lwsteps (topicModel vocab n_topics n_words) env_in
  -- Draw the most recent sampled parameters
  let env_pred   = head env_outs
      θs         = get #θ env_pred
  return (map Vec.toList θs, ws)

-- | SSMH inference on topic model (predictive)
ssmhLDA :: Int -> Int -> Sampler ([[Double]], [[Double]])
ssmhLDA n_mhsteps n_words = do
  -- Do SSMH inference over the topic model using the above data
  let n_topics  = snat @(FromGHC 2)
      env_in = #θ := [] <:>  #φ := [] <:> #w := take n_words document <:> enil
  env_outs <- SSMH.ssmhWith n_mhsteps (topicModel vocab n_topics n_words) env_in (#φ <#> #θ <#> vnil)
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
  env_outs <- SMC.mulpfilterWith n_particles (topicModel vocab n_topics n_words) env_in
  -- Draw a random particle's environment
  env_pred_idx <- uniformD 0 (length env_outs - 1)
  let env_pred   = env_outs !! env_pred_idx
      θs         = get #θ env_pred
      φs         = get #φ env_pred
  return (map Vec.toList θs, map Vec.toList φs)

-- | RMPF inference on topic model (predictive)
rmpfLDA :: Int -> Int -> Int -> Sampler ([[Double]], [[Double]])
rmpfLDA n_particles n_mhsteps n_words = do

  let n_topics  = snat @(FromGHC 2)
      env_in = #θ := [] <:>  #φ := [] <:> #w := take n_words document  <:> enil

  env_outs     <- RMPF.rmpfWith n_particles n_mhsteps (topicModel vocab n_topics n_words) env_in vnil
  -- Draw a random particle's environment
  env_pred_idx <- uniformD 0 (length env_outs - 1)
  let env_pred   = env_outs !! env_pred_idx
      θs         = get #θ env_pred
      φs         = get #φ env_pred
  return (map Vec.toList θs, map Vec.toList φs)

-- | PMH inference on topic model (predictive)
pmhLDA :: Int -> Int -> Int -> Sampler ([[Double]], [[Double]])
pmhLDA n_mhsteps n_particles n_words = do

  let n_topics  = snat @(FromGHC 2)
      env_in = #θ := [] <:>  #φ := [] <:> #w := take n_words document  <:> enil

  env_outs     <- PMH.pmhWith n_mhsteps n_particles  (topicModel vocab n_topics n_words) env_in (#φ <#> #θ <#> vnil)
  -- Draw the most recent sampled parameters
  let env_pred   = head env_outs
      θs         = get #θ env_pred
      φs         = get #φ env_pred
  return (map Vec.toList θs, map Vec.toList φs)

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

ssmhLDAMB :: Int -> Int -> IO [([String], Env TopicEnv)]
ssmhLDAMB n_mhsteps n_words  = do
  let env_in = #θ := [] <:>  #φ := [] <:> #w := document <:> enil
  Bayes.sampleIO $ Bayes.unweighted $ Bayes.mh n_mhsteps (mbayesLDA vocab 2 n_words env_in)
-}