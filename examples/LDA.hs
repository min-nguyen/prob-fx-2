
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

{- | A [Latent Dirichlet Allocation (LDA)](https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation) model
     (or topic model) for learning the distribution over words and topics in a text document.
-}

module LDA where

import Model ( Model, dirichlet, discrete, categorical' )
import Sampler ( Sampler, sampleUniformD, liftIO )
import Control.Monad ( replicateM )
import Data.Kind (Constraint)
import GHC.TypeNats
import Data.Proxy
import Env ( Observables, Observable(..), Assign((:=)), Env, enil, (<:>), vnil, (<#>), Vars (VCons) )
import Trace
import PrimDist
import Vec (Vec)
import qualified Vec
import Inference.SIM as SIM ( simulate )
import Inference.LW as LW ( lw )
import Inference.MH as MH ( mh )
import Inference.SMC as SMC ( smc )
import Inference.RMSMC as RMSMC ( rmsmc )
import Inference.PMMH as PMMH ( pmmh )
import Inference.SMC2 as SMC2 ( smc2 )
import Inference.BBVI as BBVI
import Data.Maybe
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
docTopicPrior :: (KnownNat n, Observable env "θ" (Vec n Double))
  -- | number of topics
  => Proxy n
  -- | probability of each topic
  -> Model env ts (Vec n Double)
docTopicPrior n_topics = dirichlet (Vec.replicateNat n_topics 1) #θ

-- | Prior distribution for words in a topic
topicWordPrior :: forall m env ts. Observable env "φ" (Vec m Double)
  -- | vocabulary
  => Vec m String
  -- | probability of each word
  -> Model env ts (Vec m Double)
topicWordPrior vocab
  = dirichlet (Vec.replicateNat (Proxy @m) 1) #φ

-- | A distribution generating words according to their probabilities
wordDist :: Observable env "w" String
  -- | vocabulary
  => Vec m String
  -- | probability of each word
  -> [Double]
  -- | generated word
  -> Model env ts String
wordDist vocab ps =
  discrete (zip (Vec.toList vocab) ps) #w

-- | Distribution over the topics in a document, over the distribution of words in a topic
topicModel :: (Observable env "φ" (Vec m Double),
               Observable env "θ" (Vec n Double),
               Observable env "w" String)
  -- | vocabulary
  => Vec m String
  -- | number of topics
  -> Proxy n
  -- | number of words
  -> Int
  -- | generated words
  -> Model env ts [String]
topicModel vocab n_topics n_words = do
  -- Generate distribution over words for each topic
  topic_word_ps <- Vec.replicateMNat n_topics $ topicWordPrior vocab
  let topic_word_ps' = map Vec.toList $ Vec.toList topic_word_ps
  -- Generate distribution over topics for a given document
  doc_topic_ps  <- docTopicPrior n_topics
  replicateM n_words (do  z <- categorical' (Vec.toList doc_topic_ps)
                          let word_ps = topic_word_ps' !! z
                          wordDist vocab word_ps)

-- | Topic distribution over many topics
topicModels :: (Observable env "φ" (Vec m Double),
               Observable env "θ" (Vec n Double),
               Observable env "w" String)
  -- | vocabulary
  => Vec m String
  -- | number of topics
  -> Proxy n
  -- | number of words for each document
  -> [Int]
  -- | generated words for each document
  -> Model env ts [[String]]
topicModels vocab n_topics doc_words = do
  mapM (topicModel vocab n_topics) doc_words


-- | Example possible vocabulary
vocab :: Vec 4 String
vocab = fromJust $ Vec.fromList ["DNA", "evolution", "parsing", "phonology"]

-- | Simulating from topic model
simLDA :: Int -> Sampler [String]
simLDA n_words = do
  -- Specify model inputs
  let n_topics = Proxy @2
  -- Specify model environment
      env_in = #θ := [[0.5, 0.5]] <:>
               #φ := [[0.12491280814569208,1.9941599739151505e-2,0.5385152817942926,0.3166303103208638],
                      [1.72605174564027e-2,2.9475900240868515e-2,9.906011619752661e-2,0.8542034661052021]] <:>
               #w := [] <:> enil
  -- Simulate from topic model
  (words, env_out) <- SIM.simulate (topicModel vocab n_topics n_words) env_in
  return words

-- | Example document of words to perform inference over
document :: [String]
document = ["DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA"]

-- | LW inference on topic model
lwLDA :: Int -> Int -> Sampler ([[Double]], [Double])
lwLDA n_lwsteps n_words = do
  -- Do MH inference over the topic model using the above data
  let n_topics  = 2
      env_in = #θ := [] <:>  #φ := [] <:> #w := document <:> enil
  (env_outs, ws) <- unzip <$> LW.lw n_lwsteps (topicModel vocab n_topics n_words) env_in
  -- Draw the most recent sampled parameters
  let env_pred   = head env_outs
      θs         = get #θ env_pred
  return (θs, ws)

-- | MH inference on topic model (predictive)
mhPredLDA :: Int -> Int -> Sampler ([[Double]], [[Double]])
mhPredLDA n_mhsteps n_words = do
  -- Do MH inference over the topic model using the above data
  let n_topics  = 2
      env_in = #θ := [] <:>  #φ := [] <:> #w := document <:> enil
  env_outs <- MH.mh n_mhsteps (topicModel vocab n_topics n_words) env_in (#φ <#> #θ <#> vnil)
  -- Draw the most recent sampled parameters
  let env_pred   = head env_outs
      θs         = get #θ env_pred
      φs         = get #φ env_pred
  return (θs, φs)

-- | SMC inference on topic model (predictive)
smcPredLDA :: Int -> Int -> Sampler ([[Double]], [[Double]])
smcPredLDA n_particles n_words = do
  let n_topics  = 2
      env_in = #θ := [] <:>  #φ := [] <:> #w := document <:> enil

  env_outs <- SMC.smc n_particles (topicModel vocab n_topics n_words) env_in
  -- Draw a random particle's environment
  env_pred_idx <- sampleUniformD 0 (length env_outs - 1)
  let env_pred   = env_outs !! env_pred_idx
      θs         = get #θ env_pred
      φs         = get #φ env_pred
  return (θs, φs)

-- | RMSMC inference on topic model (predictive)
rmsmcPredLDA :: Int -> Int -> Int -> Sampler ([[Double]], [[Double]])
rmsmcPredLDA n_particles n_mhsteps n_words = do

  let n_topics  = 2
      env_in = #θ := [] <:>  #φ := [] <:> #w := document <:> enil

  env_outs     <- RMSMC.rmsmc n_particles n_mhsteps (topicModel vocab n_topics n_words) env_in vnil
  -- Draw a random particle's environment
  env_pred_idx <- sampleUniformD 0 (length env_outs - 1)
  let env_pred   = env_outs !! env_pred_idx
      θs         = get #θ env_pred
      φs         = get #φ env_pred
  return (θs, φs)

-- | PMMH inference on topic model (predictive)
pmmhPredLDA :: Int -> Int -> Int -> Sampler ([[Double]], [[Double]])
pmmhPredLDA n_mhsteps n_particles n_words = do

  let n_topics  = 2
      env_in = #θ := [] <:>  #φ := [] <:> #w := document <:> enil

  env_outs     <- PMMH.pmmh n_mhsteps n_particles  (topicModel vocab n_topics n_words) env_in (#φ <#> #θ <#> vnil)
  -- Draw the most recent sampled parameters
  let env_pred   = head env_outs
      θs         = get #θ env_pred
      φs         = get #φ env_pred
  return (θs, φs)

-- | SMC2 inference on topic model (predictive)
smc2PredLDA :: Int ->  Int -> Int -> Int -> Sampler ([[Double]], [[Double]])
smc2PredLDA n_outer_particles n_mhsteps n_inner_particles n_words = do

  let n_topics  = 2
      env_in = #θ := [] <:>  #φ := [] <:> #w := document <:> enil

  env_outs     <- SMC2.smc2 n_outer_particles n_mhsteps n_inner_particles (topicModel vocab n_topics n_words) env_in (#φ <#> #θ <#> vnil)
  -- Draw the most recent sampled parameters
  let env_pred   = head env_outs
      θs         = get #θ env_pred
      φs         = get #φ env_pred
  return (θs, φs)

-- | BBVI inference on topic model
bbviLDA :: Int -> Int -> Int -> Sampler ([Double], [Double], [Double])
bbviLDA t_steps l_samples n_words = do

  let n_topics  = 2
      env_in = #θ := [] <:>  #φ := [] <:> #w := document <:> enil

  traceQ <- BBVI.bbvi t_steps l_samples (topicModel vocab n_topics n_words) env_in
  -- Draw the most recent sampled parameters
  -- let θ_dist     = toList . fromJust $ dlookup (DKey ("θ", 0) :: DKey Dirichlet) traceQ
  --     φ0_dist    = toList . fromJust $ dlookup (DKey ("φ", 0) :: DKey Dirichlet) traceQ
  --     φ1_dist    = toList . fromJust $ dlookup (DKey ("φ", 1) :: DKey Dirichlet) traceQ
  -- return (θ_dist, φ0_dist, φ1_dist)
  undefined

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