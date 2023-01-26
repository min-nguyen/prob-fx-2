module MonadBayes where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Traced.Basic
import Control.Monad.Bayes.Sampler.Lazy
import Control.Monad.Bayes.Weighted
import Criterion.Main
import Criterion (benchmark)
import           Statistics.Distribution        ( logProbability )
import qualified Statistics.Distribution.Binomial  as SB
import           Numeric.Log
import Control.Monad
import Data.Bifunctor
import Data.Maybe
import Data.Vector (Vector, fromList, toList)
import Data.List
import Control.Monad.Bayes.Inference.MCMC
import Control.Monad.Bayes.Population (multinomial, resampleMultinomial, runPopulation)
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Inference.PMMH

{- Lin Regression -}

data LinRegrParams = LinRegrParams {
  m :: Double, c :: Double, σ :: Double
} deriving Show

fromLinRegrParams :: LinRegrParams -> (Double, Double, Double)
fromLinRegrParams (LinRegrParams m c σ) = (m, c, σ)

linRegrPrior :: MonadDistribution m => m LinRegrParams
linRegrPrior = do
  m <- normal 0 3
  c <- normal 0 5
  σ <- uniform 1 3
  return (LinRegrParams m c σ)

linRegr :: MonadMeasure m => [(Double, Double)] -> LinRegrParams -> m LinRegrParams
linRegr xys (LinRegrParams m c σ ) = do
  LinRegrParams m c σ <- linRegrPrior
  mapM_ (\(x, y_obs) -> score (normalPdf (m * x + c) σ y_obs)) xys
  return (LinRegrParams m c σ)

linRegrData :: Int -> [(Double, Double)]
linRegrData n_datapoints = zip [0 .. (fromIntegral n_datapoints)] (map (*3) [0 .. (fromIntegral n_datapoints)])

-- Execute log regression
mhLinRegr :: Int -> Int -> IO [(Double, Double, Double)]
mhLinRegr mh_steps n_datapoints = do
  x <- sampler . unweighted $ mh mh_steps $ linRegrPrior >>= linRegr (linRegrData n_datapoints)
  return (map fromLinRegrParams x)

smcLinRegr :: Int -> Int -> IO [(Double, Double, Double)]
smcLinRegr n_particles n_datapoints = do
  let n_timesteps = n_datapoints
      smc_config  = SMCConfig { resampler = resampleMultinomial, numSteps = n_timesteps, numParticles = n_particles }
  x <- sampler . runPopulation . smc smc_config $ linRegrPrior >>= linRegr (linRegrData n_datapoints)
  return  (map (fromLinRegrParams . fst) x)

pmmhLinRegr :: Int -> Int -> Int -> IO [(Double, Double, Double)]
pmmhLinRegr mh_steps n_particles n_datapoints = do
  let n_timesteps = n_datapoints
  let mcmc_config  = MCMCConfig { proposal = SingleSiteMH, numMCMCSteps = mh_steps, numBurnIn = 0 }
      smc_config   = SMCConfig { resampler = resampleMultinomial, numSteps = n_timesteps, numParticles = n_particles }
  x <- sampler $ pmmh mcmc_config smc_config linRegrPrior (linRegr (linRegrData n_datapoints))
  return (map (fromLinRegrParams . fst) (concat x))

{- Hidden Markov Model -}

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

binomial :: MonadDistribution m => Int -> Double -> m Int
binomial n p = discrete $ SB.binomial n p

binomialPdf :: Int -> Double -> Int -> Log Double
binomialPdf n p y = Exp $ logProbability (SB.binomial n p) y

data HMMParams = HMMParams {
    transition_p  :: {-# UNPACK #-} !Double,
    observation_p :: {-# UNPACK #-} !Double
} deriving Show

fromHMMParams :: HMMParams -> (Double, Double)
fromHMMParams (HMMParams trans_p obs_p) = (trans_p, obs_p)

initialParams :: HMMParams
initialParams = HMMParams 0.5 0.5

-- HMM SIM Model
simulateHmm1step :: MonadDistribution m => HMMParams -> Int -> m (Int, Int)
simulateHmm1step (HMMParams transition_p observation_p) x = do
  dX <- bernoulli transition_p
  let x' = x + boolToInt dX
  y  <- binomial x observation_p
  return (x', y)

simulateHmmNsteps :: MonadDistribution m => HMMParams -> Int -> Int -> m [Int]
simulateHmmNsteps params x n = f x [] n
  where f x ys 0  = return ys
        f x ys n' = do
          (x', y) <- simulateHmm1step params x
          f x' (ys ++ [y]) (n' - 1)

-- HMM Infer Model
obsModel :: MonadMeasure m => Double -> Int -> Int -> m Int
obsModel observation_p x y_obs = do
  score (binomialPdf x observation_p y_obs)
  return y_obs

transModel :: MonadDistribution m => Double -> Int -> m Int
transModel transition_p x = do
  dX <- bernoulli transition_p
  return (x + boolToInt dX)

inferHmmNsteps :: MonadMeasure m => HMMParams -> Int -> [Int] -> m Int
inferHmmNsteps (HMMParams transition_p observation_p) x [] = return x
inferHmmNsteps p@(HMMParams transition_p observation_p) x (y:ys) = do
  x' <- transModel transition_p x
  y' <- obsModel observation_p x' y
  inferHmmNsteps p x' ys

hmmPrior :: MonadDistribution m => m HMMParams
hmmPrior = do
  trans_p <- uniform 0 1
  obs_p   <- uniform 0 1
  return (HMMParams trans_p obs_p)

hmm :: MonadMeasure m => Int -> [Int] -> HMMParams -> m HMMParams
hmm x_0 ys params = do
  inferHmmNsteps params x_0 ys
  return params

--- Execute HMM
simHMM :: Int -> IO [Int]
simHMM  n_steps = sampler $ simulateHmmNsteps initialParams 0 n_steps

mhHMM :: Int -> Int -> IO [(Double, Double)]
mhHMM mh_steps n_datapoints = do
  ys <- simHMM  n_datapoints
  x <- sampler . unweighted . mh mh_steps $ (hmmPrior >>= hmm 0 ( ys))
  return (map fromHMMParams x)

smcHMM :: Int -> Int -> IO [(Double, Double)]
smcHMM n_particles n_datapoints = do
  let n_timesteps = n_datapoints
      smc_config  = SMCConfig { resampler = resampleMultinomial, numSteps = n_timesteps, numParticles = n_particles }
  ys <- simHMM  n_datapoints
  x <- sampler . runPopulation . smc smc_config $ hmmPrior >>= hmm 0 ( ys)
  return  (map (fromHMMParams . fst) x)

pmmhHMM :: Int -> Int -> Int -> IO [(Double, Double)]
pmmhHMM mh_steps n_particles n_datapoints = do
  let n_timesteps = n_datapoints
      mcmc_config  = MCMCConfig { proposal = SingleSiteMH, numMCMCSteps = mh_steps, numBurnIn = 0 }
      smc_config   = SMCConfig { resampler = resampleMultinomial, numSteps = n_timesteps, numParticles = n_particles }
  ys <- simHMM  n_datapoints
  x <- sampler $ pmmh mcmc_config smc_config hmmPrior (hmm 0 ( ys))
  return  (map (fromHMMParams . fst) (concat x))

{- Latent Dirichlet Allocation -}
data LDAParams = LDAParams {
    θ :: [Double],   -- probabilities of each topic in a document
    φ :: [[Double]]  -- probabilities of each word in a topic in a document
   } deriving Show

fromLDAParams :: LDAParams -> ([Double], [[Double]])
fromLDAParams (LDAParams a b) = (a, b)

-- Topic Model SIM
documentDistSim :: MonadDistribution m => LDAParams -> [String] -> Int -> Int -> m [String]
documentDistSim (LDAParams doc_topic_ps topic_word_ps) vocab n_topics n_words = do
  let sampleWord = do
        topic_idx  <- categorical (fromList doc_topic_ps)
        let word_ps = topic_word_ps !! topic_idx
        word_idx   <- categorical (fromList word_ps)
        let word    = vocab !! word_idx
        return word
  replicateM n_words sampleWord

-- Topic Model Infer
wordDist :: MonadMeasure m => [String] -> [Double] -> String -> m String
wordDist vocab ps w = do
  let w_p = ps !! fromJust (elemIndex w vocab)
  score (Exp w_p)
  return w

topicWordPrior :: MonadDistribution m => [String] -> m [Double]
topicWordPrior vocab
  = toList <$> dirichlet (fromList $ replicate (length vocab) 1)

docTopicPrior ::  MonadDistribution m  => Int -> m [Double]
docTopicPrior n_topics
  = toList <$> dirichlet (fromList $ replicate n_topics 1)

ldaPrior :: MonadDistribution m => Int -> [String] -> m LDAParams
ldaPrior n_topics vocab = do
  doc_topic_ps <- docTopicPrior n_topics
  topic_word_ps <- replicateM n_topics $ topicWordPrior vocab
  return (LDAParams doc_topic_ps topic_word_ps)

lda :: MonadMeasure m => Int -> [String] -> [String] -> LDAParams -> m LDAParams
lda n_topics vocab words (LDAParams doc_topic_ps topic_word_ps) = do
  let scoreWords [] = return words
      scoreWords (w:ws) = do
        z <- categorical (fromList doc_topic_ps)
        let word_ps = topic_word_ps !! z
        wordDist vocab word_ps w
        scoreWords ws
  scoreWords words
  return (LDAParams doc_topic_ps topic_word_ps)

-- Execute topic model
vocabulary :: [String]
vocabulary = ["DNA", "evolution", "parsing", "phonology"]

document :: [String]
document = concat $ repeat ["DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA", "DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution","DNA","evolution", "parsing", "phonology", "DNA","evolution", "DNA", "parsing", "evolution","phonology", "evolution", "DNA"]

initialLDAParams :: LDAParams
initialLDAParams = LDAParams [0.5, 0.5] [[0.12491280814569208,1.9941599739151505e-2,0.5385152817942926,0.3166303103208638],[1.72605174564027e-2,2.9475900240868515e-2,9.906011619752661e-2,0.8542034661052021]]

simLDA :: Int -> Int -> IO [[String]]
simLDA n_samples n_words = do
  sampler $ replicateM n_samples (documentDistSim initialLDAParams vocabulary 2 n_words)

mhLDA :: Int -> Int -> IO [([Double], [[Double]])]
mhLDA mh_steps n_words = do
  x <- sampler . unweighted . mh mh_steps  $ (ldaPrior 2 vocabulary >>= lda 2 vocabulary (take n_words document))
  return (map fromLDAParams x)

smcLDA :: Int -> Int -> IO [([Double], [[Double]])]
smcLDA n_particles n_words = do
  let n_timesteps = n_words
      smc_config  = SMCConfig { resampler = resampleMultinomial, numSteps = n_timesteps, numParticles = n_particles }
  x <- sampler . runPopulation . smc smc_config $ (ldaPrior 2 vocabulary >>= lda 2 vocabulary (take n_words document))
  return (map (fromLDAParams . fst) x)

pmmhLDA :: Int -> Int -> Int -> IO [([Double], [[Double]])]
pmmhLDA mh_steps n_particles n_words = do
  let n_timesteps = n_words
      mcmc_config  = MCMCConfig { proposal = SingleSiteMH, numMCMCSteps = mh_steps, numBurnIn = 0 }
      smc_config   = SMCConfig { resampler = resampleMultinomial, numSteps = n_timesteps, numParticles = n_particles }
  x <- sampler $ pmmh mcmc_config smc_config (ldaPrior 2 vocabulary) (lda 2 vocabulary  (take n_words document))
  return (map (fromLDAParams . fst ) (concat x))
