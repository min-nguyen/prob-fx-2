{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module MonadBayes where

import BenchmarkUtil
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Traced.Basic
import Control.Monad.Bayes.Sampler.Lazy
import Control.Monad.Bayes.Weighted
import Sampler (liftIO)
import           Statistics.Distribution        ( logProbability )
import qualified Statistics.Distribution.Binomial  as SB
import Control.Monad
import Data.Maybe
import Data.Vector (Vector, fromList, toList)
import Data.List
import Control.Monad.Bayes.Inference.MCMC
import Control.Monad.Bayes.Population (multinomial, resampleMultinomial, runPopulation)
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Inference.PMMH
import Control.Monad.Bayes.Inference.RMSMC
import Data.List.Split (splitOn)
import Data.Bifunctor (second)

input_file :: String
input_file = "examples/benchmarks/params.txt"

output_file :: String
output_file = "examples/benchmarks/benchmarks-monad-bayes.csv"

runBenchmarks :: IO ()
runBenchmarks = do
  -- | Read input benchmark parameters
  content <- readFile input_file
  let removeComments :: [String] -> [String]
      removeComments = filter (\case []     -> False
                                     (x:xs) -> x /= '#')
  let args :: [[Int]]
      args = map (map read . splitOn ",") (removeComments (lines content))
  -- | Run benchmark programs on their corresponding parameters
  case args of
        (lr_range : hmm_range : lda_range : mh_range : smc_range : pmh_range : rmpf_range : _) -> do
            bench_LR_MonadBayes lr_range
            bench_HMM_MonadBayes hmm_range
            bench_LDA_MonadBayes lda_range
            bench_MH_MonadBayes mh_range
            bench_SMC_MonadBayes smc_range
            bench_PMH_MonadBayes pmh_range
            bench_RMPF_MonadBayes rmpf_range
        _   -> error "bad input file"

bench_LR_MonadBayes :: [Int] -> IO ()
bench_LR_MonadBayes lr_range = do
    let row_header = ("Num datapoints", lr_range)
    writeRow output_file row_header
    benchRow ("LinRegr-[ ]-SSMH-" ++ show fixed_mh_steps
              , liftIO . MonadBayes.mhLinRegr fixed_mh_steps) row_header output_file
    benchRow ("LinRegr-[ ]-MPF-" ++ show fixed_smc_particles
              , liftIO . MonadBayes.smcLinRegr fixed_smc_particles)  row_header output_file
    benchRow ("LinRegr-[ ]-PMH-" ++ show fixed_pmh_mhsteps ++ "-" ++ show fixed_pmh_particles
              , liftIO . MonadBayes.pmhLinRegr fixed_pmh_mhsteps fixed_pmh_particles) row_header  output_file
    benchRow ("LinRegr-[ ]-RMPF-" ++ show fixed_rmpf_particles ++ "-"  ++ show fixed_rmpf_mhsteps
              , liftIO . rmpfLinRegr fixed_rmpf_particles fixed_rmpf_mhsteps) row_header output_file

bench_HMM_MonadBayes :: [Int] -> IO ()
bench_HMM_MonadBayes hmm_range = do
    let row_header = ("Num nodes", hmm_range)
    writeRow output_file row_header
    benchRow ("HidMark-[ ]-SSMH-" ++ show fixed_mh_steps
              , liftIO . MonadBayes.mhHMM fixed_mh_steps) row_header output_file
    benchRow ("HidMark-[ ]-MPF-" ++ show fixed_mh_steps
              , liftIO . MonadBayes.smcHMM fixed_smc_particles) row_header output_file
    benchRow ("HidMark-[ ]-PMH-" ++ show fixed_pmh_mhsteps ++ "-" ++ show fixed_pmh_particles
              , liftIO . MonadBayes.pmhHMM fixed_pmh_mhsteps fixed_pmh_particles) row_header output_file
    benchRow ("HidMark-[ ]-RMPF" ++ show fixed_rmpf_particles ++ "-"  ++ show fixed_rmpf_mhsteps
              , liftIO . rmpfHMM fixed_rmpf_particles fixed_rmpf_mhsteps) row_header output_file

bench_LDA_MonadBayes :: [Int] -> IO ()
bench_LDA_MonadBayes lda_range = do
    let row_header = ("Num words", lda_range)
    writeRow output_file row_header
    -- | Set upper bound on executed args (to avoid early terminated benchmarks)
    let (lda_smc_bound, lda_pmh_bound, lda_rmpf_bound) = (5, 6, 4)
    benchRow ("LatDiri-[ ]-SSMH-" ++ show fixed_mh_steps
              , liftIO . MonadBayes.mhLDA fixed_mh_steps) row_header output_file
    benchRow ("LatDiri-[ ]-MPF-" ++ show fixed_mh_steps
              , liftIO . MonadBayes.smcLDA fixed_smc_particles)  (second (take lda_smc_bound) row_header) output_file
    benchRow ("LatDiri-[ ]-PMH-" ++ show fixed_pmh_mhsteps ++ "-" ++ show fixed_pmh_particles
              , liftIO . MonadBayes.pmhLDA fixed_pmh_mhsteps fixed_pmh_particles)  (second (take lda_pmh_bound) row_header) output_file
    benchRow ("LatDiri-[ ]-RMPF-" ++ show fixed_rmpf_particles ++ "-"  ++ show fixed_rmpf_mhsteps
              , liftIO . rmpfLDA fixed_rmpf_particles fixed_rmpf_mhsteps)  (second (take lda_rmpf_bound) row_header) output_file

bench_MH_MonadBayes :: [Int] -> IO ()
bench_MH_MonadBayes mh_range = do
    let row_header = ("Num SSMH steps", mh_range)
    writeRow output_file row_header
    benchRow ("SSMH-[ ]-LinRegr-" ++ show fixed_lr
              , liftIO . flip MonadBayes.mhLinRegr fixed_lr) row_header output_file
    benchRow ("SSMH-[ ]-HidMark-" ++ show fixed_hmm
              , liftIO . flip MonadBayes.mhHMM fixed_hmm) row_header output_file
    benchRow ("SSMH-[ ]-LatDiri-" ++ show fixed_lda
              , liftIO . flip MonadBayes.mhLDA fixed_lda) row_header output_file

bench_SMC_MonadBayes :: [Int] -> IO ()
bench_SMC_MonadBayes smc_range = do
    let row_header = ("Num MPF particles", smc_range)
    writeRow output_file row_header
    -- | Set upper bound on executed args (to avoid early terminated benchmarks)
    let (smc_lr_bound, smc_hmm_bound, smc_lda_bound) = (7, 6, 7)
    benchRow ("MPF-[ ]-LinRegr-" ++ show fixed_lr
              , liftIO . flip MonadBayes.smcLinRegr fixed_lr) (second (take smc_lr_bound) row_header) output_file
    benchRow ("MPF-[ ]-HidMark-" ++ show fixed_hmm
              , liftIO . flip MonadBayes.smcHMM fixed_hmm) (second (take smc_hmm_bound) row_header) output_file
    benchRow ("MPF-[ ]-LatDiri-" ++ show fixed_lda
              , liftIO . flip MonadBayes.smcLDA fixed_lda) (second (take smc_lda_bound) row_header) output_file

bench_PMH_MonadBayes :: [Int] -> IO ()
bench_PMH_MonadBayes pmh_range = do
    let row_header = ("Num PMH particles", pmh_range)
    writeRow output_file row_header
    -- | Set upper bound on executed args (to avoid early terminated benchmarks)
    let (pmh_lr_bound, pmh_hmm_bound, pmh_lda_bound) = (5, 5, 4)
    benchRow ("PMH-" ++ show fixed_pmh_mhsteps ++ "-[ ]-LinRegr-" ++ show fixed_lr
              , liftIO . flip (MonadBayes.pmhLinRegr fixed_pmh_mhsteps) fixed_lr) (second (take pmh_lr_bound) row_header) output_file
    benchRow ("PMH-" ++ show fixed_pmh_mhsteps ++ "-[ ]-HidMark-" ++ show fixed_hmm
              , liftIO . flip (MonadBayes.pmhHMM fixed_pmh_mhsteps) fixed_hmm) (second (take pmh_hmm_bound) row_header) output_file
    benchRow ("PMH-" ++ show fixed_pmh_mhsteps ++ "-[ ]-LatDiri-" ++ show fixed_lda
              , liftIO . flip (MonadBayes.pmhLDA fixed_pmh_mhsteps) fixed_lda) (second (take pmh_lda_bound) row_header) output_file

bench_RMPF_MonadBayes :: [Int] -> IO ()
bench_RMPF_MonadBayes rmpf_range = do
    let row_header = ("Num RMPF mh steps", rmpf_range)
    writeRow output_file row_header
    -- | Set upper bound on executed args (to avoid early terminated benchmarks)
    let rmpf_lda_bound = 6
    benchRow ("RMPF-" ++ show fixed_rmpf_particles ++ "-[ ]-LinRegr-" ++ show fixed_lr
              , liftIO . flip (rmpfLinRegr fixed_rmpf_particles) fixed_lr) row_header output_file
    benchRow ("RMPF-" ++ show fixed_rmpf_particles ++ "-[ ]-HidMark-" ++ show fixed_hmm
              , liftIO . flip (rmpfHMM fixed_rmpf_particles) fixed_hmm) row_header output_file
    benchRow ("RMPF-" ++ show fixed_rmpf_particles ++ "-[ ]-LatDiri-" ++ show fixed_lda
              , liftIO . flip (rmpfLDA fixed_rmpf_particles) fixed_lda) (second (take rmpf_lda_bound) row_header)  output_file

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

pmhLinRegr :: Int -> Int -> Int -> IO [(Double, Double, Double)]
pmhLinRegr mh_steps n_particles n_datapoints = do
  let n_timesteps = n_datapoints
  let mcmc_config  = MCMCConfig { proposal = SingleSiteMH, numMCMCSteps = mh_steps, numBurnIn = 0 }
      smc_config   = SMCConfig { resampler = resampleMultinomial, numSteps = n_timesteps, numParticles = n_particles }
  x <- sampler $ pmmh mcmc_config smc_config linRegrPrior (linRegr (linRegrData n_datapoints))
  return (map (fromLinRegrParams . fst) (concat x))

rmpfLinRegr :: Int -> Int -> Int -> IO [(Double, Double, Double)]
rmpfLinRegr n_particles mh_steps  n_datapoints = do
  let n_timesteps = n_datapoints
  let mcmc_config  = MCMCConfig { proposal = SingleSiteMH, numMCMCSteps = mh_steps, numBurnIn = 0 }
      smc_config   = SMCConfig { resampler = resampleMultinomial, numSteps = n_timesteps, numParticles = n_particles }
  x <- sampler . runPopulation $ rmsmc mcmc_config smc_config (linRegrPrior >>= linRegr (linRegrData n_datapoints))
  return (map (fromLinRegrParams . fst) x)

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

pmhHMM :: Int -> Int -> Int -> IO [(Double, Double)]
pmhHMM mh_steps n_particles n_datapoints = do
  let n_timesteps = n_datapoints
      mcmc_config  = MCMCConfig { proposal = SingleSiteMH, numMCMCSteps = mh_steps, numBurnIn = 0 }
      smc_config   = SMCConfig { resampler = resampleMultinomial, numSteps = n_timesteps, numParticles = n_particles }
  ys <- simHMM  n_datapoints
  x <- sampler $ pmmh mcmc_config smc_config hmmPrior (hmm 0 ( ys))
  return  (map (fromHMMParams . fst) (concat x))

rmpfHMM :: Int -> Int -> Int -> IO [(Double, Double)]
rmpfHMM n_particles mh_steps  n_datapoints = do
  let n_timesteps = n_datapoints
      mcmc_config  = MCMCConfig { proposal = SingleSiteMH, numMCMCSteps = mh_steps, numBurnIn = 0 }
      smc_config   = SMCConfig { resampler = resampleMultinomial, numSteps = n_timesteps, numParticles = n_particles }
  ys <- simHMM  n_datapoints
  x <- sampler . runPopulation $ rmsmc mcmc_config smc_config  (hmmPrior >>= hmm 0 ( ys))
  return  (map (fromHMMParams . fst) x)

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

pmhLDA :: Int -> Int -> Int -> IO [([Double], [[Double]])]
pmhLDA mh_steps n_particles n_words = do
  let n_timesteps = n_words
      mcmc_config  = MCMCConfig { proposal = SingleSiteMH, numMCMCSteps = mh_steps, numBurnIn = 0 }
      smc_config   = SMCConfig { resampler = resampleMultinomial, numSteps = n_timesteps, numParticles = n_particles }
  x <- sampler $ pmmh mcmc_config smc_config (ldaPrior 2 vocabulary) (lda 2 vocabulary  (take n_words document))
  return (map (fromLDAParams . fst ) (concat x))

rmpfLDA :: Int -> Int -> Int -> IO [([Double], [[Double]])]
rmpfLDA n_particles mh_steps  n_words = do
  let n_timesteps = n_words
      mcmc_config  = MCMCConfig { proposal = SingleSiteMH, numMCMCSteps = mh_steps, numBurnIn = 0 }
      smc_config   = SMCConfig { resampler = resampleMultinomial, numSteps = n_timesteps, numParticles = n_particles }
  x <- sampler . runPopulation $ rmsmc mcmc_config smc_config (ldaPrior 2 vocabulary >>= lda 2 vocabulary  (take n_words document))
  return (map (fromLDAParams . fst) x)
