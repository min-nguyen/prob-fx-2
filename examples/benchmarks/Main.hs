{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where

import LinRegr
import HMM
import LDA
import VIExamples
import Criterion.Main
import Criterion.Types
import Control.Monad
import Data.List
import Sampler
import Data.List.Split
import Statistics.Types
import Control.DeepSeq
import Criterion (benchmark')
import qualified MonadBayes

{- | Benchmarking utility
-}
input_file :: String
input_file = "examples/benchmarks/benchmark_params.txt"

output_file :: String
output_file = "examples/benchmarks/benchmarks-prob-fx.csv"

appendFileLn :: String -> String -> IO ()
appendFileLn file_name = appendFile file_name . (++ "\n")

writeRow :: Show a => String -> (String, [a]) -> IO ()
writeRow file_name (label, values) = appendFileLn file_name (intercalate "," (label : map show values))

benchMean :: NFData a => IO a -> IO Double
benchMean prog = do
  report <- benchmark' (nfIO prog)
  let analysis = reportAnalysis report
      estMean  = estPoint (anMean analysis)
  return estMean

benchRow :: NFData a
  -- | (Program name, program)
  => (String, Int -> Sampler a)
  -- | (Independent variable name, values)
  -> (String, [Int])
  -- | Output file
  -> String
  -> IO ()
benchRow  (prog_name, prog) (_, params) ofile = do
  putStrLn ("Running " ++ prog_name ++ " over " ++ show params)
  -- Run program over varying parameter values and write e.g. "LinRegr-MH100, 0.23, 0.87, 1.23, 1.78, 2.45"
  means <- mapM (benchMean . sampleIOFixed . prog) params
  writeRow ofile (prog_name, means)

{- | Varying over dataset size
-}
fixed_mh_steps :: Int
fixed_mh_steps = 100
fixed_smc_particles :: Int
fixed_smc_particles = 100
fixed_rmsmc_particles, fixed_rmsmc_mhsteps :: Int
fixed_rmsmc_particles = 10
fixed_rmsmc_mhsteps   = 1
fixed_pmmh_mhsteps, fixed_pmmh_particles :: Int
fixed_pmmh_mhsteps   = 50
fixed_pmmh_particles = 10
fixed_bbvi_steps, fixed_bbvi_samples :: Int
fixed_bbvi_steps = 50
fixed_bbvi_samples = 10
fixed_invi_steps, fixed_invi_samples :: Int
fixed_invi_steps = 50
fixed_invi_samples = 10

bench_LR :: [Int] -> IO ()
bench_LR args = do
    let row_header = ("Dataset size", args)
    writeRow output_file row_header
    benchRow ("LR-[ ]-MH-" ++ show fixed_mh_steps
              , mhLinRegr fixed_mh_steps) row_header output_file
    benchRow ("LR-[ ]-SMC-" ++ show fixed_smc_particles
              , smcLinRegr fixed_smc_particles)  row_header output_file
    benchRow ("LR-[ ]-PMMH-" ++ show fixed_pmmh_mhsteps ++ "-" ++ show fixed_pmmh_particles
              , pmmhLinRegr fixed_pmmh_mhsteps fixed_pmmh_particles) row_header output_file
    benchRow ("LR-[ ]-BBVI-" ++ show fixed_bbvi_steps ++ "-" ++ show fixed_bbvi_samples
              , bbviLinRegr fixed_bbvi_steps fixed_bbvi_samples) row_header output_file
    -- benchRow ("LR-[ ]-RMSMC-" ++ show fixed_rmsmc_particles ++ "-" ++ show fixed_rmsmc_mhsteps
    --           , rmsmcLinRegr fixed_rmsmc_particles fixed_rmsmc_mhsteps) row_header output_file

bench_HMM :: [Int] -> IO ()
bench_HMM args = do
    let row_header = ("Dataset size", args)
    writeRow output_file row_header
    benchRow ("HMM-[ ]-MH-" ++ show fixed_mh_steps
              , mhHMM fixed_mh_steps) row_header output_file
    benchRow ("HMM-[ ]-SMC-" ++ show fixed_mh_steps
              , smcHMM fixed_smc_particles) row_header output_file
    benchRow ("HMM-[ ]-PMMH-" ++ show fixed_pmmh_mhsteps ++ "-" ++ show fixed_pmmh_particles
              , pmmhHMM fixed_pmmh_mhsteps fixed_pmmh_particles) row_header output_file
    benchRow ("HMM-[ ]-BBVI-" ++ show fixed_bbvi_steps ++ "-" ++ show fixed_bbvi_samples
              , bbviHMM fixed_bbvi_steps fixed_bbvi_samples) row_header output_file
    -- benchRow ("HMM-[ ]-RMSMC-" ++ show fixed_rmsmc_particles ++ "-" ++ show fixed_rmsmc_mhsteps
    --           , rmsmcHMM fixed_rmsmc_particles fixed_rmsmc_mhsteps) row_header output_file

bench_LDA :: [Int] -> IO ()
bench_LDA args = do
    let row_header = ("Dataset size", args)
    writeRow output_file row_header
    benchRow ("LDA-[ ]-MH-" ++ show fixed_mh_steps
              , mhLDA fixed_mh_steps) row_header output_file
    benchRow ("LDA-[ ]-SMC-" ++ show fixed_mh_steps
              , smcLDA fixed_smc_particles) row_header output_file
    benchRow ("LDA-[ ]-PMMH-" ++ show fixed_pmmh_mhsteps ++ "-" ++ show fixed_pmmh_particles
              , pmmhLDA fixed_pmmh_mhsteps fixed_pmmh_particles) row_header output_file
    benchRow ("LDA-[ ]-BBVI-" ++ show fixed_bbvi_steps ++ "-" ++ show fixed_bbvi_samples
              , bbviLDA fixed_bbvi_steps fixed_bbvi_samples) row_header output_file
    -- benchRow ("LDA-[ ]-RMSMC-" ++ show fixed_rmsmc_particles ++ "-" ++ show fixed_rmsmc_mhsteps
    --           , rmsmcLDA fixed_rmsmc_particles fixed_rmsmc_mhsteps) row_header output_file

{- | Varying over inference parameters
-}
fixed_lr_datasize_inf :: Int
fixed_lr_datasize_inf = 50
fixed_hmm_datasize_inf :: Int
fixed_hmm_datasize_inf = 50
fixed_lda_datasize_inf :: Int
fixed_lda_datasize_inf = 50
fixed_rmsmc_particles_inf :: Int
fixed_rmsmc_particles_inf = 10
fixed_pmmh_mhsteps_inf :: Int
fixed_pmmh_mhsteps_inf = 50
fixed_bbvi_samples_inf :: Int
fixed_bbvi_samples_inf = 10

bench_MH :: [Int] -> IO ()
bench_MH args = do
    let row_header = ("Number of MH steps", args)
    writeRow output_file row_header
    benchRow ("MH-[ ]-LR-" ++ show fixed_lr_datasize_inf
              , flip mhLinRegr fixed_lr_datasize_inf) row_header output_file
    benchRow ("MH-[ ]-HMM-" ++ show fixed_hmm_datasize_inf
              , flip mhHMM fixed_hmm_datasize_inf) row_header output_file
    benchRow ("MH-[ ]-LDA-" ++ show fixed_lda_datasize_inf
              , flip mhLDA fixed_lda_datasize_inf) row_header output_file

bench_SMC :: [Int] -> IO ()
bench_SMC args = do
    let row_header = ("Number of SMC particles", args)
    writeRow output_file row_header
    benchRow ("SMC-[ ]-LR-" ++ show fixed_lr_datasize_inf
              , flip smcLinRegr fixed_lr_datasize_inf) row_header output_file
    benchRow ("SMC-[ ]-HMM-" ++ show fixed_hmm_datasize_inf
              , flip smcHMM fixed_hmm_datasize_inf) row_header output_file
    benchRow ("SMC-[ ]-LDA-" ++ show fixed_lda_datasize_inf
              , flip smcLinRegr fixed_lda_datasize_inf) row_header output_file

bench_PMMH :: [Int] -> IO ()
bench_PMMH args = do
    let row_header = ("Number of PMMH particles", args)
    writeRow output_file row_header
    benchRow ("PMMH-" ++ show fixed_pmmh_mhsteps_inf ++ "-[ ]-LR-" ++ show fixed_lr_datasize_inf
              , flip (pmmhLinRegr fixed_pmmh_mhsteps_inf) fixed_lr_datasize_inf) row_header output_file
    benchRow ("PMMH-" ++ show fixed_pmmh_mhsteps_inf ++ "-[ ]-HMM-" ++ show fixed_hmm_datasize_inf
              , flip (pmmhHMM fixed_pmmh_mhsteps_inf) fixed_hmm_datasize_inf) row_header output_file
    benchRow ("PMMH-" ++ show fixed_pmmh_mhsteps_inf ++ "-[ ]-LDA-" ++ show fixed_lda_datasize_inf
              , flip (pmmhLDA fixed_pmmh_mhsteps_inf) fixed_lda_datasize_inf) row_header output_file

bench_BBVI :: [Int] -> IO ()
bench_BBVI args = do
    let row_header = ("Number of BBVI steps", args)
    writeRow output_file row_header
    benchRow ("BBVI-[ ]-" ++ show fixed_bbvi_samples_inf ++ "-LR-" ++ show fixed_lr_datasize_inf
              , flip (bbviLinRegr fixed_bbvi_samples_inf) fixed_lr_datasize_inf) row_header output_file
    benchRow ("BBVI-[ ]-" ++ show fixed_bbvi_samples_inf ++ "-HMM-" ++ show fixed_hmm_datasize_inf
              , flip (bbviHMM fixed_bbvi_samples_inf) fixed_hmm_datasize_inf) row_header output_file
    benchRow ("BBVI-[ ]-" ++ show fixed_bbvi_samples_inf ++ "-LDA-" ++ show fixed_hmm_datasize_inf
              , flip (bbviLDA fixed_bbvi_samples_inf) fixed_lda_datasize_inf) row_header output_file

-- bench_RMSMC :: [Int] -> IO ()
-- bench_RMSMC args = do
--     let row_header = ("Number of RMSMC rejuvenation steps", args)
--     writeRow output_file row_header
--     benchRow ("RMSMC-" ++ show fixed_rmsmc_particles_inf ++ "-[ ]-LR-" ++ show fixed_lr_datasize_inf
--               , flip (rmsmcLinRegr fixed_rmsmc_particles_inf) fixed_lr_datasize_inf) row_header output_file
--     benchRow ("RMSMC-" ++ show fixed_rmsmc_particles_inf ++ "-[ ]-HMM-" ++ show fixed_hmm_datasize_inf
--               , flip (rmsmcHMM fixed_rmsmc_particles_inf) fixed_hmm_datasize_inf) row_header output_file
--     benchRow ("RMSMC-" ++ show fixed_rmsmc_particles_inf ++ "-[ ]-LDA-" ++ show fixed_lda_datasize_inf
--               , flip (rmsmcLDA fixed_rmsmc_particles_inf) fixed_lda_datasize_inf) row_header output_file

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
        [lr, hmm, lda, mh, smc, rmsmc, pmmh, bbvi] -> do
          bench_LR lr
          bench_HMM hmm
          bench_LDA lda
          bench_MH mh
          bench_SMC smc
          bench_PMMH pmmh
          bench_BBVI bbvi
          -- bench_RMSMC rmsmc
        _   -> error "bad input file"

{-
    MonadBayes Benchmarks
-}

output_file_MonadBayes :: String
output_file_MonadBayes = "examples/benchmarks/benchmarks-monad-bayes.csv"

bench_LR_MonadBayes :: [Int] -> IO ()
bench_LR_MonadBayes args = do
    let row_header = ("Dataset size", args)
    writeRow output_file_MonadBayes row_header
    benchRow ("LR-[ ]-MH-" ++ show fixed_mh_steps
              , liftIO . MonadBayes.mhLinRegr fixed_mh_steps) row_header output_file_MonadBayes
    benchRow ("LR-[ ]-SMC-" ++ show fixed_smc_particles
              , liftIO . MonadBayes.smcLinRegr fixed_smc_particles)  row_header output_file_MonadBayes
    benchRow ("LR-[ ]-PMMH-" ++ show fixed_pmmh_mhsteps ++ "-" ++ show fixed_pmmh_particles
              , liftIO . MonadBayes.pmmhLinRegr fixed_pmmh_mhsteps fixed_pmmh_particles) row_header  output_file_MonadBayes

bench_HMM_MonadBayes :: [Int] -> IO ()
bench_HMM_MonadBayes args = do
    let row_header = ("Dataset size", args)
    writeRow output_file_MonadBayes row_header
    benchRow ("HMM-[ ]-MH-" ++ show fixed_mh_steps
              , liftIO . MonadBayes.mhHMM fixed_mh_steps) row_header output_file_MonadBayes
    benchRow ("HMM-[ ]-SMC-" ++ show fixed_mh_steps
              , liftIO . MonadBayes.smcHMM fixed_smc_particles) row_header output_file_MonadBayes
    benchRow ("HMM-[ ]-PMMH-" ++ show fixed_pmmh_mhsteps ++ "-" ++ show fixed_pmmh_particles
              , liftIO . MonadBayes.pmmhHMM fixed_pmmh_mhsteps fixed_pmmh_particles) row_header output_file_MonadBayes

bench_LDA_MonadBayes :: [Int] -> IO ()
bench_LDA_MonadBayes args = do
    let row_header = ("Dataset size", args)
    writeRow output_file_MonadBayes row_header
    benchRow ("LDA-[ ]-MH-" ++ show fixed_mh_steps
              , liftIO . MonadBayes.mhLDA fixed_mh_steps) row_header output_file_MonadBayes
    benchRow ("LDA-[ ]-SMC-" ++ show fixed_mh_steps
              , liftIO . MonadBayes.smcLDA fixed_smc_particles) row_header output_file
    benchRow ("LDA-[ ]-PMMH-" ++ show fixed_pmmh_mhsteps ++ "-" ++ show fixed_pmmh_particles
              , liftIO . MonadBayes.pmmhLDA fixed_pmmh_mhsteps fixed_pmmh_particles) row_header output_file_MonadBayes

bench_MH_MonadBayes :: [Int] -> IO ()
bench_MH_MonadBayes args = do
    let row_header = ("Number of MH steps", args)
    writeRow output_file_MonadBayes row_header
    benchRow ("MH-[ ]-LR-" ++ show fixed_lr_datasize_inf
              , liftIO . flip MonadBayes.mhLinRegr fixed_lr_datasize_inf) row_header output_file_MonadBayes
    benchRow ("MH-[ ]-HMM-" ++ show fixed_hmm_datasize_inf
              , liftIO . flip MonadBayes.mhHMM fixed_hmm_datasize_inf) row_header output_file_MonadBayes
    benchRow ("MH-[ ]-LDA-" ++ show fixed_lda_datasize_inf
              , liftIO . flip MonadBayes.mhLDA fixed_lda_datasize_inf) row_header output_file_MonadBayes

bench_SMC_MonadBayes :: [Int] -> IO ()
bench_SMC_MonadBayes args = do
    let row_header = ("Number of SMC particles", args)
    writeRow output_file_MonadBayes row_header
    -- benchRow ("SMC-[ ]-LR-" ++ show fixed_lr_datasize_inf
    --           , liftIO . flip MonadBayes.smcLinRegr fixed_lr_datasize_inf) row_header output_file_MonadBayes
    -- benchRow ("SMC-[ ]-HMM-" ++ show fixed_hmm_datasize_inf
    --           , liftIO . flip MonadBayes.smcHMM fixed_hmm_datasize_inf) row_header output_file_MonadBayes
    benchRow ("SMC-[ ]-LDA-" ++ show fixed_lda_datasize_inf
              , liftIO . flip MonadBayes.smcLinRegr fixed_lda_datasize_inf) row_header output_file_MonadBayes

bench_PMMH_MonadBayes :: [Int] -> IO ()
bench_PMMH_MonadBayes args = do
    let row_header = ("Number of PMMH particles", args)
    writeRow output_file_MonadBayes row_header
    -- benchRow ("PMMH-" ++ show fixed_pmmh_mhsteps_inf ++ "-[ ]-LR-" ++ show fixed_lr_datasize_inf
    --           , liftIO . flip (MonadBayes.pmmhLinRegr fixed_pmmh_mhsteps_inf) fixed_lr_datasize_inf) row_header output_file_MonadBayes
    benchRow ("PMMH-" ++ show fixed_pmmh_mhsteps_inf ++ "-[ ]-HMM-" ++ show fixed_hmm_datasize_inf
              , liftIO . flip (MonadBayes.pmmhHMM fixed_pmmh_mhsteps_inf) fixed_hmm_datasize_inf) row_header output_file_MonadBayes
    benchRow ("PMMH-" ++ show fixed_pmmh_mhsteps_inf ++ "-[ ]-LDA-" ++ show fixed_lda_datasize_inf
              , liftIO . flip (MonadBayes.pmmhLDA fixed_pmmh_mhsteps_inf) fixed_lda_datasize_inf) row_header output_file_MonadBayes

runBenchmarks_MonadBayes :: IO ()
runBenchmarks_MonadBayes = do
  -- | Read input benchmark parameters
  content <- readFile input_file
  let removeComments :: [String] -> [String]
      removeComments = filter (\case []     -> False
                                     (x:xs) -> x /= '#')
  let args :: [[Int]]
      args = map (map read . splitOn ",") (removeComments (lines content))
  -- | Run benchmark programs on their corresponding parameters
  case args of
        [lr, hmm, lda, mh, smc, rmsmc, pmmh, bbvi] -> do
          -- bench_LR_MonadBayes lr
          -- bench_HMM_MonadBayes hmm
          -- bench_LDA_MonadBayes lda
          -- bench_MH_MonadBayes mh
          -- bench_SMC_MonadBayes smc
          bench_PMMH_MonadBayes pmmh
        _   -> error "bad input file"

main :: IO ()
main =  -- runBenchmarks
 runBenchmarks_MonadBayes
