{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use camelCase" #-}
module Main where

import LinRegr
import HMM
import LDA

import Criterion.Main
import Criterion.Types
import Control.Monad
import Data.List
import Sampler
import Data.List.Split
import Statistics.Types
import Control.DeepSeq
import Criterion (benchmark')

{- | Benchmarking utility
-}
fixed_input_file :: String
fixed_input_file = "examples/benchmarks/benchmark_params.txt"

fixed_output_file :: String
fixed_output_file = "examples/benchmarks/benchmarks.csv"

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
  -- | List of run-times
  -> IO ()
benchRow (prog_name, prog) (_, params) = do
  putStrLn ("Running " ++ prog_name ++ " over " ++ show params)
  -- Run program over varying parameter values and write e.g. "LinRegr-MH100, 0.23, 0.87, 1.23, 1.78, 2.45"
  means <- mapM (benchMean . sampleIOFixed . prog) params
  writeRow fixed_output_file (prog_name, means)

{- | Varying over dataset size
-}
fixed_simulations :: Int
fixed_simulations = 1
fixed_lw_steps :: Int
fixed_lw_steps = 100
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

bench_LR :: [Int] -> IO ()
bench_LR args = do
    let row_header = ("Dataset size", args)
    writeRow fixed_output_file row_header
    benchRow ("LR-[ ]-SIM-" ++ show fixed_simulations
              , simLinRegr) row_header
    benchRow ("LR-[ ]-LW-" ++ show fixed_lw_steps
              , lwLinRegr fixed_lw_steps) row_header
    benchRow ("LR-[ ]-MH-" ++ show fixed_mh_steps
              , mhLinRegr fixed_mh_steps) row_header
    benchRow ("LR-[ ]-SMC-" ++ show fixed_smc_particles
              , smcLinRegr fixed_smc_particles) row_header
    benchRow ("LR-[ ]-RMSMC-" ++ show fixed_rmsmc_particles ++ "-" ++ show fixed_rmsmc_mhsteps
              , rmsmcLinRegr fixed_rmsmc_particles fixed_rmsmc_mhsteps) row_header
    benchRow ("LR-[ ]-PMMH-" ++ show fixed_pmmh_mhsteps ++ "-" ++ show fixed_pmmh_particles
              , pmmhLinRegr fixed_pmmh_mhsteps fixed_pmmh_particles) row_header
    benchRow ("LR-[ ]-BBVI-" ++ show fixed_bbvi_steps ++ "-" ++ show fixed_bbvi_samples
              , bbviLinRegr fixed_bbvi_steps fixed_bbvi_samples) row_header

bench_HMM :: [Int] -> IO ()
bench_HMM args = do
    let row_header = ("Dataset size", args)
    writeRow fixed_output_file row_header
    benchRow ("HMM-[ ]-SIM-" ++ show fixed_simulations
              , simHMM) row_header
    benchRow ("HMM-[ ]-LW-" ++ show fixed_lw_steps
              , lwHMM fixed_lw_steps) row_header
    benchRow ("HMM-[ ]-MH-" ++ show fixed_mh_steps
              , mhHMM fixed_mh_steps) row_header
    benchRow ("HMM-[ ]-SMC-" ++ show fixed_mh_steps
              , smcHMM fixed_smc_particles) row_header
    benchRow ("HMM-[ ]-RMSMC-" ++ show fixed_rmsmc_particles ++ "-" ++ show fixed_rmsmc_mhsteps
              , rmsmcHMM fixed_rmsmc_particles fixed_rmsmc_mhsteps) row_header
    benchRow ("HMM-[ ]-PMMH-" ++ show fixed_pmmh_mhsteps ++ "-" ++ show fixed_pmmh_particles
              , pmmhHMM fixed_pmmh_mhsteps fixed_pmmh_particles) row_header
    benchRow ("HMM-[ ]-BBVI-" ++ show fixed_bbvi_steps ++ "-" ++ show fixed_bbvi_samples
              , bbviHMM fixed_bbvi_steps fixed_bbvi_samples) row_header

bench_LDA :: [Int] -> IO ()
bench_LDA args = do
    let row_header = ("Dataset size", args)
    writeRow fixed_output_file row_header
    -- benchRow ("LDA-[ ]-SIM-" ++ show fixed_simulations
    --           , simLDA) row_header
    benchRow ("LDA-[ ]-LW-" ++ show fixed_lw_steps
              , lwLDA fixed_lw_steps) row_header
    benchRow ("LDA-[ ]-MH-" ++ show fixed_mh_steps
              , mhLDA fixed_mh_steps) row_header
    benchRow ("LDA-[ ]-SMC-" ++ show fixed_mh_steps
              , smcLDA fixed_smc_particles) row_header
    benchRow ("LDA-[ ]-RMSMC-" ++ show fixed_rmsmc_particles ++ "-" ++ show fixed_rmsmc_mhsteps
              , rmsmcLDA fixed_rmsmc_particles fixed_rmsmc_mhsteps) row_header
    benchRow ("LDA-[ ]-PMMH-" ++ show fixed_pmmh_mhsteps ++ "-" ++ show fixed_pmmh_particles
              , pmmhLDA fixed_pmmh_mhsteps fixed_pmmh_particles) row_header
    benchRow ("LDA-[ ]-BBVI-" ++ show fixed_bbvi_steps ++ "-" ++ show fixed_bbvi_samples
              , bbviLDA fixed_bbvi_steps fixed_bbvi_samples) row_header

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

bench_SIM :: [Int] -> IO ()
bench_SIM args = do
    let row_header = ("Number of simulations", args)
    writeRow fixed_output_file row_header
    -- benchRow ("SIM-[ ]-LR-" ++ show fixed_lr_datasize_inf
    --           , flip replicateM (simLinRegr fixed_lr_datasize_inf)) row_header
    -- benchRow ("SIM-[ ]-HMM-" ++ show fixed_hmm_datasize_inf
    --           , flip replicateM (simHMM fixed_hmm_datasize_inf)) row_header
    benchRow ("SIM-[ ]-LDA-" ++ show fixed_lda_datasize_inf
              , flip replicateM (simLDA fixed_lda_datasize_inf)) row_header

bench_LW :: [Int] -> IO ()
bench_LW args = do
    let row_header = ("Number of LW iterations", args)
    writeRow fixed_output_file row_header
    -- benchRow ("LW-[ ]-LR-" ++ show fixed_lr_datasize_inf
    --           , flip lwLinRegr fixed_lr_datasize_inf) row_header
    -- benchRow ("LW-[ ]-HMM-" ++ show fixed_hmm_datasize_inf
    --           , flip lwHMM fixed_hmm_datasize_inf) row_header
    benchRow ("LW-[ ]-LDA-" ++ show fixed_lda_datasize_inf
              , flip lwLDA fixed_lda_datasize_inf) row_header

bench_MH :: [Int] -> IO ()
bench_MH args = do
    let row_header = ("Number of MH steps", args)
    writeRow fixed_output_file row_header
    -- benchRow ("MH-[ ]-LR-" ++ show fixed_lr_datasize_inf
    --           , flip mhLinRegr fixed_lr_datasize_inf) row_header
    -- benchRow ("MH-[ ]-HMM-" ++ show fixed_hmm_datasize_inf
    --           , flip mhHMM fixed_hmm_datasize_inf) row_header
    benchRow ("MH-[ ]-LDA-" ++ show fixed_lda_datasize_inf
              , flip mhLDA fixed_lda_datasize_inf) row_header

bench_SMC :: [Int] -> IO ()
bench_SMC args = do
    let row_header = ("Number of SMC particles", args)
    writeRow fixed_output_file row_header
    -- benchRow ("SMC-[ ]-LR-" ++ show fixed_lr_datasize_inf
    --           , flip smcLinRegr fixed_lr_datasize_inf) row_header
    -- benchRow ("SMC-[ ]-HMM-" ++ show fixed_hmm_datasize_inf
    --           , flip smcHMM fixed_hmm_datasize_inf) row_header
    benchRow ("SMC-[ ]-LDA-" ++ show fixed_lda_datasize_inf
              , flip smcLinRegr fixed_lda_datasize_inf) row_header

bench_RMSMC :: [Int] -> IO ()
bench_RMSMC args = do
    let row_header = ("Number of RMSMC rejuvenation steps", args)
    writeRow fixed_output_file row_header
    -- benchRow ("RMSMC-" ++ show fixed_rmsmc_particles_inf ++ "-[ ]-LR-" ++ show fixed_lr_datasize_inf
    --           , flip (rmsmcLinRegr fixed_rmsmc_particles_inf) fixed_lr_datasize_inf) row_header
    -- benchRow ("RMSMC-" ++ show fixed_rmsmc_particles_inf ++ "-[ ]-HMM-" ++ show fixed_hmm_datasize_inf
    --           , flip (rmsmcHMM fixed_rmsmc_particles_inf) fixed_hmm_datasize_inf) row_header
    benchRow ("RMSMC-" ++ show fixed_rmsmc_particles_inf ++ "-[ ]-LDA-" ++ show fixed_lda_datasize_inf
              , flip (rmsmcLDA fixed_rmsmc_particles_inf) fixed_lda_datasize_inf) row_header

bench_PMMH :: [Int] -> IO ()
bench_PMMH args = do
    let row_header = ("Number of PMMH particles", args)
    writeRow fixed_output_file row_header
    -- benchRow ("PMMH-" ++ show fixed_pmmh_mhsteps_inf ++ "-[ ]-LR-" ++ show fixed_lr_datasize_inf
    --           , flip (pmmhLinRegr fixed_pmmh_mhsteps_inf) fixed_lr_datasize_inf) row_header
    -- benchRow ("PMMH-" ++ show fixed_pmmh_mhsteps_inf ++ "-[ ]-HMM-" ++ show fixed_hmm_datasize_inf
    --           , flip (pmmhHMM fixed_pmmh_mhsteps_inf) fixed_hmm_datasize_inf) row_header
    benchRow ("PMMH-" ++ show fixed_pmmh_mhsteps_inf ++ "-[ ]-LDA-" ++ show fixed_lda_datasize_inf
              , flip (pmmhLDA fixed_pmmh_mhsteps_inf) fixed_lda_datasize_inf) row_header

bench_BBVI :: [Int] -> IO ()
bench_BBVI args = do
    let row_header = ("Number of BBVI steps", args)
    writeRow fixed_output_file row_header
    -- benchRow ("BBVI-[ ]-" ++ show fixed_bbvi_samples_inf ++ "-LR-" ++ show fixed_lr_datasize_inf
    --           , flip (bbviLinRegr fixed_bbvi_samples_inf) fixed_lr_datasize_inf) row_header
    -- benchRow ("BBVI-[ ]-" ++ show fixed_bbvi_samples_inf ++ "-HMM-" ++ show fixed_hmm_datasize_inf
    --           , flip (bbviHMM fixed_bbvi_samples_inf) fixed_hmm_datasize_inf) row_header
    benchRow ("BBVI-[ ]-" ++ show fixed_bbvi_samples_inf ++ "-LDA-" ++ show fixed_hmm_datasize_inf
              , flip (bbviLDA fixed_bbvi_samples_inf) fixed_lda_datasize_inf) row_header

runBenchmarks :: IO ()
runBenchmarks = do
  -- | Read input benchmark parameters
  content <- readFile fixed_input_file
  let removeComments :: [String] -> [String]
      removeComments = filter (\case []     -> False
                                     (x:xs) -> x /= '#')
  let args :: [[Int]]
      args = map (map read . splitOn ",") (removeComments (lines content))
  -- | Run benchmark programs on their corresponding parameters
  case args of
        [lr, hmm, lda, sim, lw, mh, smc, rmsmc, pmmh, bbvi] -> do
          -- bench_LR lr
          -- bench_HMM hmm
          bench_LDA lda
          -- bench_SIM sim
          bench_LW lw
          bench_MH mh
          bench_SMC smc
          bench_RMSMC rmsmc
          bench_PMMH pmmh
          bench_BBVI bbvi
        _   -> error "bad input file"

main :: IO ()
main = runBenchmarks
