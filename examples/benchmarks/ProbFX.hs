{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module ProbFX where

import BenchmarkUtil
import LinRegr
import HMM
import LDA
import VIExamples
import Data.List.Split

{- | Benchmarking utility
-}
input_file :: String
input_file = "examples/benchmarks/params-prob-fx.txt"

output_file :: String
output_file = "examples/benchmarks/benchmarks-prob-fx.csv"

{- | Varying over dataset size
-}

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
    benchRow ("BBVI-[ ]-" ++ show fixed_bbvi_samples_inf ++ "-LDA-" ++ show fixed_lda_datasize_inf
              , flip (bbviLDA fixed_bbvi_samples_inf) fixed_lda_datasize_inf) row_header output_file

bench_RMSMC :: [Int] -> IO ()
bench_RMSMC args = do
    let row_header = ("Number of RMSMC rejuvenation steps", args)
    writeRow output_file row_header
    benchRow ("RMSMC-" ++ show fixed_rmsmc_particles_inf ++ "-[ ]-LR-" ++ show fixed_lr_datasize_inf
              , flip (rmsmcLinRegr fixed_rmsmc_particles_inf) fixed_lr_datasize_inf) row_header output_file
    benchRow ("RMSMC-" ++ show fixed_rmsmc_particles_inf ++ "-[ ]-HMM-" ++ show fixed_hmm_datasize_inf
              , flip (rmsmcHMM fixed_rmsmc_particles_inf) fixed_hmm_datasize_inf) row_header output_file
    benchRow ("RMSMC-" ++ show fixed_rmsmc_particles_inf ++ "-[ ]-LDA-" ++ show fixed_lda_datasize_inf
              , flip (rmsmcLDA fixed_rmsmc_particles_inf) fixed_lda_datasize_inf) row_header output_file

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
