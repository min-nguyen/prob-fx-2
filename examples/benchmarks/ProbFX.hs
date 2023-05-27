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
import Data.Bifunctor (second)

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
    let row_header = ("Num datapoints", args)
    writeRow output_file row_header
    benchRow ("LinRegr-[ ]-SSMH-" ++ show fixed_mh_steps
              , mhLinRegr fixed_mh_steps) row_header output_file
    benchRow ("LinRegr-[ ]-MPF-" ++ show fixed_smc_particles
              , smcLinRegr fixed_smc_particles)  row_header output_file
    benchRow ("LinRegr-[ ]-PMMH-" ++ show fixed_pmmh_mhsteps  ++ "-" ++ show fixed_pmmh_particles
              , pmmhLinRegr fixed_pmmh_mhsteps fixed_pmmh_particles) row_header output_file
    benchRow ("LinRegr-[ ]-RMPF-" ++ show fixed_rmsmc_particles ++ "-" ++ show fixed_rmsmc_mhsteps
              , rmsmcLinRegr fixed_rmsmc_particles fixed_rmsmc_mhsteps) row_header output_file
    benchRow ("LinRegr-[ ]-BBVI-" ++ show fixed_bbvi_steps
              , bbviLinRegr fixed_bbvi_steps fixed_bbvi_samples) row_header output_file

bench_HMM :: [Int] -> IO ()
bench_HMM args = do
    let row_header = ("Num nodes", args)
    writeRow output_file row_header
    benchRow ("HidMark-[ ]-SSMH-" ++ show fixed_mh_steps
              , mhHMM fixed_mh_steps) row_header output_file
    benchRow ("HidMark-[ ]-MPF-" ++ show fixed_mh_steps
              , smcHMM fixed_smc_particles) row_header output_file
    benchRow ("HidMark-[ ]-PMMH-"  ++ show fixed_pmmh_mhsteps  ++ "-" ++ show fixed_pmmh_particles
              , pmmhHMM fixed_pmmh_mhsteps fixed_pmmh_particles) row_header output_file
    benchRow ("HidMark-[ ]-RMPF-" ++ show fixed_rmsmc_particles ++ "-" ++ show fixed_rmsmc_mhsteps
              , rmsmcHMM fixed_rmsmc_particles fixed_rmsmc_mhsteps) row_header output_file
    benchRow ("HidMark-[ ]-BBVI-" ++ show fixed_bbvi_steps
              , bbviHMM fixed_bbvi_steps fixed_bbvi_samples) row_header output_file

bench_LDA :: [Int] -> IO ()
bench_LDA args = do
    let row_header = ("Num words", args)
    writeRow output_file row_header
    benchRow ("LatDiri-[ ]-SSMH-" ++ show fixed_mh_steps
              , mhLDA fixed_mh_steps) row_header output_file
    benchRow ("LatDiri-[ ]-MPF-" ++ show fixed_mh_steps
              , smcLDA fixed_smc_particles) row_header output_file
    benchRow ("LatDiri-[ ]-PMMH-"  ++ show fixed_pmmh_mhsteps  ++ "-" ++ show fixed_pmmh_particles
              , pmmhLDA fixed_pmmh_mhsteps fixed_pmmh_particles) row_header output_file
    benchRow ("LatDiri-[ ]-RMPF-" ++ show fixed_rmsmc_particles ++ "-" ++ show fixed_rmsmc_mhsteps
              , rmsmcLDA fixed_rmsmc_particles fixed_rmsmc_mhsteps) row_header output_file
    benchRow ("LatDiri-[ ]-BBVI-" ++ show fixed_bbvi_steps
              , bbviLDA fixed_bbvi_steps fixed_bbvi_samples) row_header output_file

{- | Varying over inference parameters
-}

bench_MH :: [Int] -> IO ()
bench_MH args = do
    let row_header = ("Num SSMH steps", args)
    writeRow output_file row_header
    benchRow ("SSMH-[ ]-LinRegr-" ++ show fixed_lr
              , flip mhLinRegr fixed_lr) row_header output_file
    benchRow ("SSMH-[ ]-HidMark-" ++ show fixed_hmm
              , flip mhHMM fixed_hmm) row_header output_file
    benchRow ("SSMH-[ ]-LatDiri-" ++ show fixed_lda
              , flip mhLDA fixed_lda) row_header output_file

bench_SMC :: [Int] -> IO ()
bench_SMC args = do
    let row_header = ("Num MPF particles", args)
    writeRow output_file row_header
    benchRow ("MPF-[ ]-LinRegr-" ++ show fixed_lr
              , flip smcLinRegr fixed_lr) row_header output_file
    benchRow ("MPF-[ ]-HidMark-" ++ show fixed_hmm
              , flip smcHMM fixed_hmm) row_header output_file
    benchRow ("MPF-[ ]-LatDiri-" ++ show fixed_lda
              , flip smcLinRegr fixed_lda) row_header output_file

bench_RMSMC :: [Int] -> IO ()
bench_RMSMC args = do
    let row_header = ("Num RMPF mh steps", args)
    writeRow output_file row_header
    benchRow ("RMPF-" ++ show fixed_rmsmc_particles ++ "-[ ]-LinRegr-" ++ show fixed_lr
              , flip (rmsmcLinRegr fixed_rmsmc_particles) fixed_lr) row_header output_file
    benchRow ("RMPF-" ++ show fixed_rmsmc_particles ++ "-[ ]-HidMark-" ++ show fixed_hmm
              , flip (rmsmcHMM fixed_rmsmc_particles) fixed_hmm) row_header output_file
    benchRow ("RMPF-" ++ show fixed_rmsmc_particles ++ "-[ ]-LatDiri-" ++ show fixed_lda
              , flip (rmsmcLDA fixed_rmsmc_particles) fixed_lda) row_header output_file

bench_PMMH :: [Int] -> IO ()
bench_PMMH args = do
    let row_header = ("Num PMMH particles", args)
    writeRow output_file row_header
    benchRow ("PMMH-" ++ show fixed_pmmh_mhsteps ++ "-[ ]-LinRegr-" ++ show fixed_lr
              , flip (pmmhLinRegr fixed_pmmh_mhsteps) fixed_lr) row_header output_file
    benchRow ("PMMH-" ++ show fixed_pmmh_mhsteps ++ "-[ ]-HidMark-" ++ show fixed_hmm
              , flip (pmmhHMM fixed_pmmh_mhsteps) fixed_hmm) row_header output_file
    benchRow ("PMMH-" ++ show fixed_pmmh_mhsteps ++ "-[ ]-LatDiri-" ++ show fixed_lda
              , flip (pmmhLDA fixed_pmmh_mhsteps) fixed_lda) row_header output_file

bench_BBVI :: [Int] -> IO ()
bench_BBVI args = do
    let row_header = ("Num BBVI steps", args)
    writeRow output_file row_header
    benchRow ("BBVI-[ ]-LinRegr-" ++ show fixed_lr
              , flip (bbviLinRegr fixed_bbvi_samples) fixed_lr) row_header output_file
    benchRow ("BBVI-[ ]-HidMark-" ++ show fixed_hmm
              , flip (bbviHMM fixed_bbvi_samples) fixed_hmm) row_header output_file
    benchRow ("BBVI-[ ]-LatDiri-" ++ show fixed_lda
              , flip (bbviLDA fixed_bbvi_samples) fixed_lda) row_header output_file

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
        [lr, hmm, lda, mh, smc, rmpfWith, pmmh, bbvi] -> do
          bench_LR lr
          bench_HMM hmm
          bench_LDA lda
          bench_MH mh
          bench_SMC smc
          bench_PMMH pmmh
          bench_RMSMC rmpfWith
          bench_BBVI bbvi
        _   -> error "bad input file"
