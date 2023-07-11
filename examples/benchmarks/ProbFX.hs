{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module ProbFX where

import BenchmarkUtil
import LinRegr
import HMM
import LDA
import Data.List.Split
import Data.Bifunctor (second)

{- | Benchmarking utility
-}
input_file :: String
input_file = "examples/benchmarks/params.txt"

output_file :: String
output_file = "examples/benchmarks/benchmarks-prob-fx.csv"

{- | Varying over dataset size
-}

bench_LR :: [Int] -> IO ()
bench_LR lr_range = do
    let row_header = ("Num datapoints", lr_range)
    writeRow output_file row_header
    benchRow ("LinRegr-[ ]-SSMH-" ++ show fixed_mh_steps
              , mhLinRegr fixed_mh_steps) row_header output_file
    benchRow ("LinRegr-[ ]-MPF-" ++ show fixed_smc_particles
              , smcLinRegr fixed_smc_particles)  row_header output_file
    benchRow ("LinRegr-[ ]-PMH-" ++ show fixed_pmh_mhsteps  ++ "-" ++ show fixed_pmh_particles
              , pmhLinRegr fixed_pmh_mhsteps fixed_pmh_particles) row_header output_file
    benchRow ("LinRegr-[ ]-RMPF-" ++ show fixed_rmpf_particles ++ "-" ++ show fixed_rmpf_mhsteps
              , rmpfLinRegr fixed_rmpf_particles fixed_rmpf_mhsteps) row_header output_file

bench_HMM :: [Int] -> IO ()
bench_HMM hmm_range = do
    let row_header = ("Num nodes", hmm_range)
    writeRow output_file row_header
    benchRow ("HidMark-[ ]-SSMH-" ++ show fixed_mh_steps
              , mhHMM fixed_mh_steps) row_header output_file
    benchRow ("HidMark-[ ]-MPF-" ++ show fixed_mh_steps
              , smcHMM fixed_smc_particles) row_header output_file
    benchRow ("HidMark-[ ]-PMH-"  ++ show fixed_pmh_mhsteps  ++ "-" ++ show fixed_pmh_particles
              , pmhHMM fixed_pmh_mhsteps fixed_pmh_particles) row_header output_file
    benchRow ("HidMark-[ ]-RMPF-" ++ show fixed_rmpf_particles ++ "-" ++ show fixed_rmpf_mhsteps
              , rmpfHMM fixed_rmpf_particles fixed_rmpf_mhsteps) row_header output_file

bench_LDA :: [Int] -> IO ()
bench_LDA lda_range = do
    let row_header = ("Num words", lda_range)
    writeRow output_file row_header
    benchRow ("LatDiri-[ ]-SSMH-" ++ show fixed_mh_steps
              , mhLDA fixed_mh_steps) row_header output_file
    benchRow ("LatDiri-[ ]-MPF-" ++ show fixed_mh_steps
              , smcLDA fixed_smc_particles) row_header output_file
    benchRow ("LatDiri-[ ]-PMH-"  ++ show fixed_pmh_mhsteps  ++ "-" ++ show fixed_pmh_particles
              , pmhLDA fixed_pmh_mhsteps fixed_pmh_particles) row_header output_file
    benchRow ("LatDiri-[ ]-RMPF-" ++ show fixed_rmpf_particles ++ "-" ++ show fixed_rmpf_mhsteps
              , rmpfLDA fixed_rmpf_particles fixed_rmpf_mhsteps) row_header output_file

{- | Varying over inference parameters
-}

bench_MH :: [Int] -> IO ()
bench_MH mh_range = do
    let row_header = ("Num SSMH steps", mh_range)
    writeRow output_file row_header
    benchRow ("SSMH-[ ]-LinRegr-" ++ show fixed_lr
              , flip mhLinRegr fixed_lr) row_header output_file
    benchRow ("SSMH-[ ]-HidMark-" ++ show fixed_hmm
              , flip mhHMM fixed_hmm) row_header output_file
    benchRow ("SSMH-[ ]-LatDiri-" ++ show fixed_lda
              , flip mhLDA fixed_lda) row_header output_file

bench_SMC :: [Int] -> IO ()
bench_SMC smc_range = do
    let row_header = ("Num MPF particles", smc_range)
    writeRow output_file row_header
    benchRow ("MPF-[ ]-LinRegr-" ++ show fixed_lr
              , flip smcLinRegr fixed_lr) row_header output_file
    benchRow ("MPF-[ ]-HidMark-" ++ show fixed_hmm
              , flip smcHMM fixed_hmm) row_header output_file
    benchRow ("MPF-[ ]-LatDiri-" ++ show fixed_lda
              , flip smcLinRegr fixed_lda) row_header output_file

bench_PMH :: [Int] -> IO ()
bench_PMH pmh_range = do
    let row_header = ("Num PMH particles", pmh_range)
    writeRow output_file row_header
    benchRow ("PMH-" ++ show fixed_pmh_mhsteps ++ "-[ ]-LinRegr-" ++ show fixed_lr
              , flip (pmhLinRegr fixed_pmh_mhsteps) fixed_lr) row_header output_file
    benchRow ("PMH-" ++ show fixed_pmh_mhsteps ++ "-[ ]-HidMark-" ++ show fixed_hmm
              , flip (pmhHMM fixed_pmh_mhsteps) fixed_hmm) row_header output_file
    benchRow ("PMH-" ++ show fixed_pmh_mhsteps ++ "-[ ]-LatDiri-" ++ show fixed_lda
              , flip (pmhLDA fixed_pmh_mhsteps) fixed_lda) row_header output_file

bench_RMPF :: [Int] -> IO ()
bench_RMPF rmpf_range = do
    let row_header = ("Num RMPF mh steps", rmpf_range)
    writeRow output_file row_header
    benchRow ("RMPF-" ++ show fixed_rmpf_particles ++ "-[ ]-LinRegr-" ++ show fixed_lr
              , flip (rmpfLinRegr fixed_rmpf_particles) fixed_lr) row_header output_file
    benchRow ("RMPF-" ++ show fixed_rmpf_particles ++ "-[ ]-HidMark-" ++ show fixed_hmm
              , flip (rmpfHMM fixed_rmpf_particles) fixed_hmm) row_header output_file
    benchRow ("RMPF-" ++ show fixed_rmpf_particles ++ "-[ ]-LatDiri-" ++ show fixed_lda
              , flip (rmpfLDA fixed_rmpf_particles) fixed_lda) row_header output_file

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
          bench_LR lr_range
          bench_HMM hmm_range
          bench_LDA lda_range
          bench_MH mh_range
          bench_SMC smc_range
          bench_PMH pmh_range
          bench_RMPF rmpf_range
        _   -> error "bad input file"
