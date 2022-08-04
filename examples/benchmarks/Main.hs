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
  means <- mapM (benchMean . sampleIO . prog) params
  writeRow fixed_output_file (prog_name, means)

{- | Varying over dataset size
-}
fixed_simulations :: Int
fixed_simulations = 1
fixed_lw_steps :: Int
fixed_lw_steps = 100
fixed_mh_steps :: Int
fixed_mh_steps = 100

bench_LR :: [Int] -> IO ()
bench_LR args = do
    let row_header = ("Dataset size", args)
    writeRow fixed_output_file row_header
    benchRow ("LR-SIM" ++ show fixed_simulations, simLinRegr) row_header
    benchRow ("LR-LW" ++ show fixed_lw_steps, lwLinRegr fixed_lw_steps) row_header
    benchRow ("LR-MH" ++ show fixed_mh_steps, mhLinRegr fixed_mh_steps) row_header

bench_HMM :: [Int] -> IO ()
bench_HMM args = do
    let row_header = ("Dataset size", args)
    writeRow fixed_output_file row_header
    benchRow ("HMM-SIM" ++ show fixed_simulations, simHMM) row_header
    benchRow ("HMM-LW" ++ show fixed_lw_steps, lwHMM fixed_lw_steps) row_header
    benchRow ("HMM-MH" ++ show fixed_mh_steps, mhHMM fixed_mh_steps) row_header

bench_LDA :: [Int] -> IO ()
bench_LDA args = do
    let row_header = ("Dataset size", args)
    writeRow fixed_output_file row_header
    benchRow ("LDA-SIM" ++ show fixed_simulations, simLDA) row_header
    benchRow ("LDA-LW" ++ show fixed_lw_steps, lwLDA fixed_lw_steps) row_header
    benchRow ("LDA-MH" ++ show fixed_mh_steps, mhPredLDA fixed_mh_steps) row_header

{- | Varying over inference parameters
-}
fixed_lr_datasize :: Int
fixed_lr_datasize = 50
fixed_hmm_datasize :: Int
fixed_hmm_datasize = 50
fixed_lda_datasize :: Int
fixed_lda_datasize = 100

bench_SIM :: [Int] -> IO ()
bench_SIM args = do
    let row_header = ("Number of simulations", args)
    writeRow fixed_output_file row_header
    benchRow ("SIM-LR" ++ show fixed_lr_datasize, flip replicateM (simLinRegr fixed_lr_datasize)) row_header
    benchRow ("SIM-HMM" ++ show fixed_hmm_datasize, flip replicateM (simHMM fixed_hmm_datasize)) row_header
    benchRow ("SIM-LDA" ++ show fixed_lda_datasize, flip replicateM (simLDA fixed_lda_datasize)) row_header

bench_LW :: [Int] -> IO ()
bench_LW args = do
    let row_header = ("Number of LW iterations", args)
    writeRow fixed_output_file row_header
    benchRow ("LW-LR" ++ show fixed_lr_datasize, flip lwLinRegr fixed_lr_datasize) row_header
    benchRow ("LW-HMM" ++ show fixed_hmm_datasize, flip lwHMM fixed_hmm_datasize) row_header
    benchRow ("LW-LDA" ++ show fixed_lda_datasize, flip lwLDA fixed_lda_datasize) row_header

bench_MH :: [Int] -> IO ()
bench_MH args = do
    let row_header = ("Number of MH steps", args)
    writeRow fixed_output_file row_header
    benchRow ("MH-LR" ++ show fixed_lr_datasize, flip mhLinRegr fixed_lr_datasize) row_header
    benchRow ("MH-HMM" ++ show fixed_hmm_datasize, flip mhHMM fixed_hmm_datasize) row_header
    benchRow ("MH-LDA" ++ show fixed_lda_datasize, flip mhPredLDA fixed_lda_datasize) row_header

bench_MHInv :: [Int] -> IO ()
bench_MHInv args = do
    let row_header = ("Number of MHInv steps", args)
    writeRow fixed_output_file row_header
    benchRow ("MHInv-LR" ++ show fixed_lr_datasize, flip mhInvLinRegr fixed_lr_datasize) row_header
    benchRow ("MHInv-HMM" ++ show fixed_hmm_datasize, flip mhInvHMM fixed_hmm_datasize) row_header
    benchRow ("MHInv-LDA" ++ show fixed_lda_datasize, flip mhInvPredLDA fixed_lda_datasize) row_header

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
        [lr, hmm, lda, sim, lw, mh] -> do
          -- bench_LR lr
          -- bench_HMM hmm
          -- bench_LDA lda
          -- bench_SIM sim
          -- bench_LW lw
          bench_MH mh
          bench_MHInv mh
        _   -> error "bad input file"

main :: IO ()
main = runBenchmarks
