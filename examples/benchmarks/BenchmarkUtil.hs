{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module BenchmarkUtil where


import Criterion.Main
import Criterion.Types
import Data.List
import Sampler
import Statistics.Types
import Control.DeepSeq
import Criterion (benchmark', benchmarkWith')

fixed_mh_steps :: Int
fixed_mh_steps = 100
fixed_smc_particles :: Int
fixed_smc_particles = 100
fixed_rmsmc_particles, fixed_rmsmc_mhsteps :: Int
fixed_rmsmc_particles = 10
fixed_rmsmc_mhsteps   = 1
fixed_pmh_mhsteps, fixed_pmh_particles :: Int
fixed_pmh_mhsteps   = 50
fixed_pmh_particles = 10
fixed_bbvi_steps, fixed_bbvi_samples :: Int
fixed_bbvi_steps = 50
fixed_bbvi_samples = 10
fixed_invi_steps, fixed_invi_samples :: Int
fixed_invi_steps = 50
fixed_invi_samples = 10

fixed_lr :: Int
fixed_lr = 50
fixed_hmm :: Int
fixed_hmm = 50
fixed_lda :: Int
fixed_lda = 50

appendFileLn :: String -> String -> IO ()
appendFileLn file_name = appendFile file_name . (++ "\n")

writeRow :: Show a => String -> (String, [a]) -> IO ()
writeRow file_name (label, values) = appendFileLn file_name (intercalate "," (label : map show values))

benchMean :: NFData a => IO a -> IO Double
benchMean prog = do
  report <- benchmarkWith' (defaultConfig {confInterval=mkCL 0.14, resamples = 1}) (nfIO prog)
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
  -- Append any zeros onto early terminated benchmarks
  let means' = means ++ replicate (10 - length means) 0
  writeRow ofile (prog_name, means')

dummyRow :: String -> (a1, [a2]) -> String -> IO ()
dummyRow prog_name (_, params) ofile = do
  writeRow ofile (prog_name, map (const 0) params)
