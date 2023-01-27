{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where

import qualified ProbFX
import qualified MonadBayes

{-
    MonadBayes Benchmarks
-}


main :: IO ()
main = do
 -- ProbFX.runBenchmarks
 MonadBayes.runBenchmarks
