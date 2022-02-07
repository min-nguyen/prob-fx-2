module Main where
import Criterion.Main
import Extensible.TestPaper
import Extensible.Sampler




main :: IO ()
main = defaultMain [
    bgroup "linRegrSim" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testLinRegrBasic 2000),
                        bench "4000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrBasic 4000),
                        bench "6000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrBasic 6000),
                        bench "8000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrBasic 8000),
                        bench "10000 x 100" $ nfIO $ sampleIOFixed (testLinRegrBasic 10000) ]
  ]

benchmarkLinRegrSim = defaultMain [
    bgroup "linRegrSim" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testLinRegrBasic 2000),
                        bench "4000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrBasic 4000),
                        bench "6000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrBasic 6000),
                        bench "8000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrBasic 8000),
                        bench "10000 x 100" $ nfIO $ sampleIOFixed (testLinRegrBasic 10000) ]
  ]
{-
benchmarking linRegrSim/2000 x 100
time                 252.8 ms   (232.6 ms .. 283.6 ms)
                     0.995 R²   (0.991 R² .. 1.000 R²)
mean                 268.8 ms   (256.9 ms .. 288.4 ms)
std dev              20.34 ms   (4.584 ms .. 28.14 ms)
variance introduced by outliers: 18% (moderately inflated)

benchmarking linRegrSim/4000 x 100
time                 551.5 ms   (398.2 ms .. 663.4 ms)
                     0.991 R²   (0.970 R² .. 1.000 R²)
mean                 561.3 ms   (521.2 ms .. 603.4 ms)
std dev              44.99 ms   (25.33 ms .. 63.54 ms)
variance introduced by outliers: 21% (moderately inflated)

benchmarking linRegrSim/6000 x 100
time                 792.2 ms   (NaN s .. NaN s)
                     0.995 R²   (0.984 R² .. 1.000 R²)
mean                 816.5 ms   (795.0 ms .. 832.9 ms)
std dev              22.37 ms   (10.52 ms .. 30.85 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking linRegrSim/8000 x 100
time                 1.067 s    (653.1 ms .. 1.647 s)
                     0.968 R²   (0.915 R² .. 1.000 R²)
mean                 1.145 s    (1.031 s .. 1.217 s)
std dev              118.2 ms   (60.41 ms .. 166.1 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking linRegrSim/10000 x 100
time                 1.201 s    (665.3 ms .. 1.565 s)
                     0.978 R²   (0.925 R² .. 1.000 R²)
mean                 1.384 s    (1.253 s .. 1.482 s)
std dev              133.0 ms   (39.82 ms .. 164.1 ms)
variance introduced by outliers: 22% (moderately inflated)
-}