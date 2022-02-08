{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Main where
import Criterion.Main
import Extensible.TestPaper
import Extensible.Sampler


main :: IO ()
main = do
  -- benchmarkLinRegrSim_DataSize
  -- benchmarkLinRegrLW_DataSize
  -- benchmarkLinRegrMH_DataSize
  -- benchmarkHMMSim_DataSize
  -- benchmarkHMMLW_DataSize
  -- benchmarkHMMMH_DataSize
  -- benchmarkTopicSim_DataSize
  -- benchmarkTopicLW_DataSize
  benchmarkTopicMH_DataSize

benchmarkLinRegrSim_SampleSize = defaultMain [
    bgroup "linRegrSim" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testLinRegrBasic 100 2000),
                        bench "4000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrBasic 100 4000),
                        bench "6000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrBasic 100 6000),
                        bench "8000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrBasic 100 8000),
                        bench "10000 x 100" $ nfIO $ sampleIOFixed (testLinRegrBasic 100 10000) ]
  ]

benchmarkLinRegrLW_SampleSize = defaultMain [
    bgroup "linRegrLW" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testLinRegrLWInf 100 2000),
                        bench "4000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrLWInf 100 4000),
                        bench "6000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrLWInf 100 6000),
                        bench "8000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrLWInf 100 8000),
                        bench "10000 x 100" $ nfIO $ sampleIOFixed (testLinRegrLWInf 100 10000) ]
  ]

benchmarkLinRegrMH_SampleSize = defaultMain [
    bgroup "linRegrMH" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testLinRegrMHPost 100 2000),
                        bench "4000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrMHPost 100 4000),
                        bench "6000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrMHPost 100 6000),
                        bench "8000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrMHPost 100 8000),
                        bench "10000 x 100" $ nfIO $ sampleIOFixed (testLinRegrMHPost 100 10000) ]
  ]

benchmarkHMMSim_SampleSize = defaultMain [
    bgroup "hmmSim" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testHMMBasic 100 2000),
                    bench "4000 x 100"  $ nfIO $ sampleIOFixed (testHMMBasic 100 4000),
                    bench "6000 x 100"  $ nfIO $ sampleIOFixed (testHMMBasic 100 6000),
                    bench "8000 x 100"  $ nfIO $ sampleIOFixed (testHMMBasic 100 8000),
                    bench "10000 x 100" $ nfIO $ sampleIOFixed (testHMMBasic 100 10000) ]
  ]

benchmarkHMMLW_SampleSize = defaultMain [
    bgroup "hmmLW" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testHMMLWInf 100 2000),
                    bench "4000 x 100"  $ nfIO $ sampleIOFixed (testHMMLWInf 100 4000),
                    bench "6000 x 100"  $ nfIO $ sampleIOFixed (testHMMLWInf 100 6000),
                    bench "8000 x 100"  $ nfIO $ sampleIOFixed (testHMMLWInf 100 8000),
                    bench "10000 x 100" $ nfIO $ sampleIOFixed (testHMMLWInf 100 10000) ]
  ]

benchmarkHMMMH_SampleSize = defaultMain [
    bgroup "hmmMH" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testHMMMHPost 100 2000),
                    bench "4000 x 100"  $ nfIO $ sampleIOFixed (testHMMMHPost 100 4000),
                    bench "6000 x 100"  $ nfIO $ sampleIOFixed (testHMMMHPost 100 6000),
                    bench "8000 x 100"  $ nfIO $ sampleIOFixed (testHMMMHPost 100 8000),
                    bench "10000 x 100" $ nfIO $ sampleIOFixed (testHMMMHPost 100 10000) ]
  ]

benchmarkTopicSim_SampleSize = defaultMain [
    bgroup "topicSim" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testTopicBasic 100 2000),
                      bench "4000 x 100"  $ nfIO $ sampleIOFixed (testTopicBasic 100 4000),
                      bench "6000 x 100"  $ nfIO $ sampleIOFixed (testTopicBasic 100 6000),
                      bench "8000 x 100"  $ nfIO $ sampleIOFixed (testTopicBasic 100 8000),
                      bench "10000 x 100" $ nfIO $ sampleIOFixed (testTopicBasic 100 10000) ]
  ]

benchmarkTopicLW_SampleSize = defaultMain [
    bgroup "topicLW" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testTopicLW 100 2000),
                      bench "4000 x 100"  $ nfIO $ sampleIOFixed (testTopicLW 100 4000),
                      bench "6000 x 100"  $ nfIO $ sampleIOFixed (testTopicLW 100 6000),
                      bench "8000 x 100"  $ nfIO $ sampleIOFixed (testTopicLW 100 8000),
                      bench "10000 x 100" $ nfIO $ sampleIOFixed (testTopicLW 100 10000) ]
  ]


benchmarkTopicMH_SampleSize = defaultMain [
    bgroup "topicMH" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testTopicMHPost 100 2000),
                      bench "4000 x 100"  $ nfIO $ sampleIOFixed (testTopicMHPost 100 4000),
                      bench "6000 x 100"  $ nfIO $ sampleIOFixed (testTopicMHPost 100 6000),
                      bench "8000 x 100"  $ nfIO $ sampleIOFixed (testTopicMHPost 100 8000),
                      bench "10000 x 100" $ nfIO $ sampleIOFixed (testTopicMHPost 100 10000) ]
  ]

---------------------------------

benchmarkLinRegrSim_DataSize = defaultMain [
    bgroup "linRegrSim" [bench "200 x 200" $ nfIO $ sampleIOFixed (testLinRegrBasic 200 2000),
                        bench "400 x 200"  $ nfIO $ sampleIOFixed (testLinRegrBasic 400 2000),
                        bench "600 x 200"  $ nfIO $ sampleIOFixed (testLinRegrBasic 600 2000),
                        bench "800 x 200"  $ nfIO $ sampleIOFixed (testLinRegrBasic 800 2000),
                        bench "1000 x 200" $ nfIO $ sampleIOFixed (testLinRegrBasic 1000 2000) ]
  ]

benchmarkLinRegrLW_DataSize = defaultMain [
    bgroup "linRegrLW" [bench "200 x 200" $ nfIO $ sampleIOFixed (testLinRegrLWInf 200 2000),
                        bench "400 x 200"  $ nfIO $ sampleIOFixed (testLinRegrLWInf 400 2000),
                        bench "600 x 200"  $ nfIO $ sampleIOFixed (testLinRegrLWInf 600 2000),
                        bench "800 x 200"  $ nfIO $ sampleIOFixed (testLinRegrLWInf 800 2000),
                        bench "1000 x 200" $ nfIO $ sampleIOFixed (testLinRegrLWInf 1000 2000) ]
  ]

benchmarkLinRegrMH_DataSize = defaultMain [
    bgroup "linRegrMH" [bench "200 x 200" $ nfIO $ sampleIOFixed (testLinRegrMHPost 200 2000),
                        bench "400 x 200"  $ nfIO $ sampleIOFixed (testLinRegrMHPost 400 2000),
                        bench "600 x 200"  $ nfIO $ sampleIOFixed (testLinRegrMHPost 600 2000),
                        bench "800 x 200"  $ nfIO $ sampleIOFixed (testLinRegrMHPost 800 2000),
                        bench "1000 x 200" $ nfIO $ sampleIOFixed (testLinRegrMHPost 1000 2000) ]
  ]

benchmarkHMMSim_DataSize = defaultMain [
    bgroup "hmmSim" [bench "40 x 200" $ nfIO $ sampleIOFixed (testHMMBasic 40 2000),
                    bench "80 x 200"  $ nfIO $ sampleIOFixed (testHMMBasic 80 2000),
                    bench "120 x 200"  $ nfIO $ sampleIOFixed (testHMMBasic 120 2000),
                    bench "160 x 200"  $ nfIO $ sampleIOFixed (testHMMBasic 160 2000),
                    bench "200 x 200" $ nfIO $ sampleIOFixed (testHMMBasic 200 2000) ]
  ]

benchmarkHMMLW_DataSize = defaultMain [
    bgroup "hmmLW" [bench "40 x 200" $ nfIO $ sampleIOFixed (testHMMLWInf 40 2000),
                    bench "80 x 200"  $ nfIO $ sampleIOFixed (testHMMLWInf 80 2000),
                    bench "120 x 200"  $ nfIO $ sampleIOFixed (testHMMLWInf 120 2000),
                    bench "160 x 200"  $ nfIO $ sampleIOFixed (testHMMLWInf 160 2000),
                    bench "200 x 200" $ nfIO $ sampleIOFixed (testHMMLWInf 200 2000) ]
  ]

benchmarkHMMMH_DataSize = defaultMain [
    bgroup "hmmMH" [bench "40 x 200" $ nfIO $ sampleIOFixed (testHMMMHPost 40 2000),
                    bench "80 x 200"  $ nfIO $ sampleIOFixed (testHMMMHPost 80 2000),
                    bench "120 x 200"  $ nfIO $ sampleIOFixed (testHMMMHPost 120 2000),
                    bench "160 x 200"  $ nfIO $ sampleIOFixed (testHMMMHPost 160 2000),
                    bench "200 x 200" $ nfIO $ sampleIOFixed (testHMMMHPost 200 2000) ]
  ]

benchmarkTopicSim_DataSize = defaultMain [
    bgroup "topicSim" [bench "40 x 200" $ nfIO $ sampleIOFixed (testTopicBasic 40 2000),
                      bench "80 x 200"  $ nfIO $ sampleIOFixed (testTopicBasic 80 2000),
                      bench "120 x 200"  $ nfIO $ sampleIOFixed (testTopicBasic 120 2000),
                      bench "160 x 200"  $ nfIO $ sampleIOFixed (testTopicBasic 160 2000),
                      bench "200 x 200" $ nfIO $ sampleIOFixed (testTopicBasic 200 2000) ]
  ]

benchmarkTopicLW_DataSize = defaultMain [
    bgroup "topicLW" [bench "40 x 200" $ nfIO $ sampleIOFixed (testTopicLW 40 2000),
                      bench "80 x 200"  $ nfIO $ sampleIOFixed (testTopicLW 80 2000),
                      bench "120 x 200"  $ nfIO $ sampleIOFixed (testTopicLW 120 2000),
                      bench "160 x 200"  $ nfIO $ sampleIOFixed (testTopicLW 160 2000),
                      bench "200 x 200" $ nfIO $ sampleIOFixed (testTopicLW 200 2000) ]
  ]


benchmarkTopicMH_DataSize = defaultMain [
    bgroup "topicMH" [--bench "40 x 200" $ nfIO $ sampleIOFixed (testTopicMHPost 40 2000),
                      --bench "80 x 200"  $ nfIO $ sampleIOFixed (testTopicMHPost 80 2000),
                      --bench "120 x 200"  $ nfIO $ sampleIOFixed (testTopicMHPost 120 2000),
                      --bench "160 x 200"  $ nfIO $ sampleIOFixed (testTopicMHPost 160 2000),
                      bench "200 x 200" $ nfIO $ sampleIOFixed (testTopicMHPost 200 2000) ]
  ]

{-
benchmarking linRegrSim/200 x 200
time                 294.0 ms   (196.5 ms .. 383.5 ms)
                     0.987 R²   (0.951 R² .. 1.000 R²)
mean                 352.8 ms   (324.1 ms .. 388.2 ms)
std dev              38.25 ms   (14.03 ms .. 52.25 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking linRegrSim/400 x 200
time                 537.3 ms   (415.7 ms .. 773.2 ms)
                     0.978 R²   (0.957 R² .. 1.000 R²)
mean                 771.8 ms   (654.0 ms .. 960.7 ms)
std dev              179.3 ms   (19.66 ms .. 227.9 ms)
variance introduced by outliers: 48% (moderately inflated)

benchmarking linRegrSim/600 x 200
time                 1.390 s    (632.0 ms .. 2.273 s)
                     0.954 R²   (0.862 R² .. 1.000 R²)
mean                 1.334 s    (1.056 s .. 1.440 s)
std dev              195.7 ms   (9.241 ms .. 244.8 ms)
variance introduced by outliers: 24% (moderately inflated)

benchmarking linRegrSim/800 x 200
time                 1.389 s    (992.0 ms .. 1.875 s)
                     0.986 R²   (0.955 R² .. 1.000 R²)
mean                 1.541 s    (1.409 s .. 1.641 s)
std dev              138.9 ms   (69.96 ms .. 192.3 ms)
variance introduced by outliers: 22% (moderately inflated)

benchmarking linRegrSim/1000 x 200
time                 1.500 s    (1.129 s .. 1.806 s)
                     0.990 R²   (0.988 R² .. 1.000 R²)
mean                 1.921 s    (1.710 s .. 2.086 s)
std dev              218.5 ms   (93.42 ms .. 296.9 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking linRegrLW/200 x 200
time                 276.0 ms   (205.9 ms .. 414.8 ms)
                     0.971 R²   (0.946 R² .. 1.000 R²)
mean                 330.9 ms   (305.9 ms .. 360.5 ms)
std dev              33.01 ms   (12.67 ms .. 45.14 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking linRegrLW/400 x 200
time                 594.7 ms   (486.2 ms .. 696.4 ms)
                     0.994 R²   (0.994 R² .. 1.000 R²)
mean                 669.1 ms   (627.7 ms .. 745.5 ms)
std dev              74.56 ms   (4.296 ms .. 91.36 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking linRegrLW/600 x 200
time                 829.7 ms   (608.3 ms .. 1.107 s)
                     0.982 R²   (0.978 R² .. 1.000 R²)
mean                 951.0 ms   (886.3 ms .. 992.7 ms)
std dev              68.20 ms   (3.639 ms .. 96.28 ms)
variance introduced by outliers: 20% (moderately inflated)

benchmarking linRegrLW/800 x 200
time                 982.7 ms   (852.3 ms .. 1.251 s)
                     0.991 R²   (0.985 R² .. 1.000 R²)
mean                 1.430 s    (1.217 s .. 1.774 s)
std dev              326.4 ms   (5.636 ms .. 400.4 ms)
variance introduced by outliers: 48% (moderately inflated)

benchmarking linRegrLW/1000 x 200
time                 1.688 s    (1.223 s .. 2.049 s)
                     0.991 R²   (0.969 R² .. 1.000 R²)
mean                 1.682 s    (1.565 s .. 1.758 s)
std dev              122.6 ms   (62.32 ms .. 172.2 ms)
variance introduced by outliers: 20% (moderately inflated)

benchmarking linRegrMH/200 x 200
time                 461.4 ms   (400.9 ms .. 521.6 ms)
                     0.998 R²   (0.992 R² .. 1.000 R²)
mean                 412.4 ms   (386.3 ms .. 436.3 ms)
std dev              27.88 ms   (23.22 ms .. 30.50 ms)
variance introduced by outliers: 20% (moderately inflated)

benchmarking linRegrMH/400 x 200
time                 930.1 ms   (907.5 ms .. 957.2 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 896.4 ms   (875.7 ms .. 912.2 ms)
std dev              20.04 ms   (11.31 ms .. 24.18 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking linRegrMH/600 x 200
time                 1.726 s    (1.540 s .. 1.841 s)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 1.772 s    (1.742 s .. 1.788 s)
std dev              28.22 ms   (5.502 ms .. 36.96 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking linRegrMH/800 x 200
time                 2.660 s    (2.624 s .. 2.689 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.739 s    (2.702 s .. 2.790 s)
std dev              48.18 ms   (20.38 ms .. 60.39 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking linRegrMH/1000 x 200
time                 4.355 s    (2.605 s .. 5.715 s)
                     0.982 R²   (0.935 R² .. 1.000 R²)
mean                 4.417 s    (4.275 s .. 4.669 s)
std dev              246.3 ms   (38.29 ms .. 314.0 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking hmmSim/40 x 200
time                 172.3 ms   (130.2 ms .. 223.8 ms)
                     0.961 R²   (0.940 R² .. 1.000 R²)
mean                 270.7 ms   (226.4 ms .. 312.2 ms)
std dev              54.21 ms   (38.74 ms .. 70.64 ms)
variance introduced by outliers: 57% (severely inflated)

benchmarking hmmSim/80 x 200
time                 720.3 ms   (519.6 ms .. 977.5 ms)
                     0.986 R²   (0.955 R² .. 1.000 R²)
mean                 719.7 ms   (644.1 ms .. 750.9 ms)
std dev              56.13 ms   (25.21 ms .. 71.92 ms)
variance introduced by outliers: 21% (moderately inflated)

benchmarking hmmSim/120 x 200
time                 1.246 s    (271.8 ms .. 2.059 s)
                     0.933 R²   (0.773 R² .. 1.000 R²)
mean                 1.490 s    (1.311 s .. 1.629 s)
std dev              179.7 ms   (118.5 ms .. 217.8 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking hmmSim/160 x 200
time                 2.189 s    (1.555 s .. 3.293 s)
                     0.972 R²   (0.940 R² .. 1.000 R²)
mean                 2.394 s    (2.158 s .. 2.659 s)
std dev              280.0 ms   (132.0 ms .. 392.6 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking hmmSim/200 x 200
time                 3.629 s    (1.154 s .. 5.340 s)
                     0.951 R²   (0.828 R² .. 1.000 R²)
mean                 3.826 s    (3.564 s .. 4.153 s)
std dev              333.5 ms   (117.3 ms .. 453.5 ms)
variance introduced by outliers: 22% (moderately inflated)

benchmarking hmmLW/40 x 200
time                 200.8 ms   (164.3 ms .. 231.6 ms)
                     0.986 R²   (0.980 R² .. 1.000 R²)
mean                 217.7 ms   (200.9 ms .. 258.9 ms)
std dev              35.35 ms   (1.087 ms .. 48.81 ms)
variance introduced by outliers: 48% (moderately inflated)

benchmarking hmmLW/80 x 200
time                 424.5 ms   (364.8 ms .. 501.9 ms)
                     0.996 R²   (0.988 R² .. 1.000 R²)
mean                 438.4 ms   (421.8 ms .. 456.4 ms)
std dev              21.73 ms   (7.903 ms .. 29.14 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking hmmLW/120 x 200
time                 949.8 ms   (653.2 ms .. 1.289 s)
                     0.985 R²   (0.949 R² .. 1.000 R²)
mean                 1.095 s    (1.020 s .. 1.169 s)
std dev              91.11 ms   (46.29 ms .. 109.4 ms)
variance introduced by outliers: 21% (moderately inflated)

benchmarking hmmLW/160 x 200
time                 2.242 s    (1.732 s .. 2.757 s)
                     0.993 R²   (0.975 R² .. 1.000 R²)
mean                 2.229 s    (2.114 s .. 2.357 s)
std dev              142.6 ms   (57.46 ms .. 197.7 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking hmmLW/200 x 200
time                 3.339 s    (2.853 s .. 3.828 s)
                     0.996 R²   (0.996 R² .. 1.000 R²)
mean                 3.427 s    (3.318 s .. 3.531 s)
std dev              124.9 ms   (87.04 ms .. 148.7 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking hmmMH/40 x 200
time                 266.9 ms   (215.6 ms .. 315.8 ms)
                     0.989 R²   (0.984 R² .. 1.000 R²)
mean                 272.1 ms   (255.1 ms .. 283.8 ms)
std dev              18.39 ms   (10.60 ms .. 25.28 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking hmmMH/80 x 200
time                 600.0 ms   (583.1 ms .. 623.9 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 615.9 ms   (607.6 ms .. 631.1 ms)
std dev              14.88 ms   (359.3 μs .. 18.88 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking hmmMH/120 x 200
time                 1.319 s    (1.275 s .. 1.338 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.364 s    (1.344 s .. 1.387 s)
std dev              24.85 ms   (10.26 ms .. 34.50 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking hmmMH/160 x 200
time                 2.823 s    (2.353 s .. 3.274 s)
                     0.995 R²   (NaN R² .. 1.000 R²)
mean                 3.077 s    (2.943 s .. 3.152 s)
std dev              129.1 ms   (36.39 ms .. 174.8 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking hmmMH/200 x 200
time                 5.042 s    (4.504 s .. 5.685 s)
                     0.997 R²   (NaN R² .. 1.000 R²)
mean                 4.797 s    (4.687 s .. 4.929 s)
std dev              140.8 ms   (52.16 ms .. 194.5 ms)
variance introduced by outliers: 19% (moderately inflated)
benchmarking topicSim/40 x 200
time                 260.0 ms   (233.0 ms .. 294.4 ms)
                     0.995 R²   (0.981 R² .. 1.000 R²)
mean                 252.5 ms   (245.1 ms .. 260.3 ms)
std dev              10.34 ms   (6.571 ms .. 15.15 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking topicSim/80 x 200
time                 544.8 ms   (504.3 ms .. 602.5 ms)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 532.9 ms   (521.7 ms .. 541.5 ms)
std dev              11.66 ms   (5.323 ms .. 16.05 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking topicSim/120 x 200
time                 886.1 ms   (804.0 ms .. 961.1 ms)
                     0.998 R²   (0.998 R² .. 1.000 R²)
mean                 921.5 ms   (898.1 ms .. 967.4 ms)
std dev              43.86 ms   (7.774 ms .. 53.35 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking topicSim/160 x 200
time                 1.265 s    (1.170 s .. 1.385 s)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 1.419 s    (1.345 s .. 1.525 s)
std dev              105.4 ms   (210.4 μs .. 138.9 ms)
variance introduced by outliers: 21% (moderately inflated)

benchmarking topicSim/200 x 200
time                 1.942 s    (1.505 s .. 2.434 s)
                     0.992 R²   (0.973 R² .. 1.000 R²)
mean                 1.946 s    (1.834 s .. 1.987 s)
std dev              75.64 ms   (8.626 ms .. 95.16 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking topicLW/40 x 200
time                 214.5 ms   (209.2 ms .. 223.9 ms)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 220.7 ms   (217.4 ms .. 225.4 ms)
std dev              5.405 ms   (2.859 ms .. 8.429 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking topicLW/80 x 200
time                 479.2 ms   (447.4 ms .. 498.8 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 485.5 ms   (481.0 ms .. 488.2 ms)
std dev              4.455 ms   (1.917 ms .. 6.164 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking topicLW/120 x 200
time                 827.5 ms   (741.8 ms .. 864.5 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 895.2 ms   (864.4 ms .. 956.5 ms)
std dev              61.37 ms   (14.10 μs .. 70.92 ms)
variance introduced by outliers: 20% (moderately inflated)

benchmarking topicLW/160 x 200
time                 1.307 s    (1.123 s .. 1.498 s)
                     0.997 R²   (0.990 R² .. 1.000 R²)
mean                 1.372 s    (1.319 s .. 1.454 s)
std dev              78.36 ms   (18.64 ms .. 103.9 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking topicLW/200 x 200
time                 1.853 s    (1.747 s .. 1.931 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.902 s    (1.870 s .. 1.926 s)
std dev              33.31 ms   (25.13 ms .. 39.74 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking topicMH/40 x 200
time                 439.0 ms   (306.5 ms .. 637.5 ms)
                     0.971 R²   (0.960 R² .. 1.000 R²)
mean                 352.2 ms   (325.0 ms .. 397.7 ms)
std dev              43.73 ms   (6.169 ms .. 56.17 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking topicMH/80 x 200
time                 881.7 ms   (782.9 ms .. 1.028 s)
                     0.997 R²   (0.991 R² .. 1.000 R²)
mean                 808.2 ms   (765.1 ms .. 848.8 ms)
std dev              47.61 ms   (37.51 ms .. 55.00 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking topicMH/120 x 200
time                 1.605 s    (1.542 s .. 1.644 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.600 s    (1.584 s .. 1.612 s)
std dev              16.26 ms   (7.219 ms .. 21.81 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking topicMH/160 x 200
time                 2.716 s    (2.151 s .. 3.219 s)
                     0.995 R²   (0.981 R² .. 1.000 R²)
mean                 2.730 s    (2.672 s .. 2.808 s)
std dev              84.70 ms   (4.257 ms .. 106.0 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking topicMH/200 x 200
time                 3.655 s    (3.129 s .. 4.190 s)
                     0.997 R²   (0.990 R² .. 1.000 R²)
mean                 3.926 s    (3.794 s .. 4.126 s)
std dev              192.4 ms   (60.60 ms .. 255.3 ms)
variance introduced by outliers: 19% (moderately inflated)

-}