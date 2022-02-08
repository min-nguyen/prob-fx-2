{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Main where
import Criterion.Main
import Extensible.TestPaper
import Extensible.Sampler


main :: IO ()
main = do
  benchmarkLinRegrSim_DataSize
  benchmarkLinRegrLW_DataSize
  benchmarkLinRegrMH_DataSize
  benchmarkHMMSim_DataSize
  benchmarkHMMLW_DataSize
  benchmarkHMMMH_DataSize
  benchmarkTopicSim_DataSize
  benchmarkTopicLW_DataSize
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
    bgroup "hmmSim" [bench "200 x 200" $ nfIO $ sampleIOFixed (testHMMBasic 200 2000),
                    bench "400 x 200"  $ nfIO $ sampleIOFixed (testHMMBasic 400 2000),
                    bench "600 x 200"  $ nfIO $ sampleIOFixed (testHMMBasic 600 2000),
                    bench "800 x 200"  $ nfIO $ sampleIOFixed (testHMMBasic 800 2000),
                    bench "1000 x 200" $ nfIO $ sampleIOFixed (testHMMBasic 1000 2000) ]
  ]

benchmarkHMMLW_DataSize = defaultMain [
    bgroup "hmmLW" [bench "200 x 200" $ nfIO $ sampleIOFixed (testHMMLWInf 200 2000),
                    bench "400 x 200"  $ nfIO $ sampleIOFixed (testHMMLWInf 400 2000),
                    bench "600 x 200"  $ nfIO $ sampleIOFixed (testHMMLWInf 600 2000),
                    bench "800 x 200"  $ nfIO $ sampleIOFixed (testHMMLWInf 800 2000),
                    bench "1000 x 200" $ nfIO $ sampleIOFixed (testHMMLWInf 1000 2000) ]
  ]

benchmarkHMMMH_DataSize = defaultMain [
    bgroup "hmmMH" [bench "200 x 200" $ nfIO $ sampleIOFixed (testHMMMHPost 200 2000),
                    bench "400 x 200"  $ nfIO $ sampleIOFixed (testHMMMHPost 400 2000),
                    bench "600 x 200"  $ nfIO $ sampleIOFixed (testHMMMHPost 600 2000),
                    bench "800 x 200"  $ nfIO $ sampleIOFixed (testHMMMHPost 800 2000),
                    bench "1000 x 200" $ nfIO $ sampleIOFixed (testHMMMHPost 1000 2000) ]
  ]

benchmarkTopicSim_DataSize = defaultMain [
    bgroup "topicSim" [bench "200 x 200" $ nfIO $ sampleIOFixed (testTopicBasic 200 2000),
                      bench "400 x 200"  $ nfIO $ sampleIOFixed (testTopicBasic 400 2000),
                      bench "600 x 200"  $ nfIO $ sampleIOFixed (testTopicBasic 600 2000),
                      bench "800 x 200"  $ nfIO $ sampleIOFixed (testTopicBasic 800 2000),
                      bench "1000 x 200" $ nfIO $ sampleIOFixed (testTopicBasic 1000 2000) ]
  ]

benchmarkTopicLW_DataSize = defaultMain [
    bgroup "topicLW" [bench "200 x 200" $ nfIO $ sampleIOFixed (testTopicLW 200 2000),
                      bench "400 x 200"  $ nfIO $ sampleIOFixed (testTopicLW 400 2000),
                      bench "600 x 200"  $ nfIO $ sampleIOFixed (testTopicLW 600 2000),
                      bench "800 x 200"  $ nfIO $ sampleIOFixed (testTopicLW 800 2000),
                      bench "1000 x 200" $ nfIO $ sampleIOFixed (testTopicLW 10000 2000) ]
  ]


benchmarkTopicMH_DataSize = defaultMain [
    bgroup "topicMH" [bench "200 x 200" $ nfIO $ sampleIOFixed (testTopicMHPost 200 2000),
                      bench "400 x 200"  $ nfIO $ sampleIOFixed (testTopicMHPost 400 2000),
                      bench "600 x 200"  $ nfIO $ sampleIOFixed (testTopicMHPost 600 2000),
                      bench "800 x 200"  $ nfIO $ sampleIOFixed (testTopicMHPost 800 2000),
                      bench "1000 x 200" $ nfIO $ sampleIOFixed (testTopicMHPost 1000 2000) ]
  ]