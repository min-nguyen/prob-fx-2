module Main where
import Criterion.Main
import Extensible.TestPaper
import Extensible.Sampler




main :: IO ()
main = do
  -- benchmarkTopicSim
  benchmarkTopicLW
  benchmarkTopicMH
benchmarkLinRegrSim = defaultMain [
    bgroup "linRegrSim" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testLinRegrBasic 2000),
                        bench "4000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrBasic 4000),
                        bench "6000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrBasic 6000),
                        bench "8000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrBasic 8000),
                        bench "10000 x 100" $ nfIO $ sampleIOFixed (testLinRegrBasic 10000) ]
  ]

benchmarkLinRegrLW = defaultMain [
    bgroup "linRegrLW" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testLinRegrLWInf 2000),
                        bench "4000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrLWInf 4000),
                        bench "6000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrLWInf 6000),
                        bench "8000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrLWInf 8000),
                        bench "10000 x 100" $ nfIO $ sampleIOFixed (testLinRegrLWInf 10000) ]
  ]

benchmarkLinRegrMH = defaultMain [
    bgroup "linRegrMH" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testLinRegrMHPost 2000),
                        bench "4000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrMHPost 4000),
                        bench "6000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrMHPost 6000),
                        bench "8000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrMHPost 8000),
                        bench "10000 x 100" $ nfIO $ sampleIOFixed (testLinRegrMHPost 10000) ]
  ]

benchmarkHMMSim = defaultMain [
    bgroup "hmmSim" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testLinRegrMHPost 2000),
                    bench "4000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrMHPost 4000),
                    bench "6000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrMHPost 6000),
                    bench "8000 x 100"  $ nfIO $ sampleIOFixed (testLinRegrMHPost 8000),
                    bench "10000 x 100" $ nfIO $ sampleIOFixed (testLinRegrMHPost 10000) ]
  ]

benchmarkHMMLW = defaultMain [
    bgroup "hmmLW" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testHMMLWInf 2000),
                    bench "4000 x 100"  $ nfIO $ sampleIOFixed (testHMMLWInf 4000),
                    bench "6000 x 100"  $ nfIO $ sampleIOFixed (testHMMLWInf 6000),
                    bench "8000 x 100"  $ nfIO $ sampleIOFixed (testHMMLWInf 8000),
                    bench "10000 x 100" $ nfIO $ sampleIOFixed (testHMMLWInf 10000) ]
  ]

benchmarkHMMMH = defaultMain [
    bgroup "hmmMH" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testHMMMHPost 2000),
                    bench "4000 x 100"  $ nfIO $ sampleIOFixed (testHMMMHPost 4000),
                    bench "6000 x 100"  $ nfIO $ sampleIOFixed (testHMMMHPost 6000),
                    bench "8000 x 100"  $ nfIO $ sampleIOFixed (testHMMMHPost 8000),
                    bench "10000 x 100" $ nfIO $ sampleIOFixed (testHMMMHPost 10000) ]
  ]

benchmarkTopicSim = defaultMain [
    bgroup "topicSim" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testTopicBasic 2000),
                      bench "4000 x 100"  $ nfIO $ sampleIOFixed (testTopicBasic 4000),
                      bench "6000 x 100"  $ nfIO $ sampleIOFixed (testTopicBasic 6000),
                      bench "8000 x 100"  $ nfIO $ sampleIOFixed (testTopicBasic 8000),
                      bench "10000 x 100" $ nfIO $ sampleIOFixed (testTopicBasic 10000) ]
  ]

benchmarkTopicLW = defaultMain [
    bgroup "topicLW" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testTopicLW 2000),
                      bench "4000 x 100"  $ nfIO $ sampleIOFixed (testTopicLW 4000),
                      bench "6000 x 100"  $ nfIO $ sampleIOFixed (testTopicLW 6000),
                      bench "8000 x 100"  $ nfIO $ sampleIOFixed (testTopicLW 8000),
                      bench "10000 x 100" $ nfIO $ sampleIOFixed (testTopicLW 10000) ]
  ]


benchmarkTopicMH = defaultMain [
    bgroup "topicMH" [bench "2000 x 100" $ nfIO $ sampleIOFixed (testTopicMHPost 2000),
                      bench "4000 x 100"  $ nfIO $ sampleIOFixed (testTopicMHPost 4000),
                      bench "6000 x 100"  $ nfIO $ sampleIOFixed (testTopicMHPost 6000),
                      bench "8000 x 100"  $ nfIO $ sampleIOFixed (testTopicMHPost 8000),
                      bench "10000 x 100" $ nfIO $ sampleIOFixed (testTopicMHPost 10000) ]
  ]
