module Main where
import Criterion.Main
import Extensible.Test
import Extensible.Sampler

main :: IO ()
main = defaultMain [
    bgroup "testLR" [bench "" $ whnfIO $ sampleIOFixed testLinRegrMHPost]
  ]
