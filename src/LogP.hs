{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}


{- To be explicit that a @Double@ is a log-probability.
-}

module LogP (
    LogP(..)
  , logMeanExp
  , logSumExp
  , normalise
  , normaliseAndLogMean
  ) where

import Debug.Trace

-- | The type of log-probabilities.
--   All numerics work the same as with @Double@.
type LogP =  Double

-- | Take the logarithm of a mean of probabilities (which is -infinity if all probabilities are 0)
logMeanExp :: [LogP] -> LogP
logMeanExp ws =
  let c = maximum ws
      n = length ws
  in  if isInfinite c
      then (-1/0)
      else c + log (sum (map (\w -> exp (w - c)) ws)) - log (fromIntegral n)

-- | Take the logarithm of a sum of probabilities (which is -infinity if all probabilities are 0)
logSumExp :: [LogP] -> LogP
logSumExp ws =
  let c = maximum ws
  in  if isInfinite c   -- to avoid @-Infinity - (-Infinity)@
      then (-1/0)
      else c + log (sum (map (\w -> exp (w - c)) ws))

-- | Take the logarithms of normalised probabilities (which are all -infinity if all probabilities are 0)
normalise :: [LogP] -> [LogP]
normalise ws = map norm ws
  where n    = fromIntegral (length ws)
        z    = logSumExp ws
        norm = if isInfinite z then const (-1/0) else subtract z

normaliseAndLogMean :: [LogP] -> ([LogP], LogP)
normaliseAndLogMean ws = (map norm ws, z - log n)
  where n = fromIntegral (length ws)
        z = logSumExp ws
        norm = if isInfinite z then const (-1/0) else subtract z

