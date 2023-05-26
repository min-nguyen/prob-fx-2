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

-- | Take the log-mean-exp of a list of log-probabilities (which is -infinity if all log weights are -infinity)
logMeanExp :: [LogP] -> LogP
logMeanExp logps =
  let c = maximum logps
      n = length logps
  in  if isInfinite c
      then (-1/0)
      else c + log (sum (map (\logw -> exp (logw - c)) logps)) - log (fromIntegral n)

-- | Take the log-mean-exp of a list of log-probabilities (which is -infinity if all log weights are -infinity)
logSumExp :: [LogP] -> LogP
logSumExp logps =
  let c = maximum logps
  in  if isInfinite c   -- to avoid @-Infinity - (-Infinity)@
      then (-1/0)
      else c + log (sum (map (\logw -> exp (logw - c)) logps))

-- | Normalise log weights and return the log mean (which is -infinity if all log weights are -infinity)
normalise :: [LogP] -> [LogP]
normalise ws = map (subtract z) ws
  where n = fromIntegral (length ws)
        z = logSumExp ws

normaliseAndLogMean :: [LogP] -> ([LogP], LogP)
normaliseAndLogMean ws = (map (subtract z) ws, z - log n)
  where n = fromIntegral (length ws)
        z = logSumExp ws