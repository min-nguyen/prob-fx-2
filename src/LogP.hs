{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}


{- To be explicit that a @Double@ is a log-probability.
-}

module LogP (
    LogP(..)
  , logMeanExp
  , normaliseLogPs
  ) where

import Debug.Trace

-- | The type of log-probabilities.
--   All numerics work the same as with @Double@.
type LogP =  Double

-- | Take the log-mean-exp of a list of log-probabilities
logMeanExp :: [LogP] -> LogP
logMeanExp logps =
  let c = maximum logps
  in  if isInfinite c   -- to avoid @-Infinity - (-Infinity)@
      then (-1/0)
      else c + log ((1.0/fromIntegral (length logps)) * sum (map (\logw -> exp (logw - c)) logps))

-- | Scale all log-probabilities by setting the maximum probability to 1
normaliseLogPs :: [LogP] -> [LogP]
normaliseLogPs xs = if isInfinite c then map (const (-1/0)) xs else map (\x -> x - c) xs
  where c = maximum xs