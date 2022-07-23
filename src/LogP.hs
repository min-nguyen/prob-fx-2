{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- Helper definitions for working with log-probability.
-}

module LogP (
    LogP(..)
  , logMeanExp
  ) where

-- | To be explicit that a @Double@ is a log-probability
newtype LogP = LogP { logP :: Double } deriving (Show, Num, Eq, Ord, Fractional)

-- | Take the log-mean-exp of a list of log-probabilities
logMeanExp :: [LogP] -> LogP
logMeanExp logps =
  let logws = map logP logps
      c = maximum logws
  in  if isInfinite c   -- to avoid @-Infinity - (-Infinity)@
      then (-1/0)
      else LogP $ c + log ((1.0/fromIntegral (length logws)) * sum (map (\logw -> exp (logw - c)) logws))
