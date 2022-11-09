{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- To be explicit that a @Double@ is a log-probability.
-}

module LogP (
    LogP(..)
  , expLogP
  , fromLogP
  , logMeanExp
  , normaliseLogPs
  ) where

import Debug.Trace

-- | The type of log-probabilities.
--   All numerics work the same as with @Double@.
newtype LogP = LogP Double deriving (Num, Eq, Ord, Fractional, Floating)

fromLogP :: LogP -> Double
fromLogP (LogP logp) = logp

expLogP :: LogP -> Double
expLogP (LogP logP) = exp logP

instance Show LogP where
  show :: LogP -> String
  show (LogP p) = "LogP {" ++ show (p, exp p) ++ "}"

-- | Take the log-mean-exp of a list of log-probabilities
logMeanExp :: [LogP] -> LogP
logMeanExp logps =
  let logws = map fromLogP logps
      c     = maximum logws
  in  if isInfinite c   -- to avoid @-Infinity - (-Infinity)@
      then (-1/0)
      else LogP $ c + log ((1.0/fromIntegral (length logws)) * sum (map (\logw -> exp (logw - c)) logws))

-- | Scale all log-probabilities by setting the maximum probability to 1
normaliseLogPs :: [LogP] -> [LogP]
normaliseLogPs xs = if isInfinite (fromLogP c) then map (const (-1/0)) xs else map (\x -> x - c) xs
  where c = maximum xs