{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LogP where

newtype LogP = LogP { logP :: Double } deriving (Show, Num, Eq, Ord, Fractional)

logMeanExp :: [LogP] -> LogP
logMeanExp logps =
  let logws = map logP logps
      c = maximum logws
  in  if isInfinite c -- to avoid "-Infinity - (-Infinity)"
      then (-1/0)
      else LogP $ c + log ((1.0/fromIntegral (length logws)) * sum (map (\logw -> exp (logw - c)) logws))
