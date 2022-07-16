{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

module CoinFlip where

import Prog ( call )
import Effects.ObsReader ( ObsReader(Ask) )
import Model ( Model(Model), bernoulli, uniform )
import PrimDist ( PrimDist(Bernoulli, Uniform) )
import Effects.Dist ( Dist(Dist) )
import Data.Kind (Constraint)
import Env ( Observables )

-- | Coin flip model
coinFlip :: (Observables env '["p"] Double, Observables env '[ "y"] Bool) => Model env es Bool
coinFlip = do
  p <- uniform 0 1 #p
  y <- bernoulli p #y
  return y

-- | Desugared version of coin flip model
coinFlip' :: forall env es. (Observables env '["p"] Double, Observables env '[ "y"] Bool) => Model env es Bool
coinFlip' = Model $ do
  maybe_p  <- call (Ask @env #p)
  p        <- call (Dist (Uniform 0 1) maybe_p (Just "p"))
  maybe_y  <- call (Ask @env #y)
  y        <- call (Dist (Bernoulli p) maybe_y (Just "p") )
  return y
