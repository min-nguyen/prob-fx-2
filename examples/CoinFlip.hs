

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

{- | A coin-flip model for demonstrating how primitive distributions desugar.
-}

module CoinFlip where

import Comp ( call )
import Effects.EnvRW ( EnvRW(EnvRead) )
import Model ( MulModel(MulModel), bernoulli, uniform )
import PrimDist ( mkBernoulli, mkUniform )
import Effects.MulDist ( MulDist(MulDist) )
import Data.Kind (Constraint)
import Env ( Observables )

{- | A coin-flip model that draws a coin-bias @p@ between 0 and 1 from a uniform
     distribution, and uses this to draw a boolean @y@ representing heads or tails.
-}
coinFlip
  :: (Observables env '["p"] Double
    , Observables env '[ "y"] Bool)
  => MulModel env es Bool
coinFlip = do
  p <- uniform 0 1 #p
  y <- bernoulli p #y
  return y

{- | A desugared version of the above coin-flip model, after inlining the functions
     @uniform@ and @bernoulli@.
-}
coinFlip'
  :: forall env es. (Observables env '["p"] Double, Observables env '[ "y"] Bool)
  => MulModel env es Bool
coinFlip' = MulModel $ do
  maybe_p  <- call (EnvRead @env #p)
  p        <- call (MulDist (mkUniform 0 1) maybe_p (Just "p"))
  maybe_y  <- call (EnvRead @env #y)
  y        <- call (MulDist (mkBernoulli p) maybe_y (Just "p") )
  return y
