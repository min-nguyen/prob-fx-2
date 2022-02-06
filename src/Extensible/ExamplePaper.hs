{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications, UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedLabels #-}
module Extensible.ExamplePaper where

import Statistics.Distribution
import GHC.OverloadedLabels
import Extensible.Freer
import Extensible.Reader
import Extensible.State
import Extensible.Writer
import Extensible.Model
import Extensible.Dist
import Extensible.IO
import Extensible.Sampler
import Extensible.OpenSum (OpenSum)
import qualified Extensible.OpenSum as OpenSum
import Control.Monad
import Data.List as List
import Unsafe.Coerce
import Data.Maybe
import Data.Kind (Constraint)
import GHC.TypeLits
import Data.Typeable
import Extensible.OpenProduct
import Util
import Data.Vector.Fusion.Bundle (findIndex)
import GHC.Show (Show)
import qualified Data.Map as Map

data Params = Params {
    betaP  :: Double, -- ^ Mean contact rate between susceptible and infected people
    gammaP :: Double, -- ^ Mean recovery rate
    rhoP   :: Double -- ^ Rate of detection
}

data LatentState = LatentState {
    sus   :: Int, -- ^ Number of people susceptible to infection
    inf   :: Int, -- ^ Number of people currently infected
    recov :: Int -- ^ Number of people recovered from infection
} deriving Show

type SIREnv =
  '[ "infobs" ':= Int,
     "ρ" ':= Double,
     "β" ':= Double,
     "γ" ':= Double
   ]

type InfectionCount = Int

obsSIR :: Observable env "infobs" Int
  => Double -> LatentState -> Model env ts Int
obsSIR rho (LatentState _ inf _)  = do
  poisson' (rho * fromIntegral inf) #infobs

transSI :: Double -> LatentState -> Model env ts LatentState
transSI beta (LatentState sus inf rec) = do
  let pop = sus + inf + rec
  dN_SI <- binomial sus (1 - exp ((-beta * fromIntegral inf) / fromIntegral pop))
  let sus' = sus - dN_SI
      inf' = inf + dN_SI
  return $ LatentState sus' inf' rec

transIR :: Double -> LatentState -> Model env ts LatentState
transIR gamma (LatentState sus inf rec)  = do
  dN_IR <- binomial sus (-gamma)
  let inf' = inf - dN_IR
      rec' = rec + dN_IR
  return $ LatentState sus inf' rec'

transSIR ::  Double -> Double -> LatentState ->Model env ts LatentState
transSIR beta gamma = transSI beta >=> transIR gamma

hmmSIR :: Observable env "infobs" Int
  => Params -> LatentState -> Model env ts LatentState
hmmSIR  (Params beta gamma rho) latentState = do
  latentState'   <- transSIR beta gamma latentState
  infectionCount <- obsSIR rho latentState'
  return latentState'

paramsPrior :: Observables env '["ρ", "β", "γ"] Double
  => Model env ts Params
paramsPrior = do
  pRho   <- beta' 2 7 #ρ
  pBeta  <- gamma' 2 1 #β
  pGamma <- gamma' 1 (1/8) #γ
  return (Params pRho pBeta pGamma)

hmmSIRNsteps :: (Observable env "infobs" Int, Observables env '["ρ", "β", "γ"] Double)
  => Int -> LatentState -> Model env ts LatentState
hmmSIRNsteps n latentState  = do
  params       <- paramsPrior
  latentState' <- foldl (>=>) return (replicate n $ hmmSIR params) latentState
  return latentState'
