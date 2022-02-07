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

data LatState = LatState {
    sus   :: Int, -- ^ Number of people susceptible to infection
    inf   :: Int, -- ^ Number of people currently infected
    recov :: Int -- ^ Number of people recovered from infection
} deriving Show

type SIREnv =
  '[
     "β" ':= Double,
     "γ" ':= Double,
      "ρ" ':= Double,
      "infobs" ':= Int
   ]

type InfectionCount = Int

obsSIR :: Observable env "infobs" Int
  => Double -> LatState -> Model env ts Int
obsSIR rho (LatState _ inf _)  = do
  i <- poisson' (rho * fromIntegral inf) #infobs
  return i

transSI :: Double -> LatState -> Model env ts LatState
transSI beta (LatState sus inf rec) = do
  let pop = sus + inf + rec
  dN_SI <- binomial sus (1 - exp ((-beta * fromIntegral inf) / fromIntegral pop))
  let sus' = sus - dN_SI
      inf' = inf + dN_SI
  return $ LatState sus' inf' rec

transIR :: Double -> LatState -> Model env ts LatState
transIR gamma (LatState sus inf rec)  = do
  dN_IR <- binomial inf (1 - exp (-gamma))
  let inf' = inf - dN_IR
      rec' = rec + dN_IR
  return $ LatState sus inf' rec'

transSIR :: Member (Writer [LatState]) ts
  => Double -> Double -> LatState -> Model env ts LatState
transSIR beta gamma latentSt = do
  latentSt' <- (transSI beta >=> transIR gamma) latentSt
  tellM [latentSt']
  return latentSt'

hmmSIR :: Member (Writer [LatState]) ts
  => Observable env "infobs" Int
  => Params -> LatState -> Model env ts LatState
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
  return (Params pBeta pGamma pRho)

hmmSIRNsteps ::
     Member (Writer [LatState]) ts
  => (Observable env "infobs" Int, Observables env '["ρ", "β", "γ"] Double)
  => Int -> LatState -> Model env ts LatState
hmmSIRNsteps n latentState  = do
  params       <- paramsPrior
  latentState' <- foldl (>=>) return (replicate n $ hmmSIR params) latentState
  return latentState'
