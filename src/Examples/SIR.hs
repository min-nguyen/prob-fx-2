{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use camelCase" #-}
module Examples.SIR where

import Prog
import Effects.ObsReader
import Effects.Writer
import Model
import Inference.SIM as Simulate
import Inference.MH as MH
import Effects.Lift
import Sampler
import Env
import Control.Monad
import Env

import Examples.HMM
import Data.Extensible (Associated)

{- (Sec 3.1) SIR model -}
data Popl = Popl {
    s   :: Int, -- ^ Number of people susceptible to infection
    i   :: Int, -- ^ Number of people currently infected
    r   :: Int  -- ^ Number of people recovered from infection
} deriving Show

type Reported = Int

-- | SIR transition model
transSI :: TransModel env ts Double Popl
transSI beta (Popl s i r) = do
  let pop = s + i + r
  dN_SI <- binomial' s (1 - exp ((-beta * fromIntegral i) / fromIntegral pop))
  return $ Popl (s - dN_SI) (i + dN_SI) r

transIR :: TransModel env ts Double Popl
transIR gamma (Popl s i r)  = do
  dN_IR <- binomial' i (1 - exp (-gamma))
  return $ Popl s (i - dN_IR) (r + dN_IR)

data TransParams = TransParams {
    betaP  :: Double, -- ^ Mean contact rate between susceptible and infected people
    gammaP :: Double  -- ^ Mean recovery rate
}

transSIR :: Member (Writer [Popl]) es
  => TransModel env es TransParams Popl
transSIR (TransParams beta gamma) latentSt = do
  latentSt' <- (transSI beta >=> transIR gamma) latentSt
  tellM [latentSt']
  return latentSt'

-- | SIR observation model
type ObsParams = Double

obsSIR :: Observable env "𝜉" Int
  => ObsModel env ts Double Popl Reported
obsSIR rho (Popl _ i _)  = do
  i <- poisson (rho * fromIntegral i) #𝜉
  return i

-- | SIR transition prior
transPrior :: Observables env '["β",  "γ"] Double
  => Model env ts TransParams
transPrior = do
  pBeta  <- gamma 2 1 #β
  pGamma <- gamma 1 (1/8) #γ
  return (TransParams pBeta pGamma)

-- | SIR observation prior
obsPrior :: Observables env '["ρ"] Double
  => Model env ts ObsParams
obsPrior = do
  pRho <- beta 2 7 #ρ
  return pRho

-- | SIR as HMM
hmmSIR :: (Member (Writer [Popl]) es, Observable env "𝜉" Int, Observables env '["ρ", "β", "γ"] Double)
  => Int -> Popl -> Model env es Popl
hmmSIR = hmmGen transPrior obsPrior transSIR obsSIR

hmmSIR' :: (Observables env '["𝜉"] Int , Observables env '[ "β" , "γ" , "ρ"] Double) => Int -> Popl -> Model env es (Popl, [Popl])
hmmSIR' n = handleWriterM . hmmSIR n

type SIRenv = '["β" := Double, "γ"  := Double, "ρ"  := Double, "𝜉" := Int]

-- | Simulate from SIR model: ([(s, i, r)], [𝜉])
simSIR :: Sampler ([(Int, Int, Int)], [Reported])
simSIR = do
  let sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #ρ := [0.3] <:> #𝜉 := [] <:> ENil
      sir_0      = Popl {s = 762, i = 1, r = 0}
  ((_, sir_trace), sim_env_out) <- Simulate.simulate (hmmSIR' 100 sir_0) sim_env_in
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
      sirs = map (\(Popl s i recov) -> (s, i, recov)) sir_trace
  return (sirs, 𝜉s)

infobs_data :: [Int]
infobs_data = [0,1,4,2,1,3,3,5,10,11,30,23,48,50,91,94,129,151,172,173,198,193,214,179,229,172,205,211,191,212,185,184,173,211,185,197,176,169,198,174,163,197,152,198,153,164,154,167,178,174,160,149,140,172,169,144,137,151,166,151,147,149,159,150,151,139,137,182,121,119,133,146,141,136,126,131,124,110,120,113,117,102,120,117,122,121,110,125,127,117,117,98,109,108,108,120,98,103,104,103]

-- | Infer from SIR model: ([ρ], [β])
mhSIR :: Sampler ([Double], [Double])
mhSIR = do
  𝜉s <- snd <$> simSIR
  let mh_env_in = #β := [] <:> #γ := [0.0085] <:> #ρ := [] <:> #𝜉 := 𝜉s <:> eNil
      sir_0           = Popl {s = 762, i = 1, r = 0}
  mhTrace <- MH.mhTopLevel 50000 (handleWriterM @[Popl] $ hmmSIR' 100 sir_0) mh_env_in (#β ⋮ #ρ ⋮ONil)
  let ρs = concatMap (get #ρ) mhTrace
      βs = concatMap (get #β) mhTrace
  return (ρs, βs)

{- (3.2.1) EXTENSIONS TO SIR MODEL -}

{- SIRS (resusceptible) model -}
data TransParams' = TransParams' {
    betaP'  :: Double, -- ^ Mean contact rate between susceptible and infected people
    gammaP' :: Double, -- ^ Mean recovery rate
    etaP'   :: Double  -- ^ Rate of resusceptible
}

-- | SIRS transition model
transRS :: Double -> Popl -> Model env ts Popl
transRS eta (Popl s i r) = do
  dN_RS <- binomial' r (1 - exp (-eta))
  return $ Popl (s + dN_RS) i (r - dN_RS)

transSIRS :: Member (Writer [Popl]) es
  => TransModel env es TransParams' Popl
transSIRS (TransParams' beta gamma eta) latentSt = do
  latentSt' <- (transSI beta >=> transIR gamma >=> transRS eta) latentSt
  tellM [latentSt']
  return latentSt'

-- | SIR transition prior
transPrior' :: Observables env '["β", "η", "γ"] Double
  => Model env ts TransParams'
transPrior' = do
  TransParams pBeta pGamma  <- transPrior
  pEta <- gamma 1 (1/8) #η
  return (TransParams' pBeta pGamma pEta)

-- | SIRS as HMM
hmmSIRS :: (Observables env '["𝜉"] Int, Observables env '["β", "η", "γ", "ρ"] Double) => Int -> Popl -> Model env ts (Popl, [Popl])
hmmSIRS n = handleWriterM . hmmGen transPrior' obsPrior transSIRS obsSIR n

-- | Simulate from SIRS model: ([(s, i, r)], [𝜉])
simSIRS :: Sampler ([(Int, Int, Int)], [Reported])
simSIRS = do
  let sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #η := [0.05] <:> #ρ := [0.3] <:> #𝜉 := [] <:> eNil
      sir_0      = Popl {s = 762, i = 1, r = 0}
  ((_, sir_trace), sim_env_out) <- Simulate.simulate (hmmSIRS 100 sir_0) sim_env_in
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
      sirs = map (\(Popl s i recov) -> (s, i, recov)) sir_trace
  return (sirs, 𝜉s)

{- SIRSV (resusceptible + vacc) model -}
data TransParams'' = TransParams'' {
    betaP''  :: Double, -- ^ Mean contact rate between susceptible and infected people
    gammaP'' :: Double, -- ^ Mean recovery rate
    etaP''   :: Double, -- ^ Rate of resusceptible
    omegaP'' :: Double  -- ^ Vaccination rate
}

data Popl' = Popl' {
    s' :: Int,
    i' :: Int,
    r' :: Int,
    v' :: Int
} deriving Show

-- | SIRSV transition model
transSI' :: TransModel env ts Double Popl'
transSI' beta (Popl' s i r v) = do
  let pop = s + i + r + v
  dN_SI <- binomial' s (1 - exp ((-beta * fromIntegral i) / fromIntegral pop))
  return $ Popl' (s - dN_SI) (i + dN_SI) r v

transIR' :: TransModel env ts Double Popl'
transIR' gamma (Popl' s i r v)  = do
  dN_IR <- binomial' i (1 - exp (-gamma))
  return $ Popl' s (i - dN_IR) (r + dN_IR) v

transRS' :: TransModel env es Double Popl'
transRS' eta (Popl' s i r v) = do
  dN_RS <- binomial' r (1 - exp (-eta))
  return $ Popl' (s + dN_RS) i (r - dN_RS) v

transSV' :: TransModel env es Double Popl'
transSV' omega (Popl' s i r v)  = do
  dN_SV <- binomial' s (1 - exp (-omega))
  return $  Popl' (s - dN_SV) i r (v + dN_SV )

transSIRSV :: Member (Writer [Popl']) ts => TransModel env ts TransParams'' Popl'
transSIRSV (TransParams'' beta gamma omega eta) latentSt = do
  latentSt' <- (transSI' beta  >=>
                transIR' gamma >=>
                transRS' eta   >=>
                transSV' omega) latentSt
  tellM [latentSt']
  return latentSt'

-- | SIRSV transition prior
transPrior'' :: Observables env '["β", "γ", "ω", "η"] Double
  => Model env ts TransParams''
transPrior''  = do
  TransParams' pBeta pGamma pEta <- transPrior'
  pOmega <- gamma 1 (1/16) #ω
  return (TransParams'' pBeta pGamma pEta pOmega)

-- | SIRSV observation model
obsSIRSV :: Observable env "𝜉" Int
  => ObsModel env ts Double Popl' Reported
obsSIRSV rho (Popl' _ i _ v)  = do
  i <- poisson (rho * fromIntegral i) #𝜉
  return i

-- | SIRSV as HMM
hmmSIRSV ::  (Observables env '["𝜉"] Int, Observables env '["β", "γ", "η", "ω", "ρ"] Double) => Int -> Popl' -> Model env ts (Popl', [Popl'])
hmmSIRSV n = handleWriterM . hmmGen transPrior'' obsPrior transSIRSV obsSIRSV n

-- | Simulate from SIRSV model : ([(s, i, r, v)], [𝜉])
simSIRSV :: Sampler ([(Int, Int, Int, Int)], [Reported])
simSIRSV = do
  let sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #η := [0.05] <:> #ω := [0.02] <:> #ρ := [0.3] <:> #𝜉 := [] <:> eNil
      sirv_0      = Popl' {s' = 762, i' = 1, r' = 0, v' = 0}
  ((_, sirv_trace), sim_env_out) <- Simulate.simulate (hmmSIRSV 100 sirv_0) sim_env_in
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
      sirvs = map (\(Popl' s i recov v) -> (s, i, recov, v)) sirv_trace
  return (sirvs, 𝜉s)