{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Tests.Examples.SIR where

import Prog
import Effects.Writer
import Model
import Inference.SIM as SIM
import Inference.MH as MH
import Sampler
import Env
import Control.Monad

import Examples.HMM
import Data.Extensible (Associated)

-- ||| (Section 3.1 + Section 5.5 extension) The SIR model

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

data TransParamsSIR = TransParamsSIR {
    betaP  :: Double, -- ^ Mean contact rate between susceptible and infected people
    gammaP :: Double  -- ^ Mean recovery rate
}

transSIR :: Member (Writer [Popl]) es -- || Writer effect from Section 5.5
  => TransModel env es TransParamsSIR Popl
transSIR (TransParamsSIR beta gamma) sir = do
  sir' <- (transSI beta >=> transIR gamma) sir
  tellM [sir'] 
  return sir'

-- | SIR observation model
type ObsParams = Double

obsSIR :: Observable env "𝜉" Int
  => ObsModel env ts Double Popl Reported
obsSIR rho (Popl _ i _)  = do
  i <- poisson (rho * fromIntegral i) #𝜉
  return i

-- | SIR transition prior
transPriorSIR :: Observables env '["β",  "γ"] Double
  => Model env ts TransParamsSIR
transPriorSIR = do
  pBeta  <- gamma 2 1 #β
  pGamma <- gamma 1 (1/8) #γ
  return (TransParamsSIR pBeta pGamma)

-- | SIR observation prior
obsPriorSIR :: Observables env '["ρ"] Double
  => Model env ts ObsParams
obsPriorSIR = do
  pRho <- beta 2 7 #ρ
  return pRho

-- | SIR as HMM
hmmSIR :: (Member (Writer [Popl]) es, Observable env "𝜉" Int, Observables env '["ρ", "β", "γ"] Double)
  => Int -> Popl -> Model env es Popl
hmmSIR = hmmGen transPriorSIR obsPriorSIR transSIR obsSIR

hmmSIR' :: (Observables env '["𝜉"] Int , Observables env '[ "β" , "γ" , "ρ"] Double) => Int -> Popl -> Model env es (Popl, [Popl])
hmmSIR' n = handleWriterM . hmmSIR n

type SIRenv = '["β" := Double, "γ"  := Double, "ρ"  := Double, "𝜉" := Int]

-- ||| (Section 3.1, Fig 4a) SIM from SIR model: ([(s, i, r)], [𝜉])
simSIR :: Int -> Sampler ([(Int, Int, Int)], [Reported])
simSIR n_days = do
  let sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #ρ := [0.3] <:> #𝜉 := [] <:> enil
      sir_0      = Popl {s = 762, i = 1, r = 0}
  ((_, sir_trace), sim_env_out) <- SIM.simulate (hmmSIR' n_days sir_0) sim_env_in 
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
      sirs = map (\(Popl s i recov) -> (s, i, recov)) sir_trace
  return (sirs, 𝜉s)

-- ||| (Section 3.3, Fig 5) Infer from SIR model: ([ρ], [β])
mhSIR :: Int -> Int -> Sampler ([Double], [Double])
mhSIR n_mhsteps n_days = do
  𝜉s <- snd <$> simSIR n_days
  let mh_env_in = #β := [] <:> #γ := [0.0085] <:> #ρ := [] <:> #𝜉 := 𝜉s <:> enil
      sir_0           = Popl {s = 762, i = 1, r = 0}
  mhTrace <- MH.mh n_mhsteps (hmmSIR' n_days sir_0) mh_env_in ["β", "ρ"]
  let ρs = concatMap (get #ρ) mhTrace
      βs = concatMap (get #β) mhTrace
  return (ρs, βs)