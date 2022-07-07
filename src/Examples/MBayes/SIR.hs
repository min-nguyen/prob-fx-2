{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE RankNTypes #-}
module Examples.MBayes.SIR where

import Prog
import Effects.ObsReader
import Effects.Writer
import Model
import Inference.MBAYES
import Control.Monad.Bayes.Class (MonadInfer)
import Control.Monad.Bayes.Weighted ( prior, runWeighted )
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Traced
import Effects.Lift
import Trace
import Env
import Control.Monad

import Examples.HMM
import Data.Extensible (Associated)

{- (Sec 3.1) SIR model -}
type SIRenv = '["β" := Double, "γ"  := Double, "ρ"  := Double, "𝜉" := Int]

data Popl = Popl {
    s   :: Int, -- ^ Number of people susceptible to infection
    i   :: Int, -- ^ Number of people currently infected
    r   :: Int  -- ^ Number of people recovered from infection
} deriving Show

type Reported = Int

-- | SIR transition model
transSI :: TransModel env es Double Popl
transSI beta (Popl s i r) = do
  let pop = s + i + r
  dN_SI <- binomial' s (1 - exp ((-beta * fromIntegral i) / fromIntegral pop))
  pure $ Popl (s - dN_SI) (i + dN_SI) r

transIR :: TransModel env es Double Popl
transIR gamma (Popl s i r)  = do
  dN_IR <- binomial' i (1 - exp (-gamma))
  pure $ Popl s (i - dN_IR) (r + dN_IR)

data TransParams = TransParams {
    betaP  :: Double, -- ^ Mean contact rate between susceptible and infected people
    gammaP :: Double  -- ^ Mean recovery rate
}

transSIR :: Member (Writer [Popl]) es => TransModel env es TransParams Popl
transSIR (TransParams beta gamma) latentSt = do
  latentSt' <- (transSI beta >=> transIR gamma) latentSt
  tellM [latentSt']
  pure latentSt'

-- | SIR observation model
type ObsParams = Double

obsSIR :: Observable env "𝜉" Int
  => ObsModel env es Double Popl Reported
obsSIR rho (Popl _ i _)  = do
  i <- poisson (rho * fromIntegral i) #𝜉
  pure i

-- | SIR transition prior
transPrior :: Observables env '["β",  "γ"] Double
  => Model env es TransParams
transPrior = do
  pBeta  <- gamma 2 1 #β
  pGamma <- gamma 1 (1/8) #γ
  pure (TransParams pBeta pGamma)

-- | SIR observation prior
obsPrior :: Observables env '["ρ"] Double
  => Model env es ObsParams
obsPrior = do
  pRho <- beta 2 7 #ρ
  pure pRho

-- | SIR as HMM
hmmSIR :: ( Member (Writer [Popl]) es, Observable env "𝜉" Int, Observables env '["ρ", "β", "γ"] Double)
  => Int -> Popl -> Model env es Popl
hmmSIR = hmmGen transPrior obsPrior transSIR obsSIR

hmmSIR' :: (Observables env '["𝜉"] Int , Observables env '[ "β" , "γ" , "ρ"] Double) => Int -> Popl -> Model env es (Popl, [Popl])
hmmSIR' n popl = handleWriterM $ hmmSIR n popl

mbayesSIR :: (FromSTrace env, MonadInfer m, Observables env '["𝜉"] Int , Observables env '[ "β" , "γ" , "ρ"] Double) => 
 Int -> Popl -> Env env -> m ((Popl, [Popl]), Env env)
mbayesSIR n popl = toMBayes (hmmSIR' n popl)

-- | Executing SIR
infobs_data :: [Int]
infobs_data = [0,1,4,2,1,3,3,5,10,11,30,23,48,50,91,94,129,151,172,173,198,193,214,179,229,172,205,211,191,212,185,184,173,211,185,197,176,169,198,174,163,197,152,198,153,164,154,167,178,174,160,149,140,172,169,144,137,151,166,151,147,149,159,150,151,139,137,182,121,119,133,146,141,136,126,131,124,110,120,113,117,102,120,117,122,121,110,125,127,117,117,98,109,108,108,120,98,103,104,103]

simSIRMB :: IO ([(Int, Int, Int)], [Reported])
simSIRMB = do
  let sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #ρ := [0.3] <:> #𝜉 := [] <:> ENil
      sir_0      = Popl {s = 762, i = 1, r = 0}
  ((_, sir_trace), sim_env_out) <- sampleIO $ prior (mbayesSIR 100 sir_0 sim_env_in)
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
      sirs = map (\(Popl s i recov) -> (s, i, recov)) sir_trace
  pure (sirs, 𝜉s)

mhSIRMB :: IO ([Double], [Double]) -- [(Popl, Env SIRenv)]
mhSIRMB = do
  let sir_0      = Popl {s = 762, i = 1, r = 0}
      env = #β := [] <:> #γ := [0.0085] <:> #ρ := [] <:> #𝜉 := infobs_data <:> eNil
  (_, env) <- unzip <$> sampleIO (prior $ mh 100 (mbayesSIR 100 sir_0 env))
  let ρs = concatMap (get #ρ) env
      βs = concatMap (get #β) env
  pure (ρs, βs)