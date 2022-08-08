{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}

{- |
     This demonstrates:
      - The [SIR](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology) model for modelling
        the transition between Susceptible (S), Infected (I), and Recovered (R) individuals during an epidemic.
        We model this as a Hidden Markov Model, where the _latent states_ are the true values of S, I, and R,
        and the _observations_ are the reported number of infections (𝜉).
      - Extending the SIR to the SIRS model where recovered individuals (R) can become susceptible (S) again.
      - Extending the SIRS to the SIRSV model where susceptible individuals (S) can become vaccinated (V).

      Note that the extensions (SIRS and SIRSV) aren't as modular as we would like, due to having to
      redefine the data types Popl and TransParams when adding new variables to the SIR model.
      The file [SIRModular](examples/SIR.hs) shows how one could take steps to resolve this by using
      extensible records.
-}

module SIRNonModular where

import Prog ( Member )
import Effects.Writer ( Writer, tellM, handleWriterM )
import Model ( Model, beta, binomial', gamma, poisson )
import Control.Monad ( (>=>) )
import Env ( Env(..), Observables, Observable, Assign ((:=)), (<:>), enil, (<#>), vnil, get)
import HMM ( ObsModel, TransModel, hmmGen )
import GHC.TypeLits ( Symbol )
import Data.Kind (Constraint)
import Sampler ( Sampler )
import Inference.SIM as SIM ( simulate )
import Inference.MH as MH ( mh )
import Inference.MB as MB ( toMBayes )
import qualified Control.Monad.Bayes.Class as Bayes
import qualified Control.Monad.Bayes.Weighted as Bayes
import qualified Control.Monad.Bayes.Traced as Bayes
import qualified Control.Monad.Bayes.Sampler as Bayes

{- | SIR model.
-}
-- | SIR model environment
type SIRenv =
 '[ "β"  := Double  -- ^ mean contact rate between susceptible and infected people
  , "γ"  := Double  -- ^ mean recovery rate
  , "ρ"  := Double  -- ^ mean report rate of infection
  , "𝜉"  := Int     -- ^ number of reported infections
 ]

-- | Latent state
data Popl = Popl {
    s   :: Int, -- ^ number of people susceptible to infection
    i   :: Int, -- ^ number of people currently infected
    r   :: Int  -- ^ number of people recovered from infection
} deriving Show

-- | Transition model parameters
data TransParamsSIR = TransParamsSIR {
    betaP  :: Double, -- ^ mean contact rate between susceptible and infected people
    gammaP :: Double  -- ^ mean recovery rate
}

-- | Observation 𝜉
type Reported = Int

-- | Observation model parameters
type ObsParams = Double

-- | Transition model prior
transPriorSIR :: Observables env '["β",  "γ"] Double
  => Model env ts TransParamsSIR
transPriorSIR = do
  pBeta  <- gamma 2 1 #β
  pGamma <- gamma 1 (1/8) #γ
  return (TransParamsSIR pBeta pGamma)

-- | Transition model between S and I
transSI :: TransModel env ts Double Popl
transSI beta (Popl s i r) = do
  let pop = s + i + r
  dN_SI <- binomial' s (1 - exp ((-beta * fromIntegral i) / fromIntegral pop))
  return $ Popl (s - dN_SI) (i + dN_SI) r

-- | Transition model between I and R
transIR :: TransModel env ts Double Popl
transIR gamma (Popl s i r)  = do
  dN_IR <- binomial' i (1 - exp (-gamma))
  return $ Popl s (i - dN_IR) (r + dN_IR)

-- | Transition model between S, I, and R
transSIR :: Member (Writer [Popl]) es
  => TransModel env es TransParamsSIR Popl
transSIR (TransParamsSIR beta gamma) sir = do
  sir' <- (transSI beta >=> transIR gamma) sir
  tellM [sir']  -- a user effect for writing each latent SIR state to a stream [Popl]
  return sir'

-- | Observation model prior
obsPriorSIR :: Observables env '["ρ"] Double
  => Model env ts ObsParams
obsPriorSIR = do
  pRho <- beta 2 7 #ρ
  return pRho

-- | Observation model from I to 𝜉
obsSIR :: Observable env "𝜉" Int
  => ObsModel env ts Double Popl Reported
obsSIR rho (Popl _ i _)  = do
  i <- poisson (rho * fromIntegral i) #𝜉
  return i

-- | SIR as HMM
hmmSIR :: (Member (Writer [Popl]) es, Observable env "𝜉" Int, Observables env '["ρ", "β", "γ"] Double)
  => Int -> Popl -> Model env es Popl
hmmSIR = hmmGen transPriorSIR obsPriorSIR transSIR obsSIR

-- | Handle the user effect for writing each SIR state to a stream [Popl]
hmmSIR' :: (Observables env '["𝜉"] Int , Observables env '[ "β" , "γ" , "ρ"] Double) => Int -> Popl -> Model env es (Popl, [Popl])
hmmSIR' n = handleWriterM . hmmSIR n

-- | Simulating from SIR model: ([(s, i, r)], [𝜉])
simSIR :: Int -> Sampler ([(Int, Int, Int)], [Reported])
simSIR n_days = do
  -- Specify model input of 762 susceptible and 1 infected
  let sir_0      = Popl {s = 762, i = 1, r = 0}
  -- Specify model environment
      env_in = #β := [0.7] <:> #γ := [0.009] <:> #ρ := [0.3] <:> #𝜉 := [] <:> enil
  -- Simulate an epidemic over 100 days
  ((_, sir_trace), env_out) <- SIM.simulate (hmmSIR' 100 sir_0) env_in
  -- Get the observed infections over 100 days
  let 𝜉s :: [Reported] = get #𝜉 env_out
  -- Get the true SIR values over 100 days
      sirs = map (\(Popl s i recov) -> (s, i, recov)) sir_trace
  return (sirs, 𝜉s)

-- | MH inference from SIR model: ([ρ], [β])
mhSIR :: Int -> Int -> Sampler ([Double], [Double])
mhSIR n_mhsteps n_days = do
  𝜉s <- snd <$> simSIR n_days
  -- Specify model input of 762 susceptible and 1 infected
  let sir_0           = Popl {s = 762, i = 1, r = 0}
  -- Specify model environment
      mh_env_in = #β := [] <:> #γ := [0.0085] <:> #ρ := [] <:> #𝜉 := 𝜉s <:> enil
  -- Run MH inference over 50000 iterations
  mhTrace <- MH.mh 5000 (hmmSIR' 100 sir_0) mh_env_in (#β <#> #ρ <#> vnil)
  -- Get the sampled values for model parameters ρ and β
  let ρs = concatMap (get #ρ) mhTrace
      βs = concatMap (get #β) mhTrace
  return (ρs, βs)

{- | SIRS model.
-}
-- | Transition model parameters
data TransParamsSIRS = TransParamsSIRS {
    betaP_SIRS  :: Double, -- ^ mean contact rate between susceptible and infected people
    gammaP_SIRS :: Double, -- ^ mean recovery rate
    etaP_SIRS   :: Double  -- ^ rate of resusceptible
}

-- | Transition model prior
transPriorSIRS :: Observables env '["β", "η", "γ"] Double
  => Model env ts TransParamsSIRS
transPriorSIRS = do
  TransParamsSIR pBeta pGamma  <- transPriorSIR
  pEta <- gamma 1 (1/8) #η
  return (TransParamsSIRS pBeta pGamma pEta)

-- | Transition model between R and S
transRS :: Double -> Popl -> Model env ts Popl
transRS eta (Popl s i r) = do
  dN_RS <- binomial' r (1 - exp (-eta))
  return $ Popl (s + dN_RS) i (r - dN_RS)

-- | Transition model between S, to I, to R, and to S
transSIRS :: Member (Writer [Popl]) es
  => TransModel env es TransParamsSIRS Popl
transSIRS (TransParamsSIRS beta gamma eta) sir = do
  sir' <- (transSI beta >=> transIR gamma >=> transRS eta) sir
  tellM [sir']
  return sir'

-- | SIRS as HMM
hmmSIRS :: (Observables env '["𝜉"] Int, Observables env '["β", "η", "γ", "ρ"] Double) => Int -> Popl -> Model env ts (Popl, [Popl])
hmmSIRS n = handleWriterM . hmmGen transPriorSIRS obsPriorSIR transSIRS obsSIR n

-- | Simulate from SIRS model: ([(s, i, r)], [𝜉])
simSIRS :: Sampler ([(Int, Int, Int)], [Reported])
simSIRS = do
  -- Specify model input of 762 susceptible and 1 infected
  let sir_0      = Popl {s = 762, i = 1, r = 0}
  -- Specify model environment
      env_in = #β := [0.7] <:> #γ := [0.009] <:> #η := [0.05] <:> #ρ := [0.3] <:> #𝜉 := [] <:> enil
  -- Simulate an epidemic over 100 days
  ((_, sir_trace), env_out) <- SIM.simulate (hmmSIRS 100 sir_0) env_in
  -- Get the observed infections over 100 days
  let 𝜉s :: [Reported] = get #𝜉 env_out
  -- Get the true SIR values over 100 days
      sirs = map (\(Popl s i recov) -> (s, i, recov)) sir_trace
  return (sirs, 𝜉s)


{- | SIRSV model.
-}
-- | Transition model parameters
data TransParamsSIRSV = TransParamsSIRSV {
    betaP_SIRSV  :: Double, -- ^ mean contact rate between susceptible and infected people
    gammaP_SIRSV :: Double, -- ^ mean recovery rate
    etaP_SIRSV   :: Double, -- ^ rate of resusceptible
    omegaP_SIRSV :: Double  -- ^ vaccination rate
}

-- | Latent state
data PoplV = PoplV {
    s' :: Int,  -- ^ susceptible individuals
    i' :: Int,  -- ^ infected individuals
    r' :: Int,  -- ^ recovered individuals
    v' :: Int   -- ^ vaccinated individuals
} deriving Show

-- | Transition from S to I
transSI' :: TransModel env ts Double PoplV
transSI' beta (PoplV s i r v) = do
  let pop = s + i + r + v
  dN_SI <- binomial' s (1 - exp ((-beta * fromIntegral i) / fromIntegral pop))
  return $ PoplV (s - dN_SI) (i + dN_SI) r v

-- | Transition from I to R
transIR' :: TransModel env ts Double PoplV
transIR' gamma (PoplV s i r v)  = do
  dN_IR <- binomial' i (1 - exp (-gamma))
  return $ PoplV s (i - dN_IR) (r + dN_IR) v

-- | Transition from R to S
transRS' :: TransModel env es Double PoplV
transRS' eta (PoplV s i r v) = do
  dN_RS <- binomial' r (1 - exp (-eta))
  return $ PoplV (s + dN_RS) i (r - dN_RS) v

-- | Transition from S to V
transSV' :: TransModel env es Double PoplV
transSV' omega (PoplV s i r v)  = do
  dN_SV <- binomial' s (1 - exp (-omega))
  return $  PoplV (s - dN_SV) i r (v + dN_SV )

-- | Transition between S to I, I to R, R to S, and S to V
transSIRSV :: Member (Writer [PoplV]) ts => TransModel env ts TransParamsSIRSV PoplV
transSIRSV (TransParamsSIRSV beta gamma omega eta) sirv = do
  sirv' <- (transSI' beta  >=>
            transIR' gamma >=>
            transRS' eta   >=>
            transSV' omega) sirv
  tellM [sirv']
  return sirv'

-- | Transition model prior
transPriorSIRSV :: Observables env '["β", "γ", "ω", "η"] Double
  => Model env ts TransParamsSIRSV
transPriorSIRSV  = do
  TransParamsSIRS pBeta pGamma pEta <- transPriorSIRS
  pOmega <- gamma 1 (1/16) #ω
  return (TransParamsSIRSV pBeta pGamma pEta pOmega)

-- | Observation model
obsSIRSV :: Observable env "𝜉" Int
  => ObsModel env ts Double PoplV Reported
obsSIRSV rho (PoplV _ i _ v)  = do
  i <- poisson (rho * fromIntegral i) #𝜉
  return i

-- | SIRSV as HMM
hmmSIRSV ::  (Observables env '["𝜉"] Int, Observables env '["β", "γ", "η", "ω", "ρ"] Double)
  -- | number of days
  => Int
  -- | initial population states
  -> PoplV
  -- | (final population state, intermediate population states)
  -> Model env ts (PoplV, [PoplV])
hmmSIRSV n = handleWriterM . hmmGen transPriorSIRSV obsPriorSIR transSIRSV obsSIRSV n

-- | Simulate from SIRSV model : ([(s, i, r, v)], [𝜉])
simSIRSV :: Sampler ([(Int, Int, Int, Int)], [Reported])
simSIRSV = do
  -- Specify model input of 762 susceptible and 1 infected
  let sirv_0      = PoplV {s' = 762, i' = 1, r' = 0, v' = 0}
  -- Specify model environment
      env_in = #β := [0.7] <:> #γ := [0.009] <:> #η := [0.05] <:> #ω := [0.02] <:> #ρ := [0.3] <:> #𝜉 := [] <:> enil
  -- Simulate an epidemic over 100 days
  ((_, sirv_trace), env_out) <- SIM.simulate (hmmSIRSV 100 sirv_0) env_in
  -- Get the observed infections over 100 days
  let 𝜉s :: [Reported] = get #𝜉 env_out
  -- Get the true SIRV values over 100 days
      sirvs = map (\(PoplV s i recov v) -> (s, i, recov, v)) sirv_trace
  return (sirvs, 𝜉s)

{- | Interfacing the SIR model on top of Monad Bayes.
-}

mbayesSIR ::
   (Bayes.MonadInfer m
  , Observables env '["𝜉"] Int , Observables env '[ "β" , "γ" , "ρ"] Double)
  => Int -> Popl -> Env env -> m ((Popl, [Popl]), Env env)
mbayesSIR n popl = toMBayes (hmmSIR' n popl)

simSIRMB :: Int -> IO ([(Int, Int, Int)], [Reported])
simSIRMB n_days = do
  let env_in = #β := [0.7] <:> #γ := [0.009] <:> #ρ := [0.3] <:> #𝜉 := [] <:> enil
      sir_0      = Popl {s = 762, i = 1, r = 0}
  ((_, sir_trace), env_out) <- Bayes.sampleIO $ Bayes.prior (mbayesSIR n_days sir_0 env_in)
  let 𝜉s :: [Reported] = get #𝜉 env_out
      sirs = map (\(Popl s i recov) -> (s, i, recov)) sir_trace
  pure (sirs, 𝜉s)

mhSIRMB :: Int -> IO ([Double], [Double]) -- [(Popl, Env SIRenv)]
mhSIRMB n_days = do
  𝜉s <- snd <$> simSIRMB n_days
  let sir_0      = Popl {s = 762, i = 1, r = 0}
      env_in = #β := [] <:> #γ := [0.0085] <:> #ρ := [] <:> #𝜉 := 𝜉s <:> enil
  (_, env_outs) <- unzip <$> Bayes.sampleIO (Bayes.prior $ Bayes.mh 100 (mbayesSIR 100 sir_0 env_in))
  let ρs = concatMap (get #ρ) env_outs
      βs = concatMap (get #β) env_outs
  pure (ρs, βs)
