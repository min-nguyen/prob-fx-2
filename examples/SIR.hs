
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

{- | This demonstrates:
      - The [SIR](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology) model for modelling
        the transition between Susceptible (S), Infected (I), and Recovered (R) individuals over a number of days
        during an epidemic. We model this as a Hidden Markov Model, where the latent states are the true values
        of S, I, and R, and the observations are the reported number of infections (𝜉).
      - Extending the SIR to the SIRS model where recovered individuals (R) can become susceptible (S) again.
      - Extending the SIRS to the SIRSV model where susceptible individuals (S) can become vaccinated (V).

    For convenience, this makes use of the 'Data.Extensible' library for extensible records, and the 'Control.Lens'
    library to record accessors. If the lens notation is unfamiliar, the code below can be corresponded to a less modular
    version the file [SIRNonModular](examples/SIRNonModular.hs).
 -}

module SIR where

import Data.Extensible ( mkField, Record, Lookup, type (>:), (@=), (<:), emptyRecord, Assoc ((:>)) )
import Prog ( Member )
import Control.Lens ( (&), (^.), (.~) )
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
import Inference.MB as MB ( handleMBayes )
import qualified Control.Monad.Bayes.Class as Bayes
import qualified Control.Monad.Bayes.Weighted as Bayes
import qualified Control.Monad.Bayes.Traced as Bayes
import qualified Control.Monad.Bayes.Sampler.Strict as Bayes

-- | A type family for conveniently specifying multiple @Record@ fields of the same type
type family Lookups env (ks :: [Symbol]) a :: Constraint where
  Lookups env (x ': xs) a = (Lookup env x a, Lookups env xs a)
  Lookups env '[] a = ()

-- | HMM latent states (SIRV)
mkField "s" -- ^ susceptible individuals
mkField "i" -- ^ infected individuals
mkField "r" -- ^ recovered individuals
mkField "v" -- ^ vaccinated individuals

-- | HMM observations (𝜉) i.e. reported infections
type Reported = Int


{- | SIR model.
-}

-- | Transition prior
transPriorSIR :: Observables env '["β",  "γ"] Double
  => Model env ts (Double, Double)
transPriorSIR = do
  pBeta  <- gamma 2 1 #β
  pGamma <- gamma 1 (1/8) #γ
  return (pBeta, pGamma)

-- | Transition model from S and I
transSI :: Lookups popl '["s", "i", "r"] Int => TransModel env ts Double (Record popl)
transSI  beta popl = do
  let (s_0, i_0, r_0 ) = (popl ^. s,  popl ^. i,  popl ^. r)
      pop = s_0 + i_0 + r_0
  dN_SI <- binomial' s_0 (1 - exp ((-beta * fromIntegral i_0) / fromIntegral pop))
  return $ popl & s .~ (s_0 - dN_SI)
                & i .~ (i_0 + dN_SI)

-- | Transition model from I and R
transIR :: Lookups popl '["i", "r"] Int => TransModel env ts Double (Record popl)
transIR  gamma popl = do
  let (i_0, r_0) = (popl ^. i,  popl ^. r)
  dN_IR <- binomial' i_0 (1 - exp (-gamma))
  return $ popl & i .~ (i_0 - dN_IR)
                & r .~ (r_0 + dN_IR)

-- | Transition model from S to I, and I to R
transSIR :: (Member (Writer [Record popl]) ts, Lookups popl '["s", "i", "r"] Int)
  => TransModel env ts (Double, Double) (Record popl)
transSIR (beta, gamma) popl = do
  popl <- (transSI beta >=> transIR gamma) popl
  tellM [popl]  -- a user effect for writing each latent SIR state to a stream [Record popl]
  return popl

-- | Observation model parameter
type ObsParams = Double

-- | Observation prior
obsPriorSIR :: Observables env '["ρ"] Double
  => Model env ts Double
obsPriorSIR = beta 2 7 #ρ

-- | Observation model
obsSIR :: Lookup s "i" Int => Observable env "𝜉" Int
  => ObsModel env ts Double (Record s) Reported
obsSIR rho popl  = do
  let i_0 = popl ^. i
  poisson (rho * fromIntegral i_0) #𝜉

-- | SIR as HMM
hmmSIR :: (Lookups popl '["s", "i", "r"] Int
        , Observable env "𝜉" Int
        , Observables env '["ρ", "β", "γ"] Double)
  -- | number of days
  => Int
  -- | initial population state
  -> Record popl
  -- | (final population state, intermediate population states)
  -> Model env es (Record popl, [Record popl])
hmmSIR n = handleWriterM . hmmGen transPriorSIR obsPriorSIR transSIR obsSIR n

-- | Simulate from the SIR model
simSIR
  -- | number of days
  :: Int
  -- ([(s, i, r)], [𝜉])
  -> Sampler ([(Int, Int, Int)], [Reported])
simSIR n_days = do
  -- Specify model input of 762 susceptible and 1 infected
  let sir_0      = #s @= 762 <: #i @= 1 <: #r @= 0 <: emptyRecord
  -- Specify model environment
      sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #ρ := [0.3] <:> #𝜉 := [] <:> enil
  -- Simulate an epidemic over 100 days
  ((_, sir_trace), sim_env_out) <- SIM.simulate (hmmSIR n_days sir_0) sim_env_in
  -- Get the observed infections over 100 days
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
  -- Get the true SIR values over 100 days
      sirs = map (\sir -> (sir ^. s, sir ^. i, sir ^. r)) sir_trace
  return (sirs, 𝜉s)

-- | MH inference from SIR model:
mhSIR
  -- | number of MH iterations
  :: Int
  -- | number of days
  -> Int
  -- | ([ρ], [β])
  -> Sampler ([Double], [Double])
mhSIR n_mhsteps n_days = do
  -- Simulate some observed infections
  𝜉s <- snd <$> simSIR n_days
  -- Specify model input of 762 susceptible and 1 infected
  let sir_0     = #s @= 762 <: #i @= 1 <: #r @= 0 <: emptyRecord
  -- Specify model environment
      mh_env_in = #β := [] <:> #γ := [0.0085] <:> #ρ := [] <:> #𝜉 := 𝜉s <:> enil
  -- Run MH inference over 50000 iterations
  mhTrace <- MH.mh n_mhsteps (hmmSIR n_days sir_0) mh_env_in (#β <#> #ρ <#> vnil)
  -- Get the sampled values for model parameters ρ and β
  let ρs = concatMap (get #ρ) mhTrace
      βs = concatMap (get #β) mhTrace
  return (ρs, βs)


{- | SIRS model.
-}

-- | Transition prior
transPriorSIRS :: Observables env '["β", "η", "γ"] Double
  => Model env ts (Double, Double, Double)
transPriorSIRS = do
  (pBeta, pGamma)  <- transPriorSIR
  pEta <- gamma 1 (1/8) #η
  return (pBeta, pGamma, pEta)

-- | Transition model from S to R
transRS :: Lookups popl '["s", "r"] Int
  => TransModel env ts Double (Record popl)
transRS eta popl = do
  let (r_0, s_0) = (popl ^. r,  popl ^. s)
  dN_RS <- binomial' r_0 (1 - exp (-eta))
  return $ popl & r .~ (r_0 - dN_RS)
                & s .~ (s_0 + dN_RS)

-- | Transition model from S to I, I to R, and R to S
transSIRS :: Lookups popl '["s", "i", "r"] Int
  => TransModel env es (Double, Double, Double) (Record popl)
transSIRS (beta, gamma, eta) = transSI beta >=> transIR gamma >=> transRS eta

-- | SIRS as HMM
hmmSIRS :: (Lookups popl '["s", "i", "r"] Int,
            Observables env '["𝜉"] Int, Observables env '["β", "η", "γ", "ρ"] Double)
  -- | number of days
  => Int
  -- | initial population state
  -> Record popl
  -- | (final population state, intermediate population states)
  -> Model env es (Record popl, [Record popl])
hmmSIRS n = handleWriterM . hmmGen transPriorSIRS obsPriorSIR transSIRS obsSIR n

-- | Simulate from SIRS model: ([(s, i, r)], [𝜉])
simSIRS
  -- | number of days
  :: Int
  -- | ([(s, i, r)], [𝜉])
  -> Sampler ([(Int, Int, Int)], [Reported])
simSIRS n_days = do
  -- Specify model input of 762 susceptible and 1 infected
  let sir_0      = #s @= 762 <: #i @= 1 <: #r @= 0 <: emptyRecord
  -- Specify model environment
      sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #η := [0.05] <:> #ρ := [0.3] <:> #𝜉 := [] <:> enil
  -- Simulate an epidemic over n_days
  ((_, sir_trace), sim_env_out) <- SIM.simulate (hmmSIRS n_days sir_0) sim_env_in
  -- Get the observed infections over n_days
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
  -- Get the true SIRS values over n_days
      sirs = map (\sir -> (sir ^. s, sir ^. i, sir ^. r)) sir_trace
  return (sirs, 𝜉s)

{- | SIRSV model.
-}

-- | Transition prior
transPriorSIRSV :: Observables env '["β", "γ", "ω", "η"] Double
  => Model env ts (Double, Double, Double, Double)
transPriorSIRSV  = do
  (pBeta, pGamma, pEta) <- transPriorSIRS
  pOmega <- gamma 1 (1/16) #ω
  return (pBeta, pGamma, pEta, pOmega)

-- | Transition model from S to V
transSV :: Lookups popl '["s", "v"] Int
  => TransModel env es Double (Record popl)
transSV omega popl  = do
  let (s_0, v_0) = (popl ^. s,  popl ^. v)
  dN_SV <- binomial' s_0 (1 - exp (-omega))
  return $ popl & s .~ (s_0 - dN_SV)
                & v .~ (v_0 + dN_SV)

-- | Transition model from S to I, I to R, R to S, and S to V
transSIRSV :: Lookups popl '["s", "i", "r", "v"] Int
  => TransModel env ts (Double, Double, Double, Double) (Record popl)
transSIRSV (beta, gamma, eta, omega) =
  transSI beta >=> transIR gamma >=> transRS eta >=> transSV omega

-- | SIRSV as HMM
hmmSIRSV :: (Lookups popl '["s", "i", "r", "v"] Int,
             Observables env '["𝜉"] Int, Observables env '["β", "η", "γ", "ω", "ρ"] Double)
  -- | number of days
  => Int
  -- | initial population state
  -> Record popl
  -- | (final population state, intermediate population states)
  -> Model env es (Record popl, [Record popl])
hmmSIRSV n_days = handleWriterM . hmmGen transPriorSIRSV obsPriorSIR transSIRSV obsSIR n_days

-- | Simulate from SIRSV model
simSIRSV
  -- | number of days
  :: Int
  -- | ([(s, i, r, v)], [𝜉])
  -> Sampler ([(Int, Int, Int, Int)], [Reported])
simSIRSV n_days = do
  -- Specify model input of 762 susceptible and 1 infected
  let sir_0      = #s @= 762 <: #i @= 1 <: #r @= 0 <: #v @= 0 <: emptyRecord
  -- Specify model environment
      sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #η := [0.05] <:> #ω := [0.02] <:> #ρ := [0.3] <:> #𝜉 := [] <:> enil
  -- Simulate an epidemic over n_days
  ((_, sir_trace), sim_env_out) <- SIM.simulate (hmmSIRSV n_days sir_0) sim_env_in
  -- Get the observed infections over n_days
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
  -- Get the true SIRSV values over n_days
      sirvs = map (\sirv -> (sirv ^. s, sirv ^. i, sirv ^. r, sirv ^. v)) sir_trace
  return (sirvs, 𝜉s)


{- | Interfacing the SIR model on top of Monad Bayes.
-}

-- | Translate the HMM under a model environment to a program in Monad Bayes
mbayesSIR ::
   ( Bayes.MonadInfer m
   , Lookups popl '["s", "i", "r"] Int
   , Observables env '["𝜉"] Int
   , Observables env '[ "β" , "γ" , "ρ"] Double)
  -- | number of days
  => Int
  -- | initial population state
  -> Record popl
  -- | input model environment
  -> Env env
  -- | ((final latent state, intermediate latent states), output model environment)
  -> m ((Record popl, [Record popl]), Env env)
mbayesSIR n_days popl = handleMBayes (hmmSIR n_days popl)

-- | Simulate from the SIR model in Monad Bayes.
simSIRMB
  -- | number of days
  :: Int
  -- | ([(s, i, r)], [𝜉])
  -> IO ([(Int, Int, Int)], [Reported])
simSIRMB n_days = do
  let sir_0      = #s @= 762 <: #i @= 1 <: #r @= 0 <: #v @= 0 <: emptyRecord
      sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #ρ := [0.3] <:> #𝜉 := [] <:> enil
  ((_, sir_trace), sim_env_out) <- Bayes.sampleIO $ Bayes.unweighted (mbayesSIR n_days sir_0 sim_env_in)
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
      sirs = map (\sir -> (sir ^. s, sir ^. i, sir ^. r)) sir_trace
  pure (sirs, 𝜉s)

-- | Metropolis-Hastings from the SIR model in Monad Bayes.
mhSIRMB
  -- | number of MH iterations
  :: Int
  -- | number of days
  -> Int
  -- | ([ρ], [β])
  -> IO ([Double], [Double])
mhSIRMB n_mhsteps n_days = do
  𝜉s <- snd <$> simSIRMB n_days
  let sir_0      = #s @= 762 <: #i @= 1 <: #r @= 0 <: emptyRecord
      env = #β := [] <:> #γ := [0.0085] <:> #ρ := [] <:> #𝜉 := 𝜉s <:> enil
  (_, env) <- unzip <$> Bayes.sampleIO (Bayes.unweighted $ Bayes.mh n_mhsteps (mbayesSIR n_days sir_0 env))
  -- Get the sampled values for model parameters ρ and β
  let ρs = concatMap (get #ρ) env
      βs = concatMap (get #β) env
  pure (ρs, βs)
