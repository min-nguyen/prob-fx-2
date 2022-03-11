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
{-# LANGUAGE QuantifiedConstraints #-}
module Example where

import Statistics.Distribution
import GHC.OverloadedLabels
import Freer
-- import Reader
import ObsReader
import State
import Writer
import Model
import Dist
import IO
import Sampler
import OpenSum (OpenSum)
import qualified OpenSum as OpenSum
import Control.Monad
import Data.List as List
import Unsafe.Coerce
import Data.Maybe
import Data.Kind (Constraint)
import GHC.TypeLits
import Data.Typeable
import ModelEnv
import Util
import Data.Vector.Fusion.Bundle (findIndex)
import GHC.Show (Show)
import qualified Data.Map as Map
import Control.Lens.Combinators (isn't)

coinFlip :: (Observables env '["p"] Double, Observables env '[ "y"] Bool) => Model env es Bool
coinFlip = do
  p <- uniform' 0 1 #p
  y <- bernoulli' p #y
  return y

{- Linear regression -}
type LinRegrEnv =
    '[  "y" ':= Double,
        "m" ':=  Double,
        "c" ':=  Double,
        "σ" ':=  Double
     ]

linearRegressionOne :: forall env rs .
  Observables env '["y", "m", "c", "σ"] Double =>
  Double -> Model env rs (Double, Double)
linearRegressionOne x = do
  m <- normal' 0 3 #m
  c <- normal' 0 5 #c
  σ <- uniform' 1 3 #σ
  -- printM $ "(m * x + c) is " ++ show (m * x + c)
  y <- normal' (m * x + c) σ #y
  return (x, y)

linearRegression :: forall env rs .
  Observables env '["y", "m", "c", "σ"] Double =>
  [Double] -> Model env rs [(Double, Double)]
linearRegression xs = do
  m <- normal' 0 3 #m
  c <- normal' 0 5 #c
  σ <- uniform' 1 3 #σ
  ys <- foldM (\ys x -> do
                    y <- normal' (m * x + c) σ #y
                    return (y:ys)) [] xs
  return (zip xs (reverse ys))

type LogRegrEnv =
    '[  "label" ':= Bool,
        "m"     ':= Double,
        "b"     ':= Double
     ]

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp((-1) * x))

logisticRegression :: forall rs env.
 (Observable env "label" Bool, Observables env '["m", "b"] Double) =>
 [Double] -> Model env rs [(Double, Bool)]
logisticRegression xs = do
  m     <- normal' 0 5 #m
  b     <- normal' 0 1 #b
  sigma <- gamma 1 1
  ls    <- foldM (\ls x -> do
                     y <- normal (m * x + b) sigma
                     l <- bernoulli' (sigmoid y) #label
                     return (l:ls)) [] xs
  return (zip xs (reverse ls))

{- HMM -}
type HMMEnv =
  '[ "y"       ':= Int,
     "trans_p" ':= Double,
     "obs_p"   ':= Double
   ]

transitionModel ::  Double -> Int -> Model env ts Int
transitionModel transition_p x_prev = do
  dX <- boolToInt <$> bernoulli transition_p
  return (dX + x_prev)

observationModel :: (Observable env "y" Int)
  => Double -> Int -> Model env ts Int
observationModel observation_p x = do
  binomial' x observation_p #y

hmmNode :: (Observable env "y" Int)
  => Double -> Double -> Int -> Model env ts Int
hmmNode transition_p observation_p x_prev = do
  x_n <- transitionModel  transition_p x_prev
  y_n <- observationModel observation_p x_n
  return x_n

hmmNSteps :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double)
  => Int -> (Int -> Model env ts Int)
hmmNSteps n x = do
  trans_p <- uniform' 0 1 #trans_p
  obs_p   <- uniform' 0 1 #obs_p
  foldr (<=<) return  (replicate n (hmmNode trans_p obs_p)) x

{- HMM Loop-}
hmmForM :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double) =>
  Int -> Int -> Model env ts Int
hmmForM n x = do
  trans_p <- uniform' 0 1 #trans_p
  obs_p   <- uniform' 0 1 #obs_p
  let hmmLoop :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double) => Int -> Int -> Model env ts Int
      hmmLoop 0 x_prev = return x_prev
      hmmLoop i x_prev = do
        dX <- boolToInt <$> bernoulli trans_p
        let x = x_prev + dX
        binomial' x obs_p #y
        hmmLoop (i - 1) x
  hmmLoop n x

{- Topic Model -}
type TopicEnv =
  '[ "θ" ':= [Double],
     "φ" ':= [Double],
     "w" ':= String
   ]

-- Assignment of word probabilities to a topic
wordDist :: Observable env "w" String =>
  [String] -> [Double] -> Model env ts String
wordDist vocab ps =
  categorical' (zip vocab ps) #w

-- Probability of each word in a topic
topicWordPrior :: Observable env "φ" [Double]
  => [String] -> Model env ts [Double]
topicWordPrior vocab
  = dirichlet' (replicate (length vocab) 1) #φ

-- Probability of each topic in a document
docTopicPrior :: Observable env "θ" [Double]
  => Int -> Model env ts [Double]
docTopicPrior n_topics = dirichlet' (replicate n_topics 1) #θ

-- Learns topic model for a single document
documentDist :: (Observables env '["φ", "θ"] [Double],
                 Observable env "w" String)
  => [String] -> Int -> Int -> Model env ts [String]
documentDist vocab n_topics n_words = do
  -- Generate distribution over words for each topic
  topic_word_ps <- replicateM n_topics $ topicWordPrior vocab
  -- Distribution over topics for a given document
  doc_topic_ps  <- docTopicPrior n_topics
  replicateM n_words (do  z <- discrete doc_topic_ps
                          let word_ps = topic_word_ps !! z
                          wordDist vocab word_ps)

-- Learns topic models for multiple documents
topicModel :: (Observables env '["φ", "θ"] [Double],
               Observable env "w" String)
  => [String] ->
     Int      ->
     [Int]    -> -- Number of words per document
     Model env ts [[String]]
topicModel vocab n_topics doc_words = do
  mapM (documentDist vocab n_topics) doc_words

{- SIR -}
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
  [
     "β" := Double,
     "γ" := Double,
      "ρ" := Double,
      "infobs" := Int
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

paramsPrior :: Observables env '["ρ", "β",  "γ"] Double
  => Model env ts Params
paramsPrior = do
  pBeta  <- gamma' 2 1 #β
  pGamma <- gamma' 1 (1/8) #γ
  pRho   <- beta' 2 7 #ρ
  return (Params pBeta pGamma pRho)

hmmSIRNsteps ::
     Member (Writer [LatState]) ts
  => (Observables env '["infobs"] Int, Observables env '["ρ", "β", "γ"] Double)
  => Int -> LatState -> Model env ts LatState
hmmSIRNsteps n latentState  = do
  params       <- paramsPrior
  latentState' <- foldl (>=>) return (replicate n $ hmmSIR params) latentState
  return latentState'

{- SIRS (resusceptible) model -}
type SIRSEnv =
  [
     "β" := Double,
     "γ" := Double,
     "ρ" := Double,
     "η" := Double,
     "infobs" := Int
   ]

transRS :: Double -> LatState -> Model env ts LatState
transRS eta (LatState sus inf rec) = do
  dN_RS <- binomial rec (1 - exp (-eta))
  let sus' = sus + dN_RS
      rec' = rec - dN_RS
  return $ LatState sus' inf rec'

transSIRS :: Member (Writer [LatState]) ts
  => Double -> Double -> Double -> LatState -> Model env ts LatState
transSIRS beta gamma eta latentSt = do
  latentSt' <- (transSI beta >=> transIR gamma >=> transRS eta) latentSt
  tellM [latentSt']
  return latentSt'

paramsPrior' :: Observables env '["ρ", "β", "η", "γ"] Double
  => Model env ts (Params, Double)
paramsPrior' = do
  pBeta  <- gamma' 2 1 #β
  pGamma <- gamma' 1 (1/8) #γ
  pEta <- gamma' 1 (1/8) #η
  pRho   <- beta' 2 7 #ρ
  return ((Params pBeta pGamma pRho), pEta)

hmmSIRS :: Member (Writer [LatState]) ts
  => Observable env "infobs" Int
  => Params -> Double -> LatState -> Model env ts LatState
hmmSIRS  (Params beta gamma rho) eta latentState = do
  latentState'   <- transSIRS beta gamma eta latentState
  infectionCount <- obsSIR rho latentState'
  return latentState'

hmmSIRSNsteps ::
     Member (Writer [LatState]) ts
  => (Observable env "infobs" Int, Observables env '["ρ", "β", "η", "γ"] Double)
  => Int -> LatState -> Model env ts LatState
hmmSIRSNsteps n latentState  = do
  (params, eta)       <- paramsPrior'
  latentState' <- foldl (>=>) return (replicate n $ hmmSIRS params eta) latentState
  return latentState'

{- Generic HMM -}

type TransModel env ts params lat     = params -> lat -> Model env ts lat
type ObsModel   env ts params lat obs = params -> lat -> Model env ts obs

hmmNodeGen :: params -> TransModel env ts params lat -> ObsModel env ts params lat obs -> lat -> Model env ts lat
hmmNodeGen params  transModel obsModel lat = do
  lat' <- transModel params lat
  obs' <- obsModel params lat'
  return lat'

hmmGen :: Model env ts params -> TransModel env ts params lat -> ObsModel env ts params lat obs -> Int -> lat ->  Model env ts lat
hmmGen prior transModel obsModel n lat = do
  params <- prior
  foldl (>=>) return (replicate n (hmmNodeGen params transModel obsModel)) lat

{-  SIR model using Generic HMM -}

priorSIRGen :: Observables env '["ρ", "β", "γ"] Double
  => Model env ts Params
priorSIRGen = do
  pBeta  <- gamma' 2 1 #β
  pGamma <- gamma' 1 (1/8) #γ
  pRho   <- beta' 2 7 #ρ
  return (Params pBeta pGamma pRho)

obsSIRGen :: forall env ts. Observable env "infobs" Int
  => Params -> LatState -> Model env ts Int
obsSIRGen (Params _ _ rho) (LatState _ inf _)  = poisson' (rho * fromIntegral inf) #infobs

transSIRGen :: forall env ts. Params -> LatState -> Model env ts LatState
transSIRGen (Params beta gamma _) = transSI beta >=> transIR gamma

sirGen :: (Observable env "infobs" Int, Observables env '["ρ", "β", "γ"] Double) => Int -> LatState -> Model env ts LatState
sirGen = hmmGen priorSIRGen transSIRGen obsSIRGen

{- SIRV model with resusceptible, using Generic HMM -}

data ParamsSIRV = ParamsSIRV {
    beta_  :: Double, -- ^ Mean contact rate between susceptible and infected people
    gamma_ :: Double, -- ^ Mean recovery rate
    rho_   :: Double, -- ^ Rate of detection
    omega_ :: Double, -- ^ Vaccination rate
    eta_   :: Double  -- ^ Resusceptible rate
}

data LatStateSIRV = LatStateSIRV {
    s :: Int, -- ^ Number of people susceptible to infection
    i :: Int, -- ^ Number of people currently infected
    r :: Int, -- ^ Number of people recovered from infection
    v :: Int  -- ^ Number of vaccinated people
} deriving Show

transSI' :: Double -> LatStateSIRV -> Model env ts LatStateSIRV
transSI' beta sirv@(LatStateSIRV {s = s, i = i}) = do
  let pop = s + i + r sirv + v sirv
  dN_SI <- binomial s (1 - exp ((-beta * fromIntegral i) / fromIntegral pop))
  let s' = s - dN_SI
      i' = i + dN_SI
  return $ sirv { s = s', i = i' }

transIR' :: Double -> LatStateSIRV -> Model env ts LatStateSIRV
transIR' gamma sirv@(LatStateSIRV {i = i, r = r})  = do
  dN_IR <- binomial i (1 - exp (-gamma))
  let i' = i  - dN_IR
      r' = r  + dN_IR
  return $ sirv { i = i', r = r' }

transSV' :: Double -> LatStateSIRV -> Model env ts LatStateSIRV
transSV' omega sirv@(LatStateSIRV {s = s, v = v})  = do
  dN_SV <- binomial s (1 - exp (-omega))
  let s' = s - dN_SV
      v' = v + dN_SV
  return $ sirv { s = s', v = v' }

transRS' :: Double -> LatStateSIRV -> Model env ts LatStateSIRV
transRS' eta sirv@(LatStateSIRV {r = r, s = s}) = do
  dN_RS <- binomial r (1 - exp (-eta))
  let s' = s + dN_RS
      r' = r - dN_RS
  return $ sirv {s = s', r = r'}

transSIRV :: Member (Writer [LatStateSIRV]) ts => TransModel env ts ParamsSIRV LatStateSIRV
transSIRV (ParamsSIRV {beta_ = beta, gamma_ = gamma, omega_ = omega, eta_ = eta}) latentSt = do
  latentSt' <- (transSI' beta  >=>
                transIR' gamma >=>
                transRS' eta   >=>
                transSV' omega) latentSt
  tellM [latentSt']
  return latentSt'

obsSIRV :: Observable env "infobs" Int => ObsModel env ts ParamsSIRV LatStateSIRV Int
obsSIRV (ParamsSIRV {rho_ = rho}) sirv  = poisson' (rho * fromIntegral (i sirv)) #infobs

type SIRVEnv =
  [
     "β" := Double,
     "γ" := Double,
     "ρ" := Double,
     "ω" := Double,
     "η" := Double,
     "infobs" := Int
   ]

priorSIRV :: Observables env '["ρ", "β", "γ", "ω", "η"] Double
  => Model env ts ParamsSIRV
priorSIRV = do
  pBeta  <- gamma' 2 1 #β
  pGamma <- gamma' 1 (1/8) #γ
  pRho   <- beta' 2 7 #ρ
  pOmega <- gamma' 1 (1/16) #ω
  pEta <- gamma' 1 (1/8) #η
  return (ParamsSIRV pBeta pGamma pRho pOmega pEta)

hmmSIRVNsteps ::
     Member (Writer [LatStateSIRV]) ts
  => (Observable env "infobs" Int, Observables env '["ρ", "β", "γ", "ω", "η"] Double)
  => Int -> LatStateSIRV -> Model env ts LatStateSIRV
hmmSIRVNsteps  = hmmGen priorSIRV transSIRV obsSIRV

{- Testing generic functions -}
