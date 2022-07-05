{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}
module Examples.SIRRecord where

import qualified Data.Extensible as Extensible
import Data.Extensible hiding (Member)
import Prog
import Control.Lens hiding ((:>))
import Effects.Writer
import Model
    ( beta, binomial', gamma, handleWriterM, poisson, tellM, Model )
import Env
import Control.Monad
import Env
import Examples.HMM
import Data.Extensible (Associated)
import GHC.TypeLits
import Data.Kind (Constraint)

type family Lookups env (ks :: [Symbol]) a :: Constraint where
  Lookups env (x ': xs) a = (Lookup env x a, Lookups env xs a)
  Lookups env '[] a = ()

mkField "s i r v"

type Reported = Int

{- SIR model using extensible records -}

-- | SIR transition model
transSI :: Lookups popl '["s", "i", "r"] Int => TransModel env es Double (Record popl)
transSI  beta popl = do
  let (s_0, i_0, r_0 ) = (popl ^. s,  popl ^. i,  popl ^. r)
      pop = s_0 + i_0 + r_0
  dN_SI <- binomial' s_0 (1 - exp ((-beta * fromIntegral i_0) / fromIntegral pop))
  pure $ popl & s .~ (s_0 - dN_SI)
                & i .~ (i_0 + dN_SI)

transIR :: Lookups popl '["i", "r"] Int => TransModel env es Double (Record popl)
transIR  gamma popl = do
  let (i_0, r_0) = (popl ^. i,  popl ^. r)
  dN_IR <- binomial' i_0 (1 - exp (-gamma))
  pure $ popl & i .~ (i_0 - dN_IR)
                & r .~ (r_0 + dN_IR)

transSIR :: (Member (Writer [Record popl]) es, Lookups popl '["s", "i", "r"] Int)
  => TransModel env es (Double, Double) (Record popl)
transSIR (beta, gamma) popl = do
  popl <- (transSI beta >=> transIR gamma) popl
  tellM [popl]
  pure popl

-- | SIR observation model
type ObsParams = Double

obsSIR :: Lookup s "i" Int => Observable env "𝜉" Int
  => ObsModel env es Double (Record s) Reported
obsSIR rho popl  = do
  let i_0 = popl ^. i
  poisson (rho * fromIntegral i_0) #𝜉

-- | SIR transition prior
transPrior :: Observables env '["β",  "γ"] Double
  => Model env es (Double, Double)
transPrior = do
  pBeta  <- gamma 2 1 #β
  pGamma <- gamma 1 (1/8) #γ
  pure (pBeta, pGamma)

-- | SIR observation prior
obsPrior :: Observables env '["ρ"] Double
  => Model env es ObsParams
obsPrior = beta 2 7 #ρ

-- | SIR as HMM
hmmSIR :: (Member (Writer [Record popl]) es,
           Lookups popl '["s", "i", "r"] Int, Observable env "𝜉" Int, Observables env '["ρ", "β", "γ"] Double)
  => Int -> Record popl -> Model env es (Record popl, [Record popl])
hmmSIR n  = handleWriterM . hmmGen transPrior obsPrior transSIR obsSIR n

{- SIRS (resusceptible) model -}

-- | SIRS transition model
transRS :: Lookups popl '["s", "r"] Int => TransModel env es Double (Record popl)
transRS eta popl = do
  let (r_0, s_0) = (popl ^. r,  popl ^. s)
  dN_RS <- binomial' r_0 (1 - exp (-eta))
  pure $ popl & r .~ (r_0 - dN_RS)
                & s .~ (s_0 + dN_RS)

transSIRS :: Lookups popl '["s", "i", "r"] Int => TransModel env es (Double, Double, Double) (Record popl)
transSIRS (beta, gamma, eta) = transSI beta >=> transIR gamma >=> transRS eta

-- | SIRS transition prior
transPriorS :: Observables env '["β", "η", "γ"] Double
  => Model env es (Double, Double, Double)
transPriorS = do
  (pBeta, pGamma)  <- transPrior
  pEta <- gamma 1 (1/8) #η
  pure (pBeta, pGamma, pEta)

-- | SIRS as HMM
hmmSIRS :: (Member (Writer [Record popl]) es,
            Lookups popl '["s", "i", "r"] Int,
            Observables env '["𝜉"] Int, Observables env '["β", "η", "γ", "ρ"] Double)
  => Int -> Record popl -> Model env es (Record popl, [Record popl])
hmmSIRS n = handleWriterM . hmmGen transPriorS obsPrior transSIRS obsSIR n

{- SIRSV (resusceptible + vacc) model -}

-- | SIRSV transition model
transSV :: Lookups popl '["s", "v"] Int => TransModel env es Double (Record popl)
transSV omega popl  = do
  let (s_0, v_0) = (popl ^. s,  popl ^. v)
  dN_SV <- binomial' s_0 (1 - exp (-omega))
  pure $ popl & s .~ (s_0 - dN_SV)
                & v .~ (v_0 + dN_SV)

transSIRSV :: Lookups popl '["s", "i", "r", "v"] Int => TransModel env es (Double, Double, Double, Double) (Record popl)
transSIRSV (beta, gamma, eta, omega) =
  transSI beta >=> transIR gamma >=> transRS eta  >=> transSV omega

-- | SIRSV transition prior
transPriorSV :: Observables env '["β", "γ", "ω", "η"] Double
  => Model env es (Double, Double, Double, Double)
transPriorSV  = do
  (pBeta, pGamma, pEta) <- transPriorS
  pOmega <- gamma 1 (1/16) #ω
  pure (pBeta, pGamma, pEta, pOmega)

-- | SIRSV as HMM
hmmSIRSV :: (Member (Writer [Record popl]) es,
             Lookups popl '["s", "i", "r", "v"] Int,
             Observables env '["𝜉"] Int, Observables env '["β", "η", "γ", "ω", "ρ"] Double)
  => Int -> Record popl -> Model env es (Record popl, [Record popl])
hmmSIRSV n = handleWriterM . hmmGen transPriorSV obsPrior transSIRSV obsSIR n