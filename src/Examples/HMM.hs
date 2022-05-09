
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE TypeApplications #-}
module Examples.HMM where

import Model
import Inference.SIM as SIM
import Prog
import Effects.State
import Effects.Writer
import Inference.LW as LW
import Inference.MH as MH
import Sampler
import Control.Monad
import Data.Kind (Constraint)
import Env
import Util

{- (Fig 2) HMM Loop -}
type HMMEnv =
  '[ "trans_p" ':= Double,
     "obs_p"   ':= Double,
     "y"       ':= Int
   ]

hmmFor :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double) =>
  Int -> Int -> Model env ts Int
hmmFor n x = do
  trans_p <- uniform 0 1 #trans_p
  obs_p   <- uniform 0 1 #obs_p
  let hmmLoop i x_prev | i < n = do
                            dX <- boolToInt <$> bernoulli' trans_p
                            let x = x_prev + dX
                            binomial x obs_p #y
                            hmmLoop (i - 1) x
                       | otherwise = return x_prev
  hmmLoop 0 x

simHMM :: Sampler (Int, Env HMMEnv)
simHMM = do
  let x_0 = 0; n = 10
      env = #trans_p := [0.5] <:> #obs_p := [0.8] <:> #y := [] <:> eNil
  SIM.simulate (hmmFor n 0) env

lwHMM :: Sampler  [(Env HMMEnv, Double)]
lwHMM   = do
  let x_0 = 0; n = 10
      env = #trans_p := [] <:> #obs_p := [] <:> #y := [0, 1, 1, 3, 4, 5, 5, 5, 6, 5] <:> eNil
  LW.lwTopLevel 100 (hmmFor n x_0) env

{- (Fig 3) Modular HMM -}
transModel ::  Double -> Int -> Model env ts Int
transModel transition_p x_prev = do
  dX <- boolToInt <$> bernoulli' transition_p
  return (x_prev + dX)

obsModel :: (Observable env "y" Int)
  => Double -> Int -> Model env ts Int
obsModel observation_p x = do
  y <- binomial x observation_p #y
  return y

hmmNode :: (Observable env "y" Int)
  => Double -> Double -> Int -> Model env ts Int
hmmNode transition_p observation_p x_prev = do
  x_i <- transModel  transition_p x_prev
  y_i <- obsModel observation_p x_i
  return x_i

hmm :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double)
  => Int -> (Int -> Model env ts Int)
hmm n x = do
  trans_p <- uniform 0 1 #trans_p
  obs_p   <- uniform 0 1 #obs_p
  foldr (>=>) return (replicate n (hmmNode trans_p obs_p)) x

{- (Sec 3) Higher-order, generic HMM -}
type TransModel env ts params lat   = params -> lat -> Model env ts lat
type ObsModel env ts params lat obs = params -> lat -> Model env ts obs

hmmGen :: Model env ts ps1 -> Model env ts ps2
       -> TransModel env ts ps1 lat -> ObsModel env ts ps2 lat obs
       -> Int -> lat -> Model env ts lat
hmmGen transPrior obsPrior transModel obsModel n x_0 = do
  ps1    <- transPrior
  ps2    <- obsPrior
  let hmmNode x = do
                x' <- transModel ps1 x
                y' <- obsModel ps2 x'
                return x'
  foldl (>=>) return (replicate n hmmNode) x_0

{-  Hidden Markov Model using Writer effect -}
transModelW ::  Double -> Int -> Model env ts Int
transModelW transition_p x_prev = do
  dX <- boolToInt <$> bernoulli' transition_p
  return (dX + x_prev)

obsModelW :: (Observable env "y" Int)
  => Double -> Int -> Model env ts Int
obsModelW observation_p x = do
  binomial x observation_p #y

hmmNodeW :: (Observable env "y" Int) => Member (Writer [Int]) ts
  => Double -> Double -> Int -> Model env ts Int
hmmNodeW transition_p observation_p x_prev = do
  x_n <- transModelW  transition_p x_prev
  tellM [x_n]
  y_n <- obsModelW observation_p x_n
  return x_n

hmmW :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double) => Member (Writer [Int]) ts
  => Int -> (Int -> Model env ts Int)
hmmW n x = do
  trans_p <- uniform 0 1 #trans_p
  obs_p   <- uniform 0 1 #obs_p
  foldr (<=<) return  (replicate n (hmmNodeW trans_p obs_p)) x

simHMMw :: Sampler [(Int, Int)]
simHMMw = do
  let hmm_length = 10
      n_samples  = 30
      env = #trans_p := [0.9] <:> #obs_p := [0.2] <:> #y := [] <:> eNil
  bs <- SIM.simulateMany n_samples (handleWriterM $ hmmW hmm_length 0) env
  printS bs
  let sim_envs_out  = map snd bs
      xs :: [Int]   = concatMap (snd . fst) bs
      ys :: [Int]   = concatMap (get #y) sim_envs_out
  return $ zip xs ys

mhHMMw :: Sampler ([Double], [Double])
mhHMMw = do
  let hmm_length = 10
      mh_samples  = 5000
  ys <- map snd <$> simHMMw
  let env  = #trans_p := [] <:> #obs_p := [] <:> #y := ys <:> eNil
      spec = #trans_p ⋮ #obs_p ⋮ #y ⋮ ONil
  mh_envs_out <- MH.mhTopLevel mh_samples (handleWriterM @[Int] $ hmmW hmm_length 0) env spec
  let trans_ps    = concatMap (get #trans_p) mh_envs_out
      obs_ps      = concatMap (get #obs_p) mh_envs_out
  return (trans_ps, obs_ps)