
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE TypeApplications #-}

module Tests.Examples.HMM where

import Model
import Inference.SIM as SIM
import Inference.LW as LW
import Inference.MH as MH
import Sampler
import Prog
import Effects.Writer
import Control.Monad
import Data.Kind (Constraint)
import Env
import Util


-- ||| HMM using Writer Effect
transModelW ::  Double -> Int -> Model env es Int
transModelW transition_p x_prev = do
  dX <- boolToInt <$> bernoulli' transition_p
  pure (dX + x_prev)

obsModelW :: (Observable env "y" Int)
  => Double -> Int -> Model env es Int
obsModelW observation_p x = do
  binomial x observation_p #y

hmmNodeW :: (Observable env "y" Int) => Member (Writer [Int]) es
  => Double -> Double -> Int -> Model env es Int
hmmNodeW transition_p observation_p x_prev = do
  x_n <- transModelW  transition_p x_prev
  tellM [x_n]
  y_n <- obsModelW observation_p x_n
  pure x_n

hmmW :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double) => Member (Writer [Int]) es
  => Int -> (Int -> Model env es Int)
hmmW n x = do
  trans_p <- uniform 0 1 #trans_p
  obs_p   <- uniform 0 1 #obs_p
  foldr (<=<) pure  (replicate n (hmmNodeW trans_p obs_p)) x

simHMMw :: Int -> Sampler [(Int, Int)]
simHMMw hmm_length = do
  let env = #trans_p := [0.9] <:> #obs_p := [0.2] <:> #y := [] <:> nil
  bs <- SIM.simulate (handleWriterM . hmmW hmm_length) env 0

  let sim_envs_out  = snd bs
      xs :: [Int]   = (snd . fst) bs
      ys :: [Int]   = get #y sim_envs_out
  pure $ zip xs ys

mhHMMw :: Int -> Int -> Sampler ([Double], [Double])
mhHMMw mh_samples hmm_length = do
  ys <- map snd <$> simHMMw hmm_length
  let env  = #trans_p := [] <:> #obs_p := [] <:> #y := ys <:> nil
  mh_envs_out <- MH.mh mh_samples (handleWriterM @[Int] . hmmW hmm_length) (0, env)  ["trans_p", "obs_p"]
  let trans_ps    = concatMap (get #trans_p) mh_envs_out
      obs_ps      = concatMap (get #obs_p) mh_envs_out
  pure (trans_ps, obs_ps)
