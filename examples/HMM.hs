
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE TypeApplications #-}

{- | A variety of possible implementations of a [Hidden Markov Model (HMM)](https://en.wikipedia.org/wiki/Hidden_Markov_model).
-}

module HMM where

import Control.Monad ( (<=<), (>=>), replicateM )
import Data.Kind (Constraint)
import Effects.Writer ( handleWriterM, tellM, Writer )
import Env ( Observables, Observable(..), Assign((:=)), Env, enil, (<:>), vnil, (<#>) )
import Inference.LW as LW ( lw )
import Inference.MB as MB ( toMBayes )
import Inference.MH as MH ( mh )
import Inference.SIM as SIM ( simulate )
import Model ( Model, bernoulli', binomial, uniform )
import Numeric.Log ( Log )
import Prog ( Member )
import qualified Control.Monad.Bayes.Class as Bayes
import qualified Control.Monad.Bayes.Sampler as Bayes
import qualified Control.Monad.Bayes.Traced as Bayes
import qualified Control.Monad.Bayes.Weighted as Bayes
import Sampler ( Sampler )
import Trace ( FromSTrace )
import Util (boolToInt)

-- | A HMM environment
type HMMEnv =
  '[ "trans_p" ':= Double,    -- ^ parameter for transitioning between latent states
     "obs_p"   ':= Double,    -- ^ parameter for projecting latent state to observation
     "y"       ':= Int        -- ^ observation
   ]


{- | HMM as a loop
-}
hmmFor :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double)
  -- | number of HMM nodes
  => Int
  -- | initial HMM latent state
  -> Int
  -- | final HMM latent state
  -> Model env ts Int
hmmFor n x = do
  -- Draw transition and observation parameters from prior distributions
  trans_p <- uniform 0 1 #trans_p
  obs_p   <- uniform 0 1 #obs_p
  -- Iterate over @n@ HMM nodes
  let hmmLoop i x_prev | i < n = do
                            -- transition to next latent state
                            dX <- boolToInt <$> bernoulli' trans_p
                            let x = x_prev + dX
                            -- project latent state to observation
                            binomial x obs_p #y
                            hmmLoop (i - 1) x
                       | otherwise = return x_prev
  hmmLoop 0 x

-- | Simulate from a HMM
simHMM :: Sampler (Int, Env HMMEnv)
simHMM = do
  -- Specify model inputs
  let x_0 = 0; n = 10
  -- Specify model environment
      env = #trans_p := [0.5] <:> #obs_p := [0.8] <:> #y := [] <:> enil
  SIM.simulate (hmmFor n 0) env

-- | Perform likelihood-weighting inference over HMM
lwHMM :: Sampler [(Env HMMEnv, Double)]
lwHMM   = do
  -- Specify model inputs
  let x_0 = 0; n = 10
  -- Specify model environment
      env = #trans_p := [] <:> #obs_p := [] <:> #y := [0, 1, 1, 3, 4, 5, 5, 5, 6, 5] <:> enil
  LW.lw 100 (hmmFor n x_0) env


{- | A modular HMM version.
-}

-- | HMM transition sub-model
transModel
  -- | transition parameter
  :: Double
  -- | previous latent state
  -> Int
  -- | next latent state
  -> Model env ts Int
transModel transition_p x_prev = do
  dX <- boolToInt <$> bernoulli' transition_p
  return (x_prev + dX)

-- | HMM observation sub-model
obsModel :: (Observable env "y" Int)
  -- | observation parameter
  => Double
  -- | current latent state
  -> Int
  -- | observation
  -> Model env ts Int
obsModel observation_p x = do
  y <- binomial x observation_p #y
  return y

-- | Single HMM node
hmmNode :: (Observable env "y" Int)
  -- | transition parameter
  => Double
  -- | observation parameter
  -> Double
  -- | previous latent state
  -> Int
  -- | next latent state
  -> Model env ts Int
hmmNode transition_p observation_p x_prev = do
  x_i <- transModel  transition_p x_prev
  y_i <- obsModel observation_p x_i
  return x_i

-- | Chain of HMM nodes
hmm :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double)
  -- | number of HMM nodes
  => Int
  -- | initial latent state
  -> Int
  -- | final latent state
  -> Model env ts Int
hmm n x = do
  trans_p <- uniform 0 1 #trans_p
  obs_p   <- uniform 0 1 #obs_p
  foldr (>=>) return (replicate n (hmmNode trans_p obs_p)) x

{- | Extending the modular HMM with a user-specific effect.
     The below example uses the @Writer@ effect for recording the intermediate latent states.
-}

hmmNodeW :: (Observable env "y" Int
           , Member (Writer [Int]) es)
  -- | transition parameter
  => Double
  -- | observation parameter
  -> Double
  -- | previous latent state
  -> Int
  -- | next latent state
  -> Model env es Int
hmmNodeW transition_p observation_p x_prev = do
  x_n <- transModel transition_p x_prev
  tellM [x_n] -- write each latent state to a stream [Int]
  y_n <- obsModel observation_p x_n
  pure x_n

hmmW :: ( Observable env "y" Int
        , Observables env '["obs_p", "trans_p"] Double
        , Member (Writer [Int]) es)
  -- | number of HMM nodes
  => Int
  -- | initial latent state
  -> Int
  -- | final latent state
  -> Model env es Int
hmmW n x = do
  trans_p <- uniform 0 1 #trans_p
  obs_p   <- uniform 0 1 #obs_p
  foldr (<=<) pure  (replicate n (hmmNodeW trans_p obs_p)) x

-- | Simulate from a HMM
simHMMw
  -- | number of HMM nodes
  :: Int
  -- | [(latent state, observation)]
  -> Sampler [(Int, Int)]
simHMMw hmm_length = do
  -- Specify model environment
  let env_in = #trans_p := [0.9] <:> #obs_p := [0.2] <:> #y := [] <:> enil
  -- Handle the Writer effect to produce the stream of latent states @xs@, and then simulate
  ((_, xs), env_out) <- SIM.simulate (handleWriterM $ hmmW hmm_length 0) env_in
  -- Get the observations
  let ys :: [Int] = get #y env_out
  pure $ zip xs ys

-- | Metropolis-Hastings inference over a HMM
mhHMMw
  -- | number of MH iterations
  :: Int
  -- | number of HMM nodes
  -> Int
  -- | [(transition parameter, observation parameter)]
  -> Sampler ([Double], [Double])
mhHMMw mh_samples hmm_length = do
  -- Simulate a trace of observations from the HMM
  ys <- map snd <$> simHMMw hmm_length
  -- Specify a model environment containing those observations
  let env  = #trans_p := [] <:> #obs_p := [] <:> #y := ys <:> enil
  -- Handle the Writer effect and then run MH inference
  mh_envs_out <- MH.mh mh_samples (handleWriterM @[Int] $ hmmW hmm_length 0) env (#trans_p <#> #obs_p <#> vnil)
  -- Get the trace of sampled transition and observation parameters
  let trans_ps    = concatMap (get #trans_p) mh_envs_out
      obs_ps      = concatMap (get #obs_p) mh_envs_out
  pure (trans_ps, obs_ps)

{- | Interfacing the HMM on top of Monad Bayes.
-}

-- | Translate the HMM under a model environment to a program in Monad Bayes
mbayesHMM :: ( FromSTrace env
             , Bayes.MonadInfer m
             , Observable env "y" Int
             , Observables env '["obs_p", "trans_p"] Double)
  -- | number of HMM nodes
  => Int
  -- | initial latent state
  -> Int
  -- | input model environment
  -> Env env
  -- | ((final latent state, intermediate latent states), output model environment)
  -> m ((Int, [Int]), Env env)
mbayesHMM n x env = toMBayes (handleWriterM $ hmmW n x) env

-- | Simulate from the HMM in Monad Bayes.
simHMMMB
  -- | number of HMM nodes
  :: Int
  -- | [(latent state, observation)]
  -> IO [(Int, Int)]
simHMMMB hmm_length = do
  -- Specify initial latent state
  let x   = 0
  -- Specify model environment
      env_in = (#trans_p := [0.5]) <:> #obs_p := [0.9] <:> (#y := []) <:> enil
  -- Execute the HMM in Monad Bayes under the model environment
  ((_, xs), env_out) <- Bayes.sampleIO $ Bayes.prior $ mbayesHMM hmm_length x env_in
  -- Get the observations
  let ys :: [Int] = get #y env_out
  pure $ zip xs ys

-- | Likelihood-weighting from the HMM in Monad Bayes.
lwHMMMB
  -- | likelihood-weighting iterations
  :: Int
  -- | number of HMM nodes
  -> Int
  -- | [ (((final latent state, intermediate latent states), output model environment), likelihood-weighting) ]
  -> IO [(((Int, [Int]), Env HMMEnv), Log Double)]
lwHMMMB n_samples hmm_length = do
  -- Simulate a trace of observations from the HMM
  ys <- map snd <$> simHMMMB hmm_length
  -- Specify a model environment containing those observations
  let env = (#trans_p := []) <:> #obs_p := [] <:> (#y := ys) <:>  enil
  -- Execute the HMM in Monad Bayes under the model environment,
  Bayes.sampleIO $ replicateM n_samples $ Bayes.runWeighted $ mbayesHMM hmm_length 0 env

-- | Metropolis-Hastings from the HMM in Monad Bayes.
mhHMMMB :: Int -> Int -> IO [((Int, [Int]), Env HMMEnv)]
mhHMMMB n_mhsteps hmm_length = do
  ys <- map snd <$> simHMMMB hmm_length
  let env = (#trans_p := []) <:> #obs_p := [] <:> (#y := ys) <:>  enil
  Bayes.sampleIO $ Bayes.prior $ Bayes.mh n_mhsteps (mbayesHMM hmm_length 0 env)

{- | A higher-order, generic HMM.
-}
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
