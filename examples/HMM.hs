

{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use camelCase" #-}

{- | A variety of possible implementations of a [Hidden Markov Model (HMM)](https://en.wikipedia.org/wiki/Hidden_Markov_model).
-}

module HMM where

import Control.Monad ( (<=<), (>=>), replicateM )
import Data.Kind (Constraint)
import Effects.Writer ( handleWriterM, tellM, Writer )
import Effects.Dist (Addr(..))
import Env ( Observables, Observable(..), Assign((:=)), Env, enil, (<:>), vnil, (<#>) )
import Inference.MC.LW as LW ( lw )
import Inference.MC.MH as MH ( mh )
import Inference.MC.SMC as SMC ( smc )
import Inference.MC.SIM as SIM ( simulate )
import Inference.MC.RMSMC as RMSMC ( rmsmc )
import Inference.MC.SMC2 as SMC2 ( smc2 )
import Inference.MC.PMMH as PMMH ( pmmh )
import Inference.VI.BBVI as BBVI
import Inference.VI.INVI as INVI
import Model ( Model (..), bernoulli', binomial, uniform, beta )
import Prog ( Member, LastMember )
import Sampler ( Sampler, liftIO )
import Util (boolToInt)
import qualified Trace
import           Trace (Key(..))
import PrimDist
import Data.Maybe
{-
import Numeric.Log ( Log )
import Inference.MB as MB ( handleMBayes )
import qualified Control.Monad.Bayes.Class as Bayes
import qualified Control.Monad.Bayes.Sampler.Strict as Bayes
import qualified Control.Monad.Bayes.Traced as Bayes
import qualified Control.Monad.Bayes.Weighted as Bayes
-}

-- | A HMM environment
type HMMEnv =
  '[ "trans_p" ':= Double,    -- ^ parameter for transitioning between latent states
     "obs_p"   ':= Double,    -- ^ parameter for projecting latent state to observation
     "y"       ':= Int        -- ^ observation
   ]

{- | HMM as a loop.
-}
hmmFor :: (Observable env "y" Int, Observables env '["obs_p", "trans_p"] Double)
  -- | number of HMM nodes
  => Int
  -- | initial HMM latent state
  -> Int
  -- | final HMM latent state
  -> Model env es Int
hmmFor n x = do
  -- Draw transition and observation parameters from prior distributions
  trans_p <- beta 2 5 #trans_p
  obs_p   <- beta 2 5 #obs_p
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

{- | A more modular HMM implementation.
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
  -> Model env es Int
hmm n x = do
  trans_p <- beta 2 2 #trans_p
  obs_p   <- beta 2 2 #obs_p
  foldr (>=>) return (replicate n (hmmNode trans_p obs_p)) x

hmmGuide :: (Observables env '["obs_p", "trans_p"] Double)
  => Int -> Int -> Model env es ()
hmmGuide n x = do
  trans_p <- beta 2 2 #trans_p
  obs_p   <- beta 2 2 #obs_p
  foldr (>=>) return (replicate n (transModel trans_p)) x
  pure ()

-- | Simulate from a HMM
simHMM
  -- | number of HMM nodes
  :: Int
  -> Sampler [Int]
simHMM hmm_length = do
  -- Specify model input
  let x_0 = 0
  -- Specify model environment
      env_in = #trans_p := [0.2] <:> #obs_p := [0.9] <:> #y := [] <:> enil
  (y, env_out) <- SIM.simulate (hmm hmm_length 0) env_in
  let ys :: [Int] = get #y env_out
  pure ys

-- | Likelihood Weighting over a HMM
lwHMM
  -- | number of MH iterations
  :: Int
  -- | number of HMM nodes
  -> Int
  -- | [(transition parameter, observation parameter, likelihood-weighting)]
  -> Sampler ([Double], [Double], [Double])
lwHMM lw_samples hmm_length = do
  -- Simulate a trace of observations from the HMM
  ys <- simHMM hmm_length
  -- Specify a model environment containing those observations
  let env_in  = #trans_p := [] <:> #obs_p := [] <:> #y := ys <:> enil
  -- Handle the Writer effect and then run MH inference
  (env_outs, ws) <- unzip <$> LW.lw lw_samples (hmm hmm_length 0) env_in
  -- Get the trace of sampled transition and observation parameters
  let trans_ps    = concatMap (get #trans_p) env_outs
      obs_ps      = concatMap (get #obs_p) env_outs
  pure (trans_ps, obs_ps, ws)

-- | Metropolis-Hastings inference over a HMM
mhHMM
  -- | number of MH iterations
  :: Int
  -- | number of HMM nodes
  -> Int
  -- | [(transition parameter, observation parameter)]
  -> Sampler ([Double], [Double])
mhHMM mh_samples hmm_length = do
  -- Simulate a trace of observations from the HMM
  ys <- simHMM hmm_length
  -- Specify a model environment containing those observations
  let env_in  = #trans_p := [] <:> #obs_p := [] <:> #y := ys <:> enil
  -- Handle the Writer effect and then run MH inference
  env_outs <- MH.mh mh_samples (hmm hmm_length 0) env_in (#trans_p <#> #obs_p <#> vnil)
  -- Get the trace of sampled transition and observation parameters
  let trans_ps    = concatMap (get #trans_p) env_outs
      obs_ps      = concatMap (get #obs_p) env_outs
  pure (trans_ps, obs_ps)

-- | SMC inference over a HMM
smcHMM
  -- | number of particles
  :: Int
  -- | number of HMM nodes
  -> Int
  -- | [(transition parameter, observation parameter)]
  -> Sampler ([Double], [Double])
smcHMM n_particles hmm_length = do
  -- Simulate a trace of observations from the HMM
  ys <- simHMM hmm_length
  -- Specify a model environment containing those observations
  let env_in  = #trans_p := [] <:> #obs_p := [] <:> #y := ys <:> enil
  -- Handle the Writer effect and then run SMC inference
  env_outs <- SMC.smc n_particles (hmm hmm_length 0) env_in
  -- Get the sampled transition and observation parameters of each particle
  let trans_ps    = concatMap (get #trans_p) env_outs
      obs_ps      = concatMap (get #obs_p) env_outs
  pure (trans_ps, obs_ps)

-- | RMSMC inference over a HMM
rmsmcHMM
  -- | number of particles
  :: Int
  -- | number of MH steps
  -> Int
  -- | number of HMM nodes
  -> Int
  -- | [(transition parameter, observation parameter)]
  -> Sampler ([Double], [Double])
rmsmcHMM n_particles n_mhsteps hmm_length = do
  ys <- simHMM hmm_length
  let env_in  = #trans_p := [] <:> #obs_p := [] <:> #y := ys <:> enil

  env_outs <- RMSMC.rmsmc n_particles n_mhsteps (hmm hmm_length 0) env_in vnil
  let trans_ps    = concatMap (get #trans_p) env_outs
      obs_ps      = concatMap (get #obs_p) env_outs
  pure (trans_ps, obs_ps)

-- | PMMH inference over a HMM
pmmhHMM
  -- | number of MH steps
  :: Int
  -- | number of particles
  -> Int
  -- | number of HMM nodes
  -> Int
  -- | [(transition parameter, observation parameter)]
  -> Sampler ([Double], [Double])
pmmhHMM n_mhsteps n_particles  hmm_length = do
  ys <- simHMM hmm_length
  let env_in  = #trans_p := [] <:> #obs_p := [] <:> #y := ys <:> enil

  env_outs <- PMMH.pmmh n_mhsteps n_particles (hmm hmm_length 0) env_in (#trans_p <#> #obs_p <#> vnil)
  let trans_ps    = concatMap (get #trans_p) env_outs
      obs_ps      = concatMap (get #obs_p) env_outs
  pure (trans_ps, obs_ps)

-- | SMC2 inference over a HMM
smc2HMM
  -- | number of outer particles
  :: Int
  -- | number of MH steps
  -> Int
  -- | number of inner particles
  -> Int
  -- | number of HMM nodes
  -> Int
  -- | [(transition parameter, observation parameter)]
  -> Sampler ([Double], [Double])
smc2HMM n_outer_particles n_mhsteps n_inner_particles  hmm_length = do
  ys <- simHMM hmm_length
  let env_in  = #trans_p := [] <:> #obs_p := [] <:> #y := ys <:> enil

  env_outs <- SMC2.smc2 n_outer_particles n_mhsteps n_inner_particles (hmm hmm_length 0) env_in (#trans_p <#> #obs_p <#> vnil)
  let trans_ps    = concatMap (get #trans_p) env_outs
      obs_ps      = concatMap (get #obs_p) env_outs
  pure (trans_ps, obs_ps)

-- | BBVI inference over a HMM, using a custom guide
bbviHMM
  -- | number of optimisation steps
  :: Int
  -- | number of samples to estimate gradients over
  -> Int
  -- | number of HMM nodes
  -> Int
  -- | (transition beta parameters, observation beta parameters)
  -> Sampler ([Double], [Double])
bbviHMM t_steps l_samples hmm_length = do
  ys <- simHMM hmm_length
  let env_in  = #trans_p := [] <:> #obs_p := [] <:> #y := ys <:> enil

  traceQ <- BBVI.bbvi t_steps l_samples (hmmGuide hmm_length 0) (hmm hmm_length 0) env_in
  let trans_dist = toList . fromJust $ Trace.lookupBy @Beta  ((== "trans_p") . tag ) traceQ
      obs_dist   = toList . fromJust $ Trace.lookupBy @Beta  ((== "obs_p") . tag ) traceQ
  pure (trans_dist, obs_dist)

-- | BBVI inference over a HMM, using the model to generate a default guide
bbviDefaultHMM
  -- | number of optimisation steps
  :: Int
  -- | number of samples to estimate gradients over
  -> Int
  -- | number of HMM nodes
  -> Int
  -- | (transition beta parameters, observation beta parameters)
  -> Sampler ([Double], [Double])
bbviDefaultHMM t_steps l_samples hmm_length = do
  ys <- simHMM hmm_length
  let env_in  = #trans_p := [] <:> #obs_p := [] <:> #y := ys <:> enil

  traceQ <- BBVI.bbvi t_steps l_samples (hmm hmm_length 0) (hmm hmm_length 0) env_in
  let trans_dist = toList . fromJust $ Trace.lookupBy @Beta  ((== "trans_p") . tag ) traceQ
      obs_dist   = toList . fromJust $ Trace.lookupBy @Beta  ((== "obs_p") . tag ) traceQ
  pure (trans_dist, obs_dist)

-- | BBVI inference over a HMM, using a custom guide
inviHMM
  -- | number of optimisation steps
  :: Int
  -- | number of samples to estimate gradients over
  -> Int
  -- | number of HMM nodes
  -> Int
  -- | (transition beta parameters, observation beta parameters)
  -> Sampler ([Double], [Double])
inviHMM t_steps l_samples hmm_length = do
  ys <- simHMM hmm_length
  let env_in  = #trans_p := [] <:> #obs_p := [] <:> #y := ys <:> enil

  traceQ <- INVI.invi t_steps l_samples (hmmGuide hmm_length 0) (hmm hmm_length 0) env_in
  let trans_dist = toList . fromJust $ Trace.lookupBy @Beta  ((== "trans_p") . tag ) traceQ
      obs_dist   = toList . fromJust $ Trace.lookupBy @Beta  ((== "obs_p") . tag ) traceQ
  pure (trans_dist, obs_dist)

{- | Extending the modular HMM with a user-specific effect.
     This example uses the @Writer@ effect for recording the intermediate latent states.
-}

hmmNode_WR :: (Observable env "y" Int
           , Member (Writer [Int]) es)
  -- | transition parameter
  => Double
  -- | observation parameter
  -> Double
  -- | previous latent state
  -> Int
  -- | next latent state
  -> Model env es Int
hmmNode_WR transition_p observation_p x_prev = do
  x_n <- transModel transition_p x_prev
  tellM [x_n] -- write each latent state to a stream [Int]
  y_n <- obsModel observation_p x_n
  pure x_n

hmm_WR :: ( Observable env "y" Int
        , Observables env '["obs_p", "trans_p"] Double
        , Member (Writer [Int]) es)
  -- | number of HMM nodes
  => Int
  -- | initial latent state
  -> Int
  -- | final latent state
  -> Model env es Int
hmm_WR n x = do
  trans_p <- beta 2 2 #trans_p
  obs_p   <- beta 2 2 #obs_p
  foldl (>=>) pure  (replicate n (hmmNode_WR trans_p obs_p)) x

-- | Simulate from a HMM
simHMM_WR
  -- | number of HMM nodes
  :: Int
  -- | [(latent state, observation)]
  -> Sampler [(Int, Int)]
simHMM_WR hmm_length = do
  -- Specify model environment
  let env_in = #trans_p := [0.2] <:> #obs_p := [0.9] <:> #y := [] <:> enil
  -- Handle the Writer effect to produce the stream of latent states @xs@, and then simulate
  ((_, xs), env_out) <- SIM.simulate (handleWriterM $ hmm_WR hmm_length 0) env_in
  -- Get the observations
  let ys :: [Int] = get #y env_out
  pure $ zip xs ys

{- | Interfacing the HMM on top of Monad Bayes.

-- | Translate the HMM under a model environment to a program in Monad Bayes
mbayesHMM :: ( Bayes.MonadInfer m
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
mbayesHMM n x env_in = handleMBayes (handleWriterM $ hmmW n x) env_in

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
  ((_, xs), env_out) <- Bayes.sampleIO $ Bayes.unweighted $ mbayesHMM hmm_length x env_in
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
  let env_in = (#trans_p := []) <:> #obs_p := [] <:> (#y := ys) <:>  enil
  -- Execute the HMM in Monad Bayes under the model environment,
  Bayes.sampleIO $ replicateM n_samples $ Bayes.runWeighted $ mbayesHMM hmm_length 0 env_in

-- | Metropolis-Hastings from the HMM in Monad Bayes.
mhHMMMB
  -- | metropolis-hastings iterations
  :: Int
  -- | number of HMM nodes
  -> Int
  -- | [ (((final latent state, intermediate latent states), output model environment)) ]
  -> IO [((Int, [Int]), Env HMMEnv)]
mhHMMMB n_mhsteps hmm_length = do
  -- Simulate a trace of observations from the HMM
  ys <- map snd <$> simHMMMB hmm_length
  -- Specify a model environment containing those observations
  let env_in = (#trans_p := []) <:> #obs_p := [] <:> (#y := ys) <:>  enil
  Bayes.sampleIO $ Bayes.unweighted $ Bayes.mh n_mhsteps (mbayesHMM hmm_length 0 env_in)

-}

{- | A higher-order, generic HMM.
-}

type TransModel env ts params lat
  =  params           -- ^ transition parameter
  -> lat              -- ^ previous latent state
  -> Model env ts lat -- ^ next latent state

type ObsModel env ts params lat obs
  =  params           -- ^ observation parameter
  -> lat              -- ^ current latent state
  -> Model env ts obs -- ^ observation

hmmGen
  -- | prior for transition parameter
  :: Model env ts ps1
  -- | prior for observation parameter
  -> Model env ts ps2
  -- | transition sub-model
  -> TransModel env ts ps1 lat
  -- | observation sub-model
  -> ObsModel env ts ps2 lat obs
  -- | number of HMM nodes
  -> Int
  -- | initial latent state
  -> lat
  -- | final latent state
  -> Model env ts lat
hmmGen transPrior obsPrior transModel obsModel n x_0 = do
  ps1    <- transPrior
  ps2    <- obsPrior
  let hmmNode x = do
                x' <- transModel ps1 x
                y' <- obsModel ps2 x'
                return x'
  foldl (>=>) return (replicate n hmmNode) x_0
