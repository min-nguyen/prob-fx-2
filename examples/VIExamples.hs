

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MonoLocalBinds #-}

{-# LANGUAGE TypeApplications #-}

{- | A linear regression model, modelling a linear relationship between data points x and y.
-}

module VIExamples where

import Inference.MC.SIM as SIM ( simulate )
import Inference.MC.SMC2 as SMC2 ( smc2 )
import Inference.VI.VI
import qualified Inference.VI.BBVI as BBVI
import qualified Inference.VI.MLE as MLE
import qualified Inference.VI.MAP as MAP
import Sampler ( Sampler, sampleIO, liftIO, sampleIOFixed )
import qualified Trace
import           Trace (Key(..))
import Control.Monad ( replicateM, (>=>) )
import Data.Kind (Constraint)
import Env ( Observables, Observable(..), Assign((:=)), Env, enil, (<:>), vnil, (<#>) )
import Effects.Dist
import PrimDist
import Data.Type.Nat
import Data.Maybe
import HMM (simHMM)
import Comp
import Model
import Vec (Vec, TypeableSNatI)
import qualified Vec as Vec
import Data.Proxy
import qualified LDA


{- | Linear regression environment.
-}
type LinRegrEnv =
    '[  "m" ':= Double, -- ^ gradient
        "c" ':= Double, -- ^ intercept
        "σ" ':= Double, -- ^ noise
        "y" ':= Double    -- ^ output
     ]

{- | Linear regression as a probabilistic program for inference -}
linRegr :: forall env es. Observables env '["m", "c"] Double
  => [(Double, Double)] -> VIModel env es (Double, Double)  -- ^ y datapoints
linRegr xys = do
  -- Draw model parameters from prior
  m <- sample' @env (mkNormal 0 3) (#m, 0)
  c <- sample' @env (mkNormal 0 5) (#c, 0)
  -- Generate outputs ys
  mapM_ (\((x, y), idx) -> observe (mkNormal (m * x + c) 1) y (Addr "y" idx)) (zip xys [0 ..])
  return (m, c)

linRegrGuide :: forall env es. Observables env '["m", "c"] Double
  => VIGuide env es ()
linRegrGuide = do
  m <- param' @env (mkNormal 0 3) (#m, 0)
  c <- param' @env (mkNormal 0 5) (#c, 0)
  return ()

bbviLinRegr :: Int -> Int -> Int -> Sampler ([Double], [Double])
bbviLinRegr t_steps l_samples n_datapoints = do
  let xys          = [ (x, 2 * x) | x <- [1 .. fromIntegral n_datapoints]]
      empty_env    = (#y := []) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  enil
  traceQ <- BBVI.bbvi t_steps l_samples linRegrGuide (linRegr xys) empty_env
  let m_dist = toList . fromJust $ Trace.lookupBy @Normal ((== "m") . tag ) traceQ
      c_dist = toList . fromJust $ Trace.lookupBy @Normal ((== "c") . tag ) traceQ
  pure (m_dist, c_dist)

mleLinRegr :: Int -> Int -> Int -> Sampler ([Double], [Double])
mleLinRegr t_steps l_samples n_datapoints = do
  let xys          = [ (x, 2 * x) | x <- [1 .. fromIntegral n_datapoints]]
      empty_env    = (#y := []) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  enil
  traceQ <- MLE.mle t_steps l_samples linRegrGuide (linRegr xys) empty_env
  let m_dist = toList . fromJust $ Trace.lookupBy @Normal ((== "m") . tag ) traceQ
      c_dist = toList . fromJust $ Trace.lookupBy @Normal ((== "c") . tag ) traceQ
  pure (m_dist, c_dist)

mapLinRegr :: Int -> Int -> Int -> Sampler ([Double], [Double])
mapLinRegr t_steps l_samples n_datapoints = do
  let xys          = [ (x, 2 * x) | x <- [1 .. fromIntegral n_datapoints]]
      empty_env    = (#y := []) <:> (#m := []) <:> (#c := []) <:> (#σ := []) <:>  enil
  traceQ <- MAP.map t_steps l_samples linRegrGuide (linRegr xys) empty_env
  let m_dist = toList . fromJust $ Trace.lookupBy @Normal ((== "m") . tag ) traceQ
      c_dist = toList . fromJust $ Trace.lookupBy @Normal ((== "c") . tag ) traceQ
  pure (m_dist, c_dist)

-- | Chain of HMM nodes
hmm :: forall env es. (Observables env '["obs_p", "trans_p"] Double)
  -- | number of HMM nodes
  => Int
  -- | initial latent state
  -> Int
  -- | Observations
  -> [Int]
  -- | final latent state
  -> VIModel env es (Int, Int)
hmm n x0 ys = do
  trans_p <- sample' @env (mkBeta 2 2) (#trans_p, 0)
  obs_p   <- sample' @env (mkBeta 2 2) (#obs_p, 0)
  let hmmNode (x, idx) = do  b <- fromEnum <$> sample (mkBernoulli trans_p) (Addr "x" idx)
                             let x_i = x + b
                             y_i <- observe (mkBinomial x_i obs_p) (ys !! idx) (Addr "y" idx)
                             return (x_i, idx + 1)
  foldr (>=>) return (replicate n hmmNode) (x0, 0)

hmmGuide :: forall env es. (Observables env '["obs_p", "trans_p"] Double, Observable env "x" Bool)
  => Int -> Int -> VIGuide env es ()
hmmGuide n x0 = do
  trans_p <- param' @env (mkBeta 2 2) (#trans_p, 0)
  obs_p   <- param' @env (mkBeta 2 2) (#obs_p, 0)
  let hmmNode (x, idx) = do b <- fromEnum <$> sample' @env (mkBernoulli trans_p) (#x, idx)
                            let x_i = x + b
                            return (x_i, idx + 1)
  foldr (>=>) return (replicate n hmmNode) (x0, 0)
  pure ()

bbviHMM :: Int -> Int -> Int -> Sampler ([Double], [Double])
bbviHMM t_steps l_samples hmm_length = do
  ys <- simHMM hmm_length
  -- liftIO (print ys)
  let empty_env  = #trans_p := [] <:> #obs_p := [] <:> #x := []  <:> enil

  traceQ <- BBVI.bbvi t_steps l_samples (hmmGuide hmm_length 0) (hmm hmm_length 0 ys) empty_env
  let trans_dist = toList . fromJust $ Trace.lookupBy @Beta  ((== "trans_p") . tag ) traceQ
      obs_dist   = toList . fromJust $ Trace.lookupBy @Beta  ((== "obs_p") . tag ) traceQ
  pure (trans_dist, obs_dist)

mleHMM :: Int -> Int -> Int -> Sampler ([Double], [Double])
mleHMM t_steps l_samples hmm_length = do
  ys <- simHMM hmm_length
  -- liftIO (print ys)
  let empty_env  = #trans_p := [] <:> #obs_p := [] <:> #x := []  <:> enil

  traceQ <- MLE.mle t_steps l_samples (hmmGuide hmm_length 0) (hmm hmm_length 0 ys) empty_env
  let trans_dist = toList . fromJust $ Trace.lookupBy @Beta  ((== "trans_p") . tag ) traceQ
      obs_dist   = toList . fromJust $ Trace.lookupBy @Beta  ((== "obs_p") . tag ) traceQ
  pure (trans_dist, obs_dist)

mapHMM :: Int -> Int -> Int -> Sampler ([Double], [Double])
mapHMM t_steps l_samples hmm_length = do
  ys <- simHMM hmm_length
  -- liftIO (print ys)
  let empty_env  = #trans_p := [] <:> #obs_p := [] <:> #x := []  <:> enil

  traceQ <- MAP.map t_steps l_samples (hmmGuide hmm_length 0) (hmm hmm_length 0 ys) empty_env
  let trans_dist = toList . fromJust $ Trace.lookupBy @Beta  ((== "trans_p") . tag ) traceQ
      obs_dist   = toList . fromJust $ Trace.lookupBy @Beta  ((== "obs_p") . tag ) traceQ
  pure (trans_dist, obs_dist)


-- | Distribution over the n topics in a document, over the distribution of m words in a topic
topicModel :: forall m n env es. (TypeableSNatI m, TypeableSNatI n,
               Observable env "φ" (Vec m Double),
               Observable env "θ" (Vec n Double),
               Observable env "z" Int)
  -- | vocabulary
  => Vec m String
  -- | number of topics
  -> SNat n
  -- | number of words
  -> [String]
  -- | generated words
  -> VIModel env es (Vec n (Vec m Double))
topicModel vocab n_topics ws = do
  -- Generate distribution over words for each topic
  let idxs = Vec.iterate (snat @n) (+1) (0 :: Int)
  topic_word_ps <- Vec.mapM (\idx -> sample' @env (mkDirichlet (Vec.replicate (snat @m) 1)) (#φ, idx)
                            ) idxs
  let topic_word_ps' = (map Vec.toList . Vec.toList) topic_word_ps
  -- -- Generate distribution over topics for a given document
  doc_topic_ps  <- sample' @env (mkDirichlet (Vec.replicate n_topics 1)) (#θ, 0)
  mapM_  (\(w, idx) -> do  z <- sample' @env (mkCategorical (Vec.toList doc_topic_ps)) (#z, idx)
                           let word_ps = topic_word_ps' !! z
                           observe (mkDiscrete (zip (Vec.toList vocab) word_ps)) (ws !! idx) (Addr "w" idx)) (zip ws [0 ..])
  pure topic_word_ps

topicGuide :: forall m n env es. (TypeableSNatI m, TypeableSNatI n,
               Observable env "φ" (Vec m Double),
               Observable env "θ" (Vec n Double),
               Observable env "z" Int)
  => Vec m String -> SNat n -> [String] -> VIGuide env es ()
topicGuide vocab n_topics ws = do
  let idxs = Vec.iterate (snat @n) (+1) (0 :: Int)
  topic_word_ps <- Vec.mapM (\idx -> param' @env (mkDirichlet (Vec.replicate (snat @m) 1)) (#φ, idx)
                            ) idxs
  doc_topic_ps  <- param' @env (mkDirichlet (Vec.replicate n_topics 1)) (#θ, 0)
  mapM_  (\(w, idx) -> sample' @env (mkCategorical (Vec.toList doc_topic_ps)) (#z, idx)) (zip ws [0 ..])

bbviLDA :: Int -> Int -> Int -> Sampler ([Double], [Double], [Double])
bbviLDA t_steps l_samples n_words = do

  let empty_env  = #θ := [] <:>  #φ := [] <:> #z := [] <:> enil
      vocab      = LDA.vocab
      n_topics   = snat @(FromGHC 2)
      ws         = take n_words LDA.document
  traceQ <- BBVI.bbvi t_steps l_samples (topicGuide vocab n_topics ws) (topicModel vocab n_topics ws) empty_env
  let θ_dist     = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 2)) ((== "θ") . tag) traceQ
      φ0_dist    = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 4)) (\(Addr  t i) -> (t, i) == ("φ", 0)) traceQ
      φ1_dist    = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 4)) (\(Addr  t i) -> (t, i) == ("φ", 1)) traceQ
  pure (θ_dist, φ0_dist, φ1_dist)

mleLDA :: Int -> Int -> Int -> Sampler ([Double], [Double], [Double])
mleLDA t_steps l_samples n_words = do

  let empty_env  = #θ := [] <:>  #φ := [] <:> #z := [] <:> enil
      vocab      = LDA.vocab
      n_topics   = snat @(FromGHC 2)
      ws         = take n_words LDA.document
  traceQ <- MLE.mle t_steps l_samples (topicGuide vocab n_topics ws) (topicModel vocab n_topics ws) empty_env
  let θ_dist     = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 2)) ((== "θ") . tag) traceQ
      φ0_dist    = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 4)) (\(Addr  t i) -> (t, i) == ("φ", 0)) traceQ
      φ1_dist    = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 4)) (\(Addr  t i) -> (t, i) == ("φ", 1)) traceQ
  pure (θ_dist, φ0_dist, φ1_dist)

mapLDA :: Int -> Int -> Int -> Sampler ([Double], [Double], [Double])
mapLDA t_steps l_samples n_words = do

  let empty_env  = #θ := [] <:>  #φ := [] <:> #z := [] <:> enil
      vocab      = LDA.vocab
      n_topics   = snat @(FromGHC 2)
      ws         = take n_words LDA.document
  traceQ <- MAP.map t_steps l_samples (topicGuide vocab n_topics ws) (topicModel vocab n_topics ws) empty_env
  let θ_dist     = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 2)) ((== "θ") . tag) traceQ
      φ0_dist    = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 4)) (\(Addr  t i) -> (t, i) == ("φ", 0)) traceQ
      φ1_dist    = toList . fromJust $ Trace.lookupBy @(Dirichlet (FromGHC 4)) (\(Addr  t i) -> (t, i) == ("φ", 1)) traceQ
  pure (θ_dist, φ0_dist, φ1_dist)
