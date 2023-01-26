
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}



{- | Likelihood-Weighting inference.
-}

module Inference.MC.LW
  ( -- * Inference wrapper functions
    lw
    -- * Inference effect handlers
  , runLW
  , likelihood
  , joint
  ) where

import Data.Bifunctor ( Bifunctor(first), second, bimap )
import Control.Monad ( replicateM )
import Effects.Dist ( Sample, Observe(..), Dist, pattern ObsPrj, pattern SampPrj )
import Effects.EnvRW ( EnvRW )
import Effects.State ( modify, handleState, State )
import Env ( Env )
import LogP ( LogP )
import Inference.MC.SIM as SIM (defaultSample)
import Model ( handleCore, GenModel, Model )
import PrimDist ( logProb )
import Comp ( discharge, Comp(..), Handler, handle )
import Sampler ( Sampler, handleIO )

-- | Top-level wrapper for Likelihood-Weighting (LW) inference
lw
  -- | number of LW iterations
  :: Int
  -- | model
  -> GenModel env [EnvRW env, Dist, Sampler] a
  -- | input model environment
  -> Env env
  -- | [(output model environment, likelihood-weighting)]
  -> Sampler [(Env env, Double)]
lw n model env_in = do
  let prog = handleCore env_in model
  lwTrace <- replicateM n (runLW prog)
  pure (map (bimap snd exp) lwTrace)

-- | Handler for one iteration of LW
runLW
  :: Comp [Observe, Sample, Sampler] a
  -- | ((model output, sample trace), likelihood-weighting)
  -> Sampler (a, LogP)
runLW = handleIO . SIM.defaultSample . likelihood 0

-- | Handle each @Observe@ operation by accumulating the log-likelihood P(Y | X)
likelihood :: LogP -> Handler Observe es a (a, LogP)
likelihood lρ0 = handle lρ0 (\lρ x -> Val (x, lρ)) hop
  where
  hop :: LogP -> Observe x -> (LogP -> x -> Comp es b) -> Comp es b
  hop lρ (Observe d y α) k = k (lρ + logProb d y) y

{- | Record the joint log-probability P(Y, X)
-}
joint :: LogP -> Model es a -> Model es (a, LogP)
joint lρ (Val x)   = pure (x, lρ)
joint lρ (Op op k) = case op of
  ObsPrj d y α   -> Op op (\x -> joint (lρ + logProb d x) $ k x)
  SampPrj d  α   -> Op op (\x -> joint (lρ + logProb d x) $ k x)
  _              -> Op op (joint lρ . k)
