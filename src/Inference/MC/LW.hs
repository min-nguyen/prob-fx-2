
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}



{- | Likelihood-Weighting inference.
-}

module Inference.MC.LW
  ( -- * Inference wrapper functions
    lwWith
    -- * Inference effect handlers
  , lw
  , likelihood
  , joint
  ) where

import Data.Bifunctor ( Bifunctor(first), second, bimap )
import Control.Monad ( replicateM )
import Effects.MulDist ( Sample, Observe(..), MulDist, pattern ObsPrj, pattern SampPrj )
import Effects.EnvRW ( EnvRW )
import Effects.State ( modify, handleState, State )
import Env ( Env )
import LogP ( LogP )
import Inference.MC.SIM as SIM (defaultSample)
import Model ( conditionWith, MulModel, Model )
import Dist ( logProb )
import Comp ( discharge, Comp(..), Handler, handleWith, runImpure )
import Sampler ( Sampler )

-- | Top-level wrapper for Likelihood-Weighting (LW) inference
lwWith
  -- | number of LW iterations
  :: Int
  -- | model
  -> MulModel env [EnvRW env, MulDist, Sampler] a
  -- | input model environment
  -> Env env
  -- | [(output model environment, likelihood-weighting)]
  -> Sampler [(Env env, Double)]
lwWith n gen_model env_in = do
  let model = conditionWith env_in gen_model
  map (bimap snd exp) <$> replicateM n (lw model)

-- | Handler for one iteration of LW
lw
  :: Comp [Observe, Sample, Sampler] a
  -- | ((model output, sample trace), likelihood-weighting)
  -> Sampler (a, LogP)
lw = runImpure . SIM.defaultSample . likelihood

-- | Handle each @Observe@ operation by accumulating the log-likelihood P(Y | X)
likelihood :: Handler Observe es a (a, LogP)
likelihood  = handleWith 0 (\lρ x -> Val (x, lρ)) hop
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
