{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators, TypeApplications, UndecidableInstances #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Effects.MonadBayes where

import Prog
import Effects.Dist
import Control.Monad.Bayes.Class
import Numeric.Log
import Control.Lens (Field14(_14))

handleBayes :: forall m es a. MonadInfer m => Prog '[Dist] a -> m a
handleBayes (Val x)  = return x
handleBayes (Op u k) = case discharge u of
    Right d ->
      case getObs d of
          Just y  -> do let p = logProb d y
                        score (Exp p)
                        handleBayes (k y)
          Nothing -> do y <- sampleBayes d
                        handleBayes (k y)
    Left  u'  -> error "impossible; Dist must be the last effect"

sampleBayes :: MonadSample m => Dist a -> m a
sampleBayes (UniformDist a b _ _)   = uniform a b
sampleBayes (NormalDist mu std _ _) = normal mu std
sampleBayes (GammaDist k t _ _)     = gamma k t
sampleBayes (BetaDist a b _ _)      = beta a b
sampleBayes (BernoulliDist p _ _)   = bernoulli p
sampleBayes (PoissonDist l _ _)     = poisson l
sampleBayes _                       = error "not supported"