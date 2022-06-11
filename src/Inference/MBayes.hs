{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators, TypeApplications, UndecidableInstances #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Inference.MBayes where

import Prog
import Effects.Dist hiding (handleDist)
import Effects.Lift
import Control.Monad.Bayes.Class
import Control.Monad
import Numeric.Log
import Control.Monad.IO.Class
import qualified Data.Vector as Vec

{-  Handle Dist as last effect directly into a monad -}

handleDist :: forall m es a. MonadInfer m => Prog '[Dist] a -> m a
handleDist (Val x)  = return x
handleDist (Op u k) = case discharge u of
    Right d ->
      case getObs d of
          Just y  -> do let p = logProb d y
                        score (Exp p)
                        handleDist (k y)
          Nothing -> do y <- sampleBayes d
                        handleDist (k y)
    Left  u'  -> error "impossible; Dist must be the last effect"

{- Handle Obs and Sample separately, using the "Lift m" effect and a MTL approach to "m" -}

handleObs :: forall m es a. MonadCond m => Member (Lift m) es => Prog (Observe : es) a -> Prog es a
handleObs (Val x)  = Val x
handleObs (Op u k) = case discharge u of
  Left u' -> do
     Op u' (handleObs @m . k)
  Right (Observe d y _) -> 
      do let p = logProb d y
         lift @m $ score (Exp p)
         handleObs @m (k y)

handleSamp :: forall m es a. (MonadIO m, MonadSample m) => Member (Lift m) es => Prog (Sample : es) a -> Prog es a
handleSamp (Val x) = return x
handleSamp (Op u k) = case discharge u of
  Left u' -> do
     Op u' (handleSamp @m . k)
  Right (Sample d _) -> 
      do y <- lift @m $ sampleBayes d
         handleSamp @m (k y)
  Right (Printer s) ->
      do lift @m (liftIO $ print "")
         handleSamp @m (k ())

sampleBayes :: MonadSample m => Dist a -> m a
sampleBayes (UniformDist a b _ _)     = uniform a b
sampleBayes (DiscreteDist as _ _)     = categorical (Vec.fromList as)
sampleBayes (CategoricalDist as _ _)  = categorical (Vec.fromList (map snd as)) >>= (pure . fst . (as !!))
sampleBayes (NormalDist mu std _ _)   = normal mu std
sampleBayes (GammaDist k t _ _)       = gamma k t
sampleBayes (BetaDist a b _ _)        = beta a b
sampleBayes (BernoulliDist p _ _)     = bernoulli p
sampleBayes (BinomialDist n p _ _)    = replicateM n (bernoulli p) >>= (pure . length . filter (== True))
sampleBayes (PoissonDist l _ _)       = poisson l
sampleBayes (DirichletDist as _ _)    = dirichlet (Vec.fromList as) >>= pure . Vec.toList
sampleBayes (DistDict d)              = error ("Sampling from " ++ show d ++ " is not supported")
