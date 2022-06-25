{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications, UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module FusedEffects.Dist where

import qualified Data.Vector as Vec
import Control.Monad.Bayes.Class
import Control.Monad
import FusedEffects.Algebra
import FusedEffects.Sum
import Env
import PrimDist
import Util
import OpenSum (OpenSum)
import qualified OpenSum as OpenSum
import Numeric.Log

-- | Effect
data Dist m a = Dist { getPrimDist :: PrimDist a, getObs :: Maybe a, getTag :: Maybe String}

-- | Carrier
newtype DistC m a = DistC (MonadInfer m => m a) deriving (Functor)

runDistC :: MonadInfer m => DistC m a -> m a
runDistC (DistC m) = m

instance Monad m => Applicative (DistC m) where 
  pure x              = DistC $ pure x
  DistC x <*> DistC y = DistC (x <*> y)

instance (MonadInfer m, Monad m) => Monad (DistC m) where 
  DistC x >>= f = DistC (x >>= (runDistC . f))

-- | Algebra
instance (MonadInfer m, Algebra sig m) => Algebra (Dist :+: sig) (DistC m) where
  alg :: (MonadInfer m, Functor ctx)
      => (forall x. ctx (n x) -> DistC m (ctx x))
      -> (Dist :+: sig) n a
      -> ctx () 
      -> DistC m (ctx a)
  alg hdl sig ctx = DistC $ case sig of
    L (Dist d maybe_y _) -> case maybe_y of
        (Just y) -> do let p = logProb d y
                       score (Exp p)
                       pure (fmap (const y) ctx)
        Nothing  -> (<$ ctx) <$> sampleBayes d
    R other   -> alg (runDistC . hdl) other ctx
