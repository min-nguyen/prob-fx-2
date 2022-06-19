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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Fused.Lift where

import Data.Functor.Identity
import Fused.Algebra ( Algebra(..), send )
import Fused.Sum

type Handler ctx n m = forall x. ctx (n x) -> m (ctx x)

data LiftEff m m' a where
  LiftWith :: m a -> LiftEff m m' a

newtype Lift m a = Lift { runLift :: m a } deriving Functor

instance Monad m => Algebra (LiftEff IO) IO where 
  alg :: (Functor ctx) => 
          Handler ctx n IO -> (LiftEff IO) n a -> ctx () -> IO (ctx a)
  alg hdl (LiftWith mop) ctx = (\x -> fmap (const x) ctx) <$> mop

instance Monad m => Algebra (LiftEff Identity) Identity where 
  alg :: (Functor ctx) => 
          Handler ctx n Identity -> (LiftEff Identity) n a -> ctx () -> Identity (ctx a)
  alg hdl (LiftWith mop) ctx = (\x -> fmap (const x) ctx) <$> mop