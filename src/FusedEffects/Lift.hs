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

module FusedEffects.Lift where

import Data.Functor.Identity
import FusedEffects.Algebra ( Algebra(..), send )
import FusedEffects.Sum

type Handler ctx n m = forall x. ctx (n x) -> m (ctx x)

data (Lift n) m k where -- lifts a monadic computation n, into a compound monadic computation m
  LiftWith :: (forall ctx. Functor ctx => ctx () -> n (ctx a)) -> Lift n m a
  
liftWith :: (Member (Lift n) sig, Algebra sig m) 
  => (forall ctx. Functor ctx => ctx () -> n (ctx a)) -> m a
liftWith with = send (LiftWith with)

sendM :: (Member (Lift n) sig, Algebra sig m, Functor n) => n a -> m a
sendM m = liftWith (\ctx -> (<$ ctx) <$> m)

instance Algebra (Lift IO) IO where 
  alg :: Functor ctx => 
         Handler ctx n IO -> (Lift IO) n a -> ctx () -> IO (ctx a)
  alg _ (LiftWith with) ctx = with ctx

instance Algebra (Lift Identity) Identity where 
  alg :: (Functor ctx) => 
          Handler ctx n Identity -> (Lift Identity) n a -> ctx () -> Identity (ctx a)
  alg _ (LiftWith with) ctx = with ctx
