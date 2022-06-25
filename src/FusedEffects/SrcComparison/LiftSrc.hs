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

module FusedEffects.SrcComparison.LiftSrc (Lift(..)) where

import Data.Functor.Identity
import FusedEffects.SrcComparison.AlgebraSrc ( Algebra(..), send )
import FusedEffects.Sum

type Handler ctx n m = forall x. ctx (n x) -> m (ctx x)

data (Lift n) m k where
  LiftWith :: (forall ctx . Functor ctx => Handler ctx m n -> ctx () -> n (ctx a)) -> Lift n m a
  
liftWith
  :: (Member (Lift n) sig, Algebra sig m) 
  => (forall ctx. Functor ctx => Handler ctx m n -> ctx () -> n (ctx a)) -> m a
liftWith with = send (LiftWith with)

instance Algebra (Lift IO) IO where 
  alg :: Functor ctx => 
         Handler ctx n IO -> (Lift IO) n a -> ctx () -> IO (ctx a)
  alg hdl (LiftWith with) ctx = with hdl ctx

instance Algebra (Lift Identity) Identity where 
  alg :: (Functor ctx) => 
          Handler ctx n Identity -> (Lift Identity) n a -> ctx () -> Identity (ctx a)
  alg hdl (LiftWith with) ctx = with hdl ctx
