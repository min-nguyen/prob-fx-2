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

module Fused.SrcComparison.LiftSrc (Lift(..)) where

import Data.Functor.Identity
import Fused.SrcComparison.AlgebraSrc ( Algebra(..), send )
import Fused.Sum

type Handler ctx n m = forall x. ctx (n x) -> m (ctx x)

data Lift sig m k where
  LiftWith :: (forall ctx . Functor ctx => Handler ctx m sig -> ctx () -> sig (ctx a)) -> Lift sig m a

instance Monad m => Algebra (Lift IO) IO where 
  alg :: (Functor ctx) => 
          Handler ctx n IO -> (Lift IO) n a -> ctx () -> IO (ctx a)
  alg hdl (LiftWith with) ctx = with hdl ctx

instance Algebra (Lift Identity) Identity where 
  alg :: (Functor ctx) => 
          Handler ctx n Identity -> (Lift Identity) n a -> ctx () -> Identity (ctx a)
  alg hdl (LiftWith with) ctx = with hdl ctx
