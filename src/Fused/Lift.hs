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

data Lift sig m k where
  LiftWith :: m a -> Lift sig m a

-- lift :: (Member (Lift n) sig, Algebra sig m) => n a -> m a
-- lift na = send $ LiftWith na

instance Algebra (Lift IO) IO where 
  alg :: (Functor ctx) => 
          Handler ctx n IO -> (Lift IO) n a -> ctx () -> IO (ctx a)
  alg hdl (LiftWith na) ctx = hdl (fmap (const na) ctx)

instance Algebra (Lift Identity) Identity where 
  alg :: (Functor ctx) => 
          Handler ctx n Identity -> (Lift Identity) n a -> ctx () -> Identity (ctx a)
  alg hdl (LiftWith na) ctx = hdl (fmap (const na) ctx)
