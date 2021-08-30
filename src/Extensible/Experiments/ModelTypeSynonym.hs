{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes, OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Extensible.Experiments.ModelTypeSynonym where

import Extensible.Dist
import Extensible.Freer
import qualified Extensible.OpenProduct as OP
import Extensible.AffineReader

-- | Type synonym version of Model
type Model env ts v = (Member Dist ts, Member (AffReader env) ts, Member Sample ts) => Freer ts v

normal :: Double -> Double -> Model env ts Double
normal mu sigma = do
  send (NormalDist mu sigma Nothing Nothing)

normal' :: forall env ts x. OP.Lookup (OP.AsList env) x [Double]
  => Double -> Double -> OP.Var x
  -> Model env ts Double
normal' mu sigma field = do
  let tag = Just $ OP.varToStr field
  maybe_y <- ask @env field
  send (NormalDist mu sigma maybe_y tag)

-- | The reason we need to use type applications to apply the type 'env' to 'normal' is because the "constructors" of Model are exactly the constructors of Freer, which can't possibly provide an appropriate typeclass dictionary.

f :: forall env ts .  OP.Observables env '["y", "m", "c", "σ"] Double => (Member Dist ts, Member (AffReader env) ts, Member Sample ts) => Freer ts ()
f = do
  normal' @env 0 3 #y
  return ()

f' :: forall env ts .  OP.Observables env '["y", "m", "c", "σ"] Double => Model env ts ()
f' = do
  normal' @env 0 3 #y
  return ()
