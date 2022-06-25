{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module FusedEffects.SrcComparison.AlgebraSrc where

import FusedEffects.Sum
import Data.Functor.Identity
-- import FusedEffects.SrcComparison.LiftSrc

type Handler ctx n m = forall x. ctx (n x) -> m (ctx x)

class Monad m => Algebra sig m | m -> sig where
  alg
    :: Functor ctx
    => Handler ctx n m -- ^ A 'Handler' lowering computations inside the effect into the carrier type @m@.
    -> sig n a         -- ^ The effect signature to be interpreted.
    -> ctx ()          -- ^ The initial state.
    -> m (ctx a)       -- ^ The interpretation of the effect in @m@.

type Has eff sig m = (Members eff sig, Algebra sig m)

-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Member eff sig, Algebra sig m) => eff m a -> m a
send sig = runIdentity <$> alg (fmap Identity . runIdentity) (inj sig) (Identity ())

run :: Identity a -> a
run = runIdentity 

-- | Compose two contexts, and compose two handlers
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving Functor

infixr 1 ~<~
(~<~) :: (Functor n, Functor ctx1) => Handler ctx1 m n -> Handler ctx2 l m -> Handler (Compose ctx1 ctx2) l n
hdl1 ~<~ hdl2 = fmap Compose . hdl1 . fmap hdl2 . getCompose

-- | Thread a composed handler and input state through the algebra for some underlying signature.
thread :: (Functor ctx1, Functor ctx2, Algebra sig m)
  => Handler (Compose ctx1 ctx2) n m
  -> sig n a
  -> ctx1 (ctx2 ())
  -> m (ctx1 (ctx2 a))
thread hdl op = fmap getCompose . alg hdl op . Compose

newtype Swap s a = Swap { getSwap :: (a, s) }
  deriving Functor

