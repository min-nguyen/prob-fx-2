{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

module Fused.Algebra where

import Fused.Sum
import Data.Functor.Compose
import Data.Functor.Identity

{- 
  Higher-order signatures, sig m a, are a refinement of first-order signatures, sig k. In particular, the rest of the computation 'k' corresponds to a monadic computation 'm a'.

    data Sig (m :: * -> *) (a :: *) where
      Op :: arg -> Sig m a

  We understand this as: the syntax of 'Sig' has an underlying computation type 'm'.
-}

type Handler ctx n m = forall x. ctx (n x) -> m (ctx x)

infixr 1 ~<~
(~<~) :: (Functor n, Functor ctx1) => Handler ctx1 m n -> Handler ctx2 l m -> Handler (Compose ctx1 ctx2) l n
hdl1 ~<~ hdl2 = fmap Compose . hdl1 . fmap hdl2 . getCompose

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

-- | Thread a composed handler and input state through the algebra for some underlying signature.
thread
  :: ( Functor ctx1, Functor ctx2, Algebra sig m)
  => Handler (Compose ctx1 ctx2) n m
  -> sig n a
  -> ctx1 (ctx2 ())
  -> m (ctx1 (ctx2 a))
thread hdl sig = fmap getCompose . alg hdl sig . Compose

-- | Run an action exhausted of effects to produce its final result value.
run :: Identity a -> a
run = runIdentity
