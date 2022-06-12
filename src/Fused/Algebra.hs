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
  -- | Interpret an effect, running any nested actions using a 'Handler' starting from an init-ial state in @ctx@.
  --
  -- Instances receive a signature of effects containing actions in @n@ which can be lowered to @m@ using the passed 'Handler' and initial context. Continuations in @n@ can be handled after mapping into contexts returned from previous actions.
  --
  -- For example, considering the 'Algebra' instance for @'Either' e@:
  --
  -- > instance Algebra (Error e) (Either e) where
  -- >   alg hdl sig ctx = case sig of
  -- >     L (Throw e)   -> Left e
  -- >     R (Catch m h) -> either (hdl . (<$ ctx) . h) pure (hdl (m <$ ctx))
  --
  -- The 'Catch' case holds actions @m :: n x@ and @h :: e -> n x@ (for some existentially-quantified type @x@), and a continuation @k :: x -> n a@. The algebra must return @m (ctx a)@, so we have to ultimately use and lower the continuation in order to produce that type. The continuation takes an @x@, which we can get from either of the actions, after lowering them to values in @'Either' e@.
  --
  -- To that end, the algebra lifts both the action @m@ and the result of the error handler @h@ into the initial context @ctx@ before lowering them with @hdl@. The continuation @k@ is 'fmap'ed into the resulting context and then itself lowered with @hdl@.
  --
  -- By contrast, the 'Throw' case can simply return a value in 'Left', since there is no continuation to call—it represents an exceptional return—and @'Left' e :: forall a . Either e a@ (i.e. 'Left' is polymorphic in @a@).
  --
  -- Instances for monad transformers will most likely handle a signature containing multiple effects, with the tail of the signature handled by whatever monad the transformer wraps. In these cases, the tail of the signature can be delegated most conveniently using 'thread'; see the 'Algebra' instances for @transformers@ types such as 'Reader.ReaderT' and 'Except.ExceptT' for details.
  alg
    :: Functor ctx
    => Handler ctx n m -- ^ A 'Handler' lowering computations inside the effect into the carrier type @m@.
    -> sig n a         -- ^ The effect signature to be interpreted.
    -> ctx ()          -- ^ The initial state.
    -> m (ctx a)       -- ^ The interpretation of the effect in @m@.

-- | @m@ is a carrier for @sig@ containing @eff@.
type Has eff sig m = (Members eff sig, Algebra sig m)

-- | Construct a request for an effect to be interpreted by some handler later on.
send :: (Member eff sig, Algebra sig m) => eff m a -> m a
send sig = runIdentity <$> alg (fmap Identity . runIdentity) (inj sig) (Identity ())

-- | Thread a composed handler and input state through the algebra for some underlying signature.
thread
  :: ( Functor ctx1
     , Functor ctx2
     , Algebra sig m
     )
  => Handler (Compose ctx1 ctx2) n m
  -> sig n a
  -> ctx1 (ctx2 ())
  -> m (ctx1 (ctx2 a))
thread hdl sig = fmap getCompose . alg hdl sig . Compose

-- | Run an action exhausted of effects to produce its final result value.
run :: Identity a -> a
run = runIdentity
