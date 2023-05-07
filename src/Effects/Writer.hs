
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE TypeApplications #-}


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE IncoherentInstances #-}

{- | Writer effect.
-}

module Effects.Writer (
    Writer(..)
  , tell
  , tellM
  , handleWriter
  , handleWriterM) where

import Comp ( discharge, Member(inj), Comp(..), Handler )
import Model ( MulModel(..), liftHandler )

-- | Writer effect for writing to a strean @w@
data Writer w a where
  -- | Write to a stream @w@
  Tell ::
       w  -- ^ value to write
    -> Writer w ()

-- | Wrapper for @Tell@
tell :: Member (Writer w) es => w -> Comp es ()
tell w = Op (inj $ Tell w) Val

-- | Wrapper for @Tell@ inside @MulModel@
tellM :: Member (Writer w) es => w -> MulModel env es ()
tellM w = MulModel $ tell w

-- | Handle the @Writer@ effect for a stream @w@
handleWriter :: forall w es a. Monoid w
  => Handler (Writer w) es a (a, w)
handleWriter = loop mempty where
  loop ::  w -> Comp (Writer w ': es) a -> Comp es (a, w)
  loop w (Val x) = return (x, w)
  loop w (Op u k) = case discharge u of
    Right (Tell w') -> loop (w `mappend` w') (k ())
    Left u'         -> Op u' (loop w . k)

-- | Handle the @Writer@ effect inside a @MulModel@
handleWriterM :: Monoid w
  => MulModel env (Writer w : es) a
  -- | (output, final stream)
  -> MulModel env es (a, w)
handleWriterM = liftHandler handleWriter
