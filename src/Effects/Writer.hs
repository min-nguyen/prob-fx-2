
{-# LANGUAGE FlexibleContexts #-}




{- | Writer effect.
-}

module Effects.Writer (
    Writer(..)
  , tell
  , tellM
  , handleWriter
  , handleWriterM) where

import Comp ( discharge, Member(inj), Comp(..) )
import Model ( GenModel(..) )

-- | Writer effect for writing to a strean @w@
data Writer w a where
  -- | Write to a stream @w@
  Tell ::
       w  -- ^ value to write
    -> Writer w ()

-- | Wrapper for @Tell@
tell :: Member (Writer w) es => w -> Comp es ()
tell w = Op (inj $ Tell w) Val

-- | Wrapper for @Tell@ inside @GenModel@
tellM :: Member (Writer w) es => w -> GenModel env es ()
tellM w = GenModel $ tell w

-- | Handle the @Writer@ effect for a stream @w@
handleWriter :: forall w es a. Monoid w
  => Comp (Writer w ': es) a
  -- | (output, final stream)
  -> Comp es (a, w)
handleWriter = loop mempty where
  loop ::  w -> Comp (Writer w ': es) a -> Comp es (a, w)
  loop w (Val x) = return (x, w)
  loop w (Op u k) = case discharge u of
    Right (Tell w') -> loop (w `mappend` w') (k ())
    Left u'         -> Op u' (loop w . k)

-- | Handle the @Writer@ effect inside a @GenModel@
handleWriterM :: Monoid w
  => GenModel env (Writer w : es) a
  -- | (output, final stream)
  -> GenModel env es (a, w)
handleWriterM m = GenModel $ handleWriter $ runGenModel m
