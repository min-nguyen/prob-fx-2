{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | For lifting arbitrary monadic computations into an algebraic effect setting.
-}

module Effects.Lift (
    Lift(..)
  , lift
  , liftPrint
  , handleLift) where

import Prog ( call, Member(prj), LastMember, Prog(..) )
import Sampler

-- | Lift a monadic computation @m a@ into the effect @Lift m@
newtype Lift m a = Lift (m a)

-- | Wrapper function for calling @Lift@ as the last effect
lift :: LastMember (Lift m) es => m a -> Prog es a
lift = call . Lift

liftPrint :: LastMember (Lift Sampler) es => String -> Prog es ()
liftPrint = lift . liftIO . print

-- | Handle @Lift m@ as the last effect
handleLift :: Monad m => Prog '[Lift m] w -> m w
handleLift (Val x) = return x
handleLift (Op u q) = case prj u of
     Just (Lift m) -> m >>= handleLift . q
     Nothing -> error "Impossible: Nothing cannot occur"

