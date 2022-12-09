{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

{- | For lifting arbitrary monadic computations into an algebraic effect setting.
-}

module Effects.Lift (
    Lift(..)
  , HasSampler
  , lift
  , liftPrint
  , liftPutStrLn
  , handleLift) where

import Prog ( call, Member(prj), LastMember, Prog(..) )
import Sampler

-- | Lift a monadic computation @m a@ into the effect @Lift m@
newtype Lift m a = Lift (m a)

type HasSampler es = LastMember (Lift Sampler) es

-- | Wrapper function for calling @Lift@ as the last effect
lift :: LastMember (Lift m) es => m a -> Prog es a
lift = call . Lift

liftPrint :: LastMember (Lift Sampler) es => Show a => a -> Prog es ()
liftPrint = lift . liftIO . print

liftPutStrLn :: LastMember (Lift Sampler) es => String -> Prog es ()
liftPutStrLn = lift . liftIO . putStrLn

-- | Handle @Lift m@ as the last effect
handleLift :: Monad m => Prog '[Lift m] w -> m w
handleLift (Val x) = return x
handleLift (Op u q) = case prj u of
     Just (Lift m) -> m >>= handleLift . q
     Nothing -> error "Impossible: Nothing cannot occur"

