{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Effects.Lift where

import Data.Function (fix)
import Prog

newtype Lift m a = Lift (m a)

-- By using SetMember, it is possible to assert that the lifted type occurs
-- only once in the effect list
lift :: (LastMember (Lift m) es, Member (Lift m) es) => m a -> Prog es a
lift = call . Lift

handleLift :: forall m w. Monad m => Prog '[Lift m] w -> m w
handleLift (Val x) = pure x
handleLift (Op u q) = case prj u of
     Just (Lift m) -> m >>= handleLift . q
     Nothing -> error "Impossible: Nothing cannot occur"