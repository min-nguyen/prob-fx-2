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

module Extensible.IO where

import Extensible.Freer
import Data.Function (fix)

newtype Lift m a = Lift (m a)


-- By using SetMember, it is possible to assert that the lifted type occurs
-- only once in the effect list
lift :: (Member (Lift m) ts) => m a -> Prog ts a
lift = send . Lift

runLift :: forall m w. Monad m => Prog '[Lift m] w -> m w
runLift (Val x) = return x
runLift (Op u q) = case prj u of
     Just (Lift m) -> m >>= runLift . q
     Nothing -> error "Impossible: Nothing cannot occur"