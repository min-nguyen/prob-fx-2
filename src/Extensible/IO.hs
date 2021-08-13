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
lift :: (Member (Lift m) ts) => m a -> Freer ts a
lift = send . Lift

runLift :: forall m w. Monad m => Freer '[Lift m] w -> m w
runLift (Pure x) = return x
runLift (Free u q) = case prj u of
     Just (Lift m) -> m >>= runLift . q
     Nothing -> error "Impossible: Nothing cannot occur"