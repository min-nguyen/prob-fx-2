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
{-# LANGUAGE TypeFamilyDependencies #-}

module Extensible.AffineReader where

import Extensible.State
import Extensible.Freer
-- import Data.Extensible hiding (Member)
import qualified Extensible.OpenProduct as OP
-- import Extensible.Model
import Control.Lens hiding ((:>))
import Util

data AffReader env a where
  Ask :: OP.Observable env k a => OP.Var k -> AffReader env (Maybe a)

runAffReader :: forall env rs a.
  OP.OpenProduct (OP.AsList env) -> Freer (AffReader env ': rs) a -> Freer rs a
runAffReader env (Pure x) = return x
runAffReader env (Free u k) = case decomp u of
  Right (Ask key) -> do
    let ys = OP.getOP key env
        y  = maybeHead ys
        env' = OP.setOP key (safeTail ys) env
    runAffReader env' (k y)
  Left  u'  -> Free u' (runAffReader env . k)

ask :: forall env f k a. Member (AffReader env) f => OP.Observable env k a => OP.Var k -> Freer f (Maybe a)
ask k = Free (inj (Ask k :: AffReader env (Maybe a))) Pure
