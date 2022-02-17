{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, TypeApplications, UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Extensible.AffineReader where

import Extensible.State
import Extensible.Freer
-- import Data.Extensible hiding (Member)
import qualified Extensible.OpenProduct as OP
-- import Extensible.Model
import Control.Lens hiding ((:>))
import Util

data AffReader env a where
  Ask :: OP.Observable env x a => OP.Var x -> AffReader env (Maybe a)

ask :: forall env ts x a. Member (AffReader env) ts => OP.Observable env x a => OP.Var x -> Freer ts (Maybe a)
ask k = Free (inj (Ask k :: AffReader env (Maybe a))) Pure

pattern AskPatt :: () => ((v :: *) ~ (Maybe a :: *), OP.Observable env x a) => OP.Var x -> Union ((AffReader env) ': r) v
pattern AskPatt x <- (decomp -> Right (Ask x))

runAffReader' :: forall env ts a.
  OP.OpenProduct (OP.AsList env) -> Freer (AffReader env ': ts) a -> Freer ts a
runAffReader' env (Pure x) = return x
runAffReader' env (Free (AskPatt key) k) = do
    let ys = OP.getOP key env
        y  = maybeHead ys
        env' = OP.setOP key (safeTail ys) env
    runAffReader env' (k y)
runAffReader' env (Free (Other u) k) = Free u (runAffReader' env . k)
-- runAffReader' env (Free u k) = case decomp u of
--   (Right (Ask key)) -> do
--     let ys = OP.getOP key env
--         y  = maybeHead ys
--         env' = OP.setOP key (safeTail ys) env
--     runAffReader env' (k y)
--   Left u -> Free u (runAffReader env . k)

runAffReader :: forall env ts a.
  OP.OpenProduct (OP.AsList env) -> Freer (AffReader env ': ts) a -> Freer ts a
runAffReader env0 = handleRelaySt env0
  (\env x    -> return x)
  (\env tx k ->
    case tx of
      Ask key -> let ys   = OP.getOP key env
                     y    = maybeHead ys
                     env' = OP.setOP key (safeTail ys) env
                 in  k env' y)


-- pattern AskPatt :: OP.Observable env x a =>  OP.Var x -> Union ts x
-- pattern AskPatt :: FindElem (AffReader env) ts => (OP.Observable env x a) => OP.Var x1 -> Union ts a
