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

module Extensible.ObsReader where

import Extensible.State
import Extensible.Freer
-- import Data.Extensible hiding (Member)
import Extensible.ModelEnv
-- import Extensible.Model
import Control.Lens hiding ((:>))
import Util

data ObsReader env a where
  Ask :: Observable env x a => ObsVar x -> ObsReader env (Maybe a)

ask :: forall env ts x a. Member (ObsReader env) ts => Observable env x a => ObsVar x -> Freer ts (Maybe a)
ask k = Free (inj (Ask k :: ObsReader env (Maybe a))) Pure

pattern AskPatt :: () => ((v :: *) ~ (Maybe a :: *), Observable env x a) => ObsVar x -> EffectSum (ObsReader env ': r) v
pattern AskPatt x <- (decomp -> Right (Ask x))

runObsReader :: forall env ts a.
  ModelEnv env -> Freer (ObsReader env ': ts) a -> Freer ts a
runObsReader env (Pure x) = return x
runObsReader env (Free (AskPatt key) k) = do
    let ys = getOP key env
        y  = maybeHead ys
        env' = setOP key (safeTail ys) env
    runObsReader env' (k y)
runObsReader env (Free (Other u) k) = Free u (runObsReader env . k)
-- runObsReader' env (Free u k) = case decomp u of
--   (Right (Ask key)) -> do
--     let ys = getOP key env
--         y  = maybeHead ys
--         env' = setOP key (safeTail ys) env
--     runObsReader env' (k y)
--   Left u -> Free u (runObsReader env . k)

runObsReader' :: forall env ts a.
  ModelEnv env -> Freer (ObsReader env ': ts) a -> Freer ts a
runObsReader' env0 = handleRelaySt env0
  (\env x    -> return x)
  (\env tx k ->
    case tx of
      Ask key -> let ys   = getOP key env
                     y    = maybeHead ys
                     env' = setOP key (safeTail ys) env
                 in  k env' y)


-- pattern AskPatt :: Observable env x a =>  ObsVar x -> EffectSum ts x
-- pattern AskPatt :: FindElem (ObsReader env) ts => (Observable env x a) => ObsVar x1 -> EffectSum ts a
