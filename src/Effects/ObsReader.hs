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

module Effects.ObsReader where

import Effects.State
import Prog
-- import Data.Extensible hiding (Member)
import Env
-- import Model
import Util

data ObsReader env (a :: *) where
  Ask :: Observable env x a => ObsVar x -> ObsReader env (Maybe a)

ask :: forall env es x a. Member (ObsReader env) es => Observable env x a => ObsVar x -> Prog es (Maybe a)
ask k = Op (inj (Ask k :: ObsReader env (Maybe a))) Val

pattern AskPatt :: () => (v ~ Maybe a, Observable env x a) => ObsVar x -> EffectSum (ObsReader env : es) v
pattern AskPatt x <- (discharge -> Right (Ask x))

handleObsRead :: forall env es a.
  Env env -> Prog (ObsReader env ': es) a -> Prog es a
handleObsRead env (Val x) = return x
handleObsRead env (Op (AskPatt key) k) = do
    let ys = get key env
        y  = maybeHead ys
        env' = set key (safeTail ys) env
    handleObsRead env' (k y)
handleObsRead env (Op (Other u) k) = Op u (handleObsRead env . k)

handleObsRead' :: forall env es a.
  Env env -> Prog (ObsReader env ': es) a -> Prog es a
handleObsRead' env0 = handleRelaySt env0
  (\env x    -> return x)
  (\env tx k ->
    case tx of
      Ask key -> let ys   = get key env
                     y    = maybeHead ys
                     env' = set key (safeTail ys) env
                 in  k env' y)


-- pattern AskPatt :: Observable env x a =>  ObsVar x -> EffectSum es x
-- pattern AskPatt :: FindElem (ObsReader env) es => (Observable env x a) => ObsVar x1 -> EffectSum es a
