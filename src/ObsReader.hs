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

module ObsReader where

import State
import Freer
-- import Data.Extensible hiding (Member)
import ModelEnv
-- import Model
import Control.Lens hiding ((:>))
import Util

data ObsReader env (a :: *) where
  Ask :: Observable env x a => ObsVar x -> ObsReader env (Maybe a)

ask :: forall env es x a. Member (ObsReader env) es => Observable env x a => ObsVar x -> Prog es (Maybe a)
ask k = Op (inj (Ask k :: ObsReader env (Maybe a))) Val

pattern AskPatt :: () => (v ~ Maybe a, Observable env x a) => ObsVar x -> EffectSum (ObsReader env : es) v
pattern AskPatt x <- (decomp -> Right (Ask x))

runObsReader :: forall env es a.
  ModelEnv env -> Prog (ObsReader env ': es) a -> Prog es a
runObsReader env (Val x) = return x
runObsReader env (Op (AskPatt key) k) = do
    let ys = getOP key env
        y  = maybeHead ys
        env' = setOP key (safeTail ys) env
    runObsReader env' (k y)
runObsReader env (Op (Other u) k) = Op u (runObsReader env . k)
-- runObsReader' env (Op u k) = case decomp u of
--   (Right (Ask key)) -> do
--     let ys = getOP key env
--         y  = maybeHead ys
--         env' = setOP key (safeTail ys) env
--     runObsReader env' (k y)
--   Left u -> Op u (runObsReader env . k)

runObsReader' :: forall env es a.
  ModelEnv env -> Prog (ObsReader env ': es) a -> Prog es a
runObsReader' env0 = handleRelaySt env0
  (\env x    -> return x)
  (\env tx k ->
    case tx of
      Ask key -> let ys   = getOP key env
                     y    = maybeHead ys
                     env' = setOP key (safeTail ys) env
                 in  k env' y)


-- pattern AskPatt :: Observable env x a =>  ObsVar x -> EffectSum es x
-- pattern AskPatt :: FindElem (ObsReader env) es => (Observable env x a) => ObsVar x1 -> EffectSum es a
