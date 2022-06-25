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


module Fused.ObsReader where

import Fused.Algebra
import Fused.Sum
import Env
import Util

data ObsReader env m (a :: *) where
  Ask :: Observable env x a => ObsVar x -> ObsReader env m (Maybe a)

ask :: forall sig env es x m a. (Member (ObsReader env) sig, Algebra sig m) => Observable env x a => ObsVar x -> m (Maybe a)
ask k = send (Ask k :: ObsReader env m (Maybe a))

-- handleObsRead :: forall env es a.
--   Env env -> Prog (ObsReader env ': es) a -> Prog es a
-- handleObsRead env (Val x) = return x
-- handleObsRead env (Op (AskPatt key) k) = do
--     let ys = get key env
--         y  = maybeHead ys
--         env' = set key (safeTail ys) env
--     handleObsRead env' (k y)
-- handleObsRead env (Op (Other u) k) = Op u (handleObsRead env . k)

-- handleObsRead' :: forall env es a.
--   Env env -> Prog (ObsReader env ': es) a -> Prog es a
-- handleObsRead' env0 = handleRelaySt env0
--   (\env x    -> return x)
--   (\env tx k ->
--     case tx of
--       Ask key -> let ys   = get key env
--                      y    = maybeHead ys
--                      env' = set key (safeTail ys) env
--                  in  k env' y)
      