{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Effects.ObsReader where

import Prog
import Env
import Util

-- ||| Effect for reading observable variables
data ObsReader env a where
  Ask :: Observable env x a => Var x -> ObsReader env (Maybe a)

ask :: forall env es x a. Member (ObsReader env) es => Observable env x a => Var x -> Prog es (Maybe a)
ask x = call (Ask @env x)

-- ||| Handle requests to read observable variables
handleObsRead :: forall env es a.
  Env env -> Prog (ObsReader env ': es) a -> Prog es a
handleObsRead env (Val x) = return x
handleObsRead env (Op op k) = case discharge op of
  Right (Ask x) ->
    let vs       = get x env
        maybe_v  = safeHead vs
        env'     = set x (safeTail vs) env
    in  handleObsRead env' (k maybe_v)
  Left op' -> Op op' (handleObsRead env . k)
