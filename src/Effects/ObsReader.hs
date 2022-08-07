{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{- | The effect for reading observable variables from a model environment.
-}

module Effects.ObsReader (
    ObsReader(..)
  , ObsWriter(..)
  , oAsk
  , oTell
  , handleObsRead
  , handleObsWrite) where

import Prog ( call, discharge, Member, Prog(..) )
import Env ( Env, Var, Observable(..), emptyEnv )
import Util ( safeHead, safeTail )

-- | The effect for reading observed values from a model environment @env@
data ObsReader env a where
  -- | Given the observable variable @x@ is assigned a list of type @[a]@ in @env@, attempt to retrieve its head value.
  OAsk :: Observable env x a
        => Var x                    -- ^ variable @x@ to read from
        -> ObsReader env (Maybe a)  -- ^ the head value from @x@'s list

-- | Wrapper function for calling @OAsk@
oAsk :: forall env es x a. (Member (ObsReader env) es, Observable env x a)
  => Var x
  -> Prog es (Maybe a)
oAsk x = call (OAsk @env x)


-- | Handle the @OAsk@ requests of observable variables
handleObsRead ::
  -- | initial model environment
     Env env
  -> Prog (ObsReader env ': es) a
  -> Prog es a
handleObsRead env_in  (Val x) = return x
handleObsRead env_in  (Op op k) = case discharge op of
  Right (OAsk x) ->
    let vs       = get x env_in
        maybe_v  = safeHead vs
        env_in'     = set x (safeTail vs) env_in
    in  handleObsRead env_in' (k maybe_v)
  Left op' -> Op op' (handleObsRead env_in . k)


-- | The effect for writing observed values to a model environment @env@
data ObsWriter env a where
  OTell :: (Observable env x a, Show a)
        => Var x
        -> a
        -> ObsWriter env ()

oTell :: forall env es x a. (Show a, Member (ObsWriter env) es, Observable env x a)
  => Var x
  -> a
  -> Prog es ()
oTell x v = call (OTell @env x v)

handleObsWrite ::
     Env env
  -> Prog (ObsWriter env ': es) a
  -> Prog es (a, Env env)
handleObsWrite env_in = loop (emptyEnv env_in)
  where
    loop :: Env env -> Prog (ObsWriter env ': es) a -> Prog es (a, Env env)
    loop env_out (Val x) = return (x, env_out)
    loop env_out (Op op k) = case discharge op of
      Right (OTell x v) ->
        let vs        = get x env_out
            env_out'  = set x (vs ++ [v]) env_out
        in  loop env_out' (k ())
      Left op' -> Op op' (loop env_out . k)
