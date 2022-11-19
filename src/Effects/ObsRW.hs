{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{- | The effect for reading from and writing to observable variables from a model environment.
-}

module Effects.ObsRW (
    ObsRW(..)
  , oAsk
  , oTell
  , handleObsRW) where

import Prog ( call, discharge, Member, Prog(..) )
import Env ( Env, Var, Observable(..), empty, reverse )
import Util ( safeHead, safeTail )

-- | The 'observable read-write' effect for reading from and writing to an environment @env@
data ObsRW env a where
  -- | Given observable variable @x@ is assigned a list of type @[a]@ in @env@, attempt to retrieve a value from the list.
  OAsk  :: Observable env x a
        => Var x                    -- ^ variable @x@ to read from
        -> ObsRW env (Maybe a)      -- ^ head value from @x@'s list
  -- | Given observable variable @x@ is assigned a list of type @[a]@ in @env@, write a value to the the list.
  OTell :: Observable env x a
        => Var x                    -- ^ variable @x@ to write to
        -> a                        -- ^ value to write
        -> ObsRW env ()

-- | Wrapper function for calling @OAsk@
oAsk :: forall env es x a. (Member (ObsRW env) es, Observable env x a)
  => Var x
  -> Prog es (Maybe a)
oAsk x = call (OAsk @env x)

-- | Wrapper function for calling @OTell@
oTell :: forall env es x a. (Show a, Member (ObsRW env) es, Observable env x a)
  => Var x
  -> a
  -> Prog es ()
oTell x v = call (OTell @env x v)

{- Handle the @OAsk@ operations by reading from an input model environment,
   and handle the @OTell@ operations by writing to an output model environment  -}
handleObsRW :: forall env es a.
  -- | input model environment
     Env env
  -> Prog (ObsRW env ': es) a
  -- | (final result, output model environment)
  -> Prog es (a, Env env)
handleObsRW env_in = loop env_in (Env.empty env_in) where
  loop :: Env env -> Env env -> Prog (ObsRW env ': es) a -> Prog es (a, Env env)
  loop env_in env_out (Val x) = return (x, Env.reverse env_out)
  loop env_in env_out (Op op k) = case discharge op of
    Right (OAsk x) ->
      let vs       = get x env_in
          maybe_v  = safeHead vs
          env_in'  = set x (safeTail vs) env_in
      in  loop env_in' env_out (k maybe_v)
    Right (OTell x v) ->
      let vs        = get x env_out
          env_out'  = set x (v:vs) env_out
      in  loop env_in env_out' (k ())
    Left op' -> Op op' (loop env_in env_out . k)