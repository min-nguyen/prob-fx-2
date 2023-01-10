{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE FlexibleContexts #-}


{-# LANGUAGE TypeApplications #-}


{- | The effect for reading from and writing to observable variables from a model environment.
-}

module Effects.EnvRW (
    EnvRW(..)
  , Effects.EnvRW.read
  , write
  , handleEnvRW) where

import Prog ( call, discharge, Member, Prog(..) )
import Env ( Env, Var, Observable(..), empty, reverse )
import Util ( safeHead, safeTail )

-- | The 'observable read-write' effect for reading from and writing to an environment @env@
data EnvRW env a where
  -- | Given observable variable @x@ is assigned a list of type @[a]@ in @env@, attempt to retrieve a value from the list.
  Read  :: Observable env x a
        => Var x                    -- ^ variable @x@ to read from
        -> EnvRW env (Maybe a)      -- ^ head value from @x@'s list
  -- | Given observable variable @x@ is assigned a list of type @[a]@ in @env@, write a value to the the list.
  Write :: Observable env x a
        => Var x                    -- ^ variable @x@ to write to
        -> a                        -- ^ value to write
        -> EnvRW env ()

-- | Wrapper function for calling @Read@
read :: forall env es x a. (Member (EnvRW env) es, Observable env x a)
  => Var x
  -> Prog es (Maybe a)
read x = call (Read @env x)

-- | Wrapper function for calling @Write@
write :: forall env es x a. (Member (EnvRW env) es, Observable env x a)
  => Var x
  -> a
  -> Prog es ()
write x v = call (Write @env x v)

{- Handle the @Read@ operations by reading from an input model environment,
   and handle the @Write@ operations by writing to an output model environment  -}
handleEnvRW :: forall env es a.
  -- | input model environment
     Env env
  -> Prog (EnvRW env ': es) a
  -- | (final result, output model environment)
  -> Prog es (a, Env env)
handleEnvRW env_in = loop env_in (Env.empty env_in) where
  loop :: Env env -> Env env -> Prog (EnvRW env ': es) a -> Prog es (a, Env env)
  loop env_in env_out (Val x) = return (x, Env.reverse env_out)
  loop env_in env_out (Op op k) = case discharge op of
    Right (Read x) ->
      let vs       = get x env_in
          maybe_v  = safeHead vs
          env_in'  = set x (safeTail vs) env_in
      in  loop env_in' env_out (k maybe_v)
    Right (Write x v) ->
      let vs        = get x env_out
          env_out'  = set x (v:vs) env_out
      in  loop env_in env_out' (k ())
    Left op' -> Op op' (loop env_in env_out . k)