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

import Comp ( call, discharge, Member, Comp(..), handleWith, Handler )
import Env ( Env, Var, Observable(..), initEmpty, reverse )

-- | The 'observable read-write' effect for reading from and writing to an environment @env@
data EnvRW env a where
  -- | Given observable variable @x@ is assigned a list of type @[a]@ in @env@, attempt to retrieve a value from the list.
  EnvRead  :: Observable env x a
        => Var x                    -- ^ variable @x@ to read from
        -> EnvRW env (Maybe a)      -- ^ head value from @x@'s list
  -- | Given observable variable @x@ is assigned a list of type @[a]@ in @env@, write a value to the the list.
  EnvWrite :: Observable env x a
        => Var x                    -- ^ variable @x@ to write to
        -> a                        -- ^ value to write
        -> EnvRW env ()

-- | Wrapper function for calling @Read@
read :: forall env es x a. (Member (EnvRW env) es, Observable env x a)
  => Var x
  -> Comp es (Maybe a)
read x = call (EnvRead @env x)

-- | Wrapper function for calling @Write@
write :: forall env es x a. (Member (EnvRW env) es, Observable env x a)
  => Var x
  -> a
  -> Comp es ()
write x v = call (EnvWrite @env x v)

{- Handle the @EnvRead@ operations by reading from an input model environment,
   and handleWith the @Write@ operations by writing to an output model environment  -}
handleEnvRW :: Env env -> Handler (EnvRW env) es a (a, Env env)
handleEnvRW env_in = handleWith (env_in, Env.initEmpty env_in) hval hop where
  hval :: (Env env, Env env) -> a -> Comp es (a, Env env)
  hval (env_in, env_out) x = Val (x, Env.reverse env_out)
  hop  :: (Env env, Env env) -> EnvRW env b -> ((Env env, Env env) -> b -> Comp es (a, s)) -> Comp es (a, s)
  hop (env_in, env_out) (EnvRead x) k
    = case get x env_in of
        []      -> k (env_in, env_out) Nothing
        (v:vs)  -> k (set x vs env_in, env_out) (Just v)
  hop (env_in, env_out) (EnvWrite x v) k =
    let vs        = get x env_out
    in  k (env_in, set x (v:vs) env_out) ()