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
  , oAsk
  , handleObsRead) where

import Prog ( call, discharge, Member, Prog(..) )
import Env ( Env, Var, Observable(..) )
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
handleObsRead env (Val x) = return x
handleObsRead env (Op op k) = case discharge op of
  Right (OAsk x) ->
    let vs       = get x env
        maybe_v  = safeHead vs
        env'     = set x (safeTail vs) env
    in  handleObsRead env' (k maybe_v)
  Left op' -> Op op' (handleObsRead env . k)
