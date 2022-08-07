{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{- | This implements the model environments that users must provide upon running a model;
     such environments assign traces of values to the "observable variables" (random
     variables which can be conditioned against) of a model.
-}

module Env
  ( -- * Observable variable
    Var(..)
  , varToStr
  , vnil
  , emptyEnv
  , (<#>)
  , Vars(..)
  , ContainsVars(..)
    -- * Model environment
  , Assign(..)
  , Env(..)
  , enil
  , (<:>)
  , Observable(..)
  , Observables(..)
  , UniqueVar
  , LookupType) where

import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy(Proxy) )
import Effects.Dist ( Tag )
import FindElem ( FindElem(..), Idx(..) )
import GHC.OverloadedLabels ( IsLabel(..) )
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Unsafe.Coerce ( unsafeCoerce )

-- * Observable variable

-- | A container for a variable name @x@ represented as a type-level string.
data Var (x :: Symbol) where
  Var :: KnownSymbol x => Var x

-- | Allows the syntax @#x@ to be automatically lifted to the type @Var "x"@.
instance (KnownSymbol x, x ~ x') => IsLabel x (Var x') where
  fromLabel = Var

-- | Convert an observable variable from a type-level string to a value-level string
varToStr :: forall x. Var x -> String
varToStr Var = symbolVal (Proxy @x)

-- | A container for many observable variable names
data Vars (xs :: [Symbol]) where
  VNil  :: Vars '[]
  VCons :: Vars xs -> Vars (x : xs)

-- | Empty set of variables
vnil :: Vars '[]
vnil = VNil

infixr 5 <#>
-- | Prepend a variable name to a list of variables
(<#>) :: Var x -> Vars xs -> Vars (x : xs)
x <#> xs = VCons xs


-- * Model environment

{- | A model environment assigning traces (lists) of observed values to observable
     variables i.e. the type @Env ((x := a) : env)@ indicates @x@ is assigned a value
     of type @[a]@.
-}
data Env (env :: [Assign Symbol *]) where
  ENil  :: Env '[]
  ECons :: [a]
        -> Env env
        -> Env (x := a : env)

-- | Assign or associate a variable @x@ with a value of type @a@
data Assign x a = x := a

-- | Empty model environment
enil :: Env '[]
enil = ENil

emptyEnv :: Env env -> Env env
emptyEnv (ECons _ env) = ECons [] (emptyEnv env)
emptyEnv ENil = ENil

infixr 5 <:>
-- | Prepend a variable assignment to a model environment
(<:>) :: UniqueVar x env ~ 'True => Assign (Var x) [a] -> Env env -> Env ((x ':= a) ': env)
(_ := a) <:> env = ECons a env

instance (KnownSymbol x, Show a, Show (Env env)) => Show (Env ((x := a) ': env)) where
  show (ECons a env) = varToStr (Var @x) ++ ":=" ++ show a ++ ", " ++ show env

instance Show (Env '[]) where
  show ENil = "[]"

instance FindElem x ((x := a) : env) where
  findElem = Idx 0
instance {-# OVERLAPPABLE #-} FindElem x env => FindElem x ((x' := a) : env) where
  findElem = Idx $ 1 + unIdx (findElem :: Idx x env)

-- | Specifies that an environment @Env env@ has an observable variable @x@ whose observed values are of type @a@
class (FindElem x env, LookupType x env ~ a)
  => Observable env x a where
  -- | Get the trace of observed values for @x@ from @env@
  get  :: Var x -> Env env -> [a]
  -- | Set the trace of observed values for @x@ from @env@
  set  :: Var x -> [a] -> Env env -> Env env

instance (FindElem x env, LookupType x env ~ a)
  => Observable env x a where
  get _ env = f idx env
    where idx = unIdx $ findElem @x @env
          f :: Int -> Env env' -> [a]
          f n (ECons a env) | n == 0    = unsafeCoerce a
                            | otherwise = f (n - 1) env
  set _ a' env = f idx env
    where idx = unIdx $ findElem @x @env
          f :: Int -> Env env' -> Env env'
          f n (ECons a env) | n == 0    = ECons (unsafeCoerce a') env
                            | otherwise = ECons a (f (n - 1) env)

-- | For each observable variable @x@ in @xs@, construct the constraint @Observable env x a@
type Observables :: [Assign Symbol Type] -> [Symbol] -> Type -> Constraint
type family Observables env xs a :: Constraint where
  Observables env (x ': xs) a = (Observable env x a, Observables env xs a)
  Observables env '[] a = ()

-- | Extract the observable variables from a model environment type
type GetVars :: [Assign Symbol Type] -> [Symbol]
type family GetVars env where
  GetVars (x := a : env) = x : GetVars env

-- | Check whether an observable variable @x@ is unique in model environment @env@
type UniqueVar :: Symbol -> [Assign Symbol Type] -> Bool
type family UniqueVar x env where
  UniqueVar x ((x ':= a) : env)  = False
  UniqueVar x ((x' ':= a) : env) = UniqueVar x env
  UniqueVar x '[] = True

-- | Retrieve the type of observed values for variable @x@ from model environment @env@
type LookupType :: Symbol -> [Assign Symbol Type] -> Type
type family LookupType x env where
  LookupType x ((x := a) : env) = a
  LookupType x ((x' := a) : env) = LookupType x env

-- | Enforce that @xs@ is a subset of observable variable names from @env@.
{-   This class is used as a type-safe interface for allowing users to specify
     valid observable variable names with respect to an environment; these can
     then later be converted to normal strings to be used by backend inference methods.
-}
class ContainsVars (env :: [Assign Symbol *]) (xs :: [Symbol])  where
  -- | Convert a set of type-level strings @xs@ to value-level strings.
  varsToStrs :: Vars xs -> [Tag]

instance ContainsVars env '[] where
  varsToStrs _    = []

instance (FindElem x (GetVars env), KnownSymbol x, ContainsVars env xs) => ContainsVars env (x : xs)  where
  varsToStrs (VCons xs)  = varToStr (Var @x) : varsToStrs @env @xs xs

