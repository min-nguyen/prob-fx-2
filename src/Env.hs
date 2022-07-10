{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Env where

import Data.Kind ( Constraint )
import Data.Proxy ( Proxy(Proxy) )
import Effects.Dist (Tag)
import FindElem ( FindElem(..), P(..) )
import GHC.OverloadedLabels ( IsLabel(..) )
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import qualified Data.Vector as V
import qualified GHC.TypeLits as TL
import Unsafe.Coerce ( unsafeCoerce )

-- ||| Observable variable container
data ObsVar (x :: Symbol) where
  ObsVar :: KnownSymbol x => ObsVar x

instance (KnownSymbol x, x ~ x') => IsLabel x (ObsVar x') where
  fromLabel = ObsVar

-- ||| Specification of observable variables
data ObsVars (xs :: [Symbol]) where
  ONil  :: ObsVars '[]
  OCons :: ObsVars xs -> ObsVars (x : xs)

onil :: ObsVars '[]
onil = ONil

infixr 5 <#>
(<#>) :: ObsVar x -> ObsVars xs -> ObsVars (x : xs)
x <#> xs = OCons xs

-- ||| Observable variable assignment 
data Assign x a = x := a

type family Keys env where
  Keys (x := a : env) = x : Keys env

type family UniqueKey x env where
  UniqueKey x ((x ':= a) : env) = False
  UniqueKey x ((x' ':= a) : env) = UniqueKey x env
  UniqueKey x '[] = True

type family LookupType x env where
  LookupType x ((x := a) : env) = a
  LookupType x ((x' := a) : env) = LookupType x env

-- ||| Valid specification of observable variables
class ValidSpec (env :: [Assign Symbol *]) (xs :: [Symbol])  where
  asTags :: ObsVars xs -> [Tag]

instance ValidSpec env '[] where
  asTags _    = []

instance (FindElem x (Keys env), KnownSymbol x, ValidSpec env xs) => ValidSpec env (x : xs)  where
  asTags (OCons xs)  = varToStr (ObsVar @x) : asTags @env @xs xs

varToStr :: forall x. ObsVar x -> String
varToStr ObsVar = symbolVal (Proxy @x)

-- ||| (Section 5.1) Model environments
data Env (env :: [Assign Symbol *]) where
  ENil  :: Env '[]
  ECons :: forall a x env. [a] -> Env env -> Env (x := a : env)

enil :: Env '[]
enil = ENil

infixr 5 <:>
(<:>) :: UniqueKey x env ~ 'True => Assign (ObsVar x) [a] -> Env env -> Env ((x ':= a) ': env)
(_ := a) <:> env = ECons a env

-- ||| Constraining model environments
instance FindElem x ((x := a) : env) where
  findElem = P 0
instance {-# OVERLAPPABLE #-} FindElem x env => FindElem x ((x' := a) : env) where
  findElem = P $ 1 + unP (findElem :: P x env)

class (FindElem x env, LookupType x env ~ a)
  => Observable env x a where
  get  :: ObsVar x -> Env env -> [a]
  set  :: ObsVar x -> [a] -> Env env -> Env env

instance (FindElem x env, LookupType x env ~ a)
  => Observable env x a where
  get _ env =
    let idx = unP $ findElem @x @env
        f :: Int -> Env env' -> [a]
        f n (ECons a env) = if   n == 0
                            then unsafeCoerce a
                            else f (n - 1) env
    in  f idx env
  set _ a' env =
    let idx = unP $ findElem @x @env
        f :: Int -> Env env' -> Env env'
        f n (ECons a env) = if   n == 0
                            then ECons (unsafeCoerce a') env
                            else ECons a (f (n - 1) env)
    in  f idx env

type family Observables env (ks :: [Symbol]) a :: Constraint where
  Observables env (x ': xs) a = (Observable env x a, Observables env xs a)
  Observables env '[] a = ()

instance (KnownSymbol x, Show a, Show (Env env)) => Show (Env ((x := a) ': env)) where
  show (ECons a env) = varToStr (ObsVar @x) ++ ":=" ++ show a ++ ", " ++ show env
instance Show (Env '[]) where
  show ENil = "[]"
