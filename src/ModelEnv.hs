{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts, FlexibleInstances, GADTs, PolyKinds, RankNTypes, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances,  OverloadedLabels #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module ModelEnv where

import Data.Kind
-- import Data.Functor.Identity
import qualified Data.Vector as V
-- import Fcf
import GHC.OverloadedLabels
import GHC.TypeLits
import qualified GHC.TypeLits as TL
import Unsafe.Coerce
import FindElem
import Effects.Dist ( Tag )
import Util

{- Observable variable container -}
data ObsVar (x :: Symbol) where
  ObsVar :: KnownSymbol x => ObsVar x

instance (KnownSymbol x, x ~ x') => IsLabel x (ObsVar x') where
  fromLabel = ObsVar

varToStr :: forall x. ObsVar x -> String
varToStr ObsVar = symbolVal (Proxy @x)

{- Specification of observable variables -}
data ObsVars (xs :: [Symbol]) where
  ONil  :: ObsVars '[]
  OCons :: ObsVars xs -> ObsVars (x : xs)

onil :: ObsVars '[]
onil = ONil

infixr 5 ⋮
(⋮) :: ObsVar x -> ObsVars xs -> ObsVars (x : xs)
x ⋮ xs = OCons xs

{- Observable variable assignment -}
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

{- Valid specification of observable variables-}
class ValidSpec (env :: [Assign Symbol *]) (xs :: [Symbol])  where
  asTags :: ObsVars xs ->  [Tag]

instance ValidSpec env '[] where
  asTags _    = []

instance (FindElem x (Keys env), KnownSymbol x, ValidSpec env xs) => ValidSpec env (x : xs)  where
  asTags (OCons xs)  = varToStr (ObsVar @x) : asTags @env @xs xs

{- Model environment -}
data ModelEnv (env :: [Assign Symbol *]) where
  ENil  :: ModelEnv '[]
  ECons :: forall a x env. [a] -> ModelEnv env -> ModelEnv (x := a : env)

instance (KnownSymbol x, Show a, Show (ModelEnv env)) => Show (ModelEnv ((x := a) ': env)) where
  show (ECons a env) = varToStr (ObsVar @x) ++ ":=" ++ show a ++ ", " ++ show env
instance Show (ModelEnv '[]) where
  show ENil = "[]"

instance FindElem x ((x := a) : env) where
  findElem = P 0
instance {-# OVERLAPPABLE #-} FindElem x env => FindElem x ((x' := a) : env) where
  findElem = P $ 1 + unP (findElem :: P x env)

eNil :: ModelEnv '[]
eNil = ENil

infixr 5 <:>
(<:>) :: UniqueKey x env ~ 'True => Assign (ObsVar x) [a] -> ModelEnv env -> ModelEnv ((x ':= a) ': env)
(_ := a) <:> env = ECons a env

infixr 5 ∙
(∙) :: UniqueKey x env ~ 'True => Assign (ObsVar x) [a] -> ModelEnv env -> ModelEnv ((x ':= a) ': env)
(_ := a) ∙ env = ECons a env

{- Observable class -}
class (FindElem x env, LookupType x env ~ a) => Observable env x a where
  getIdx :: ObsVar x -> ModelEnv env -> Int
  getIdx _ (ECons  a env) = unP $ findElem @x @env
  getO  :: ObsVar x -> ModelEnv env -> [a]
  setO  :: ObsVar x -> [a] -> ModelEnv env -> ModelEnv env

instance (FindElem x env, LookupType x env ~ a) => Observable env x a where
  getIdx _ _ = unP $ findElem @x @env
  getO _ env =
    let idx = unP $ findElem @x @env
        f :: Int -> ModelEnv env' -> [a]
        f n (ECons a env) = if   n == 0
                            then unsafeCoerce a
                            else f (n - 1) env
    in  f idx env
  setO _ a' env =
    let idx = unP $ findElem @x @env
        f :: Int -> ModelEnv env' -> ModelEnv env'
        f n (ECons a env) = if   n == 0
                            then ECons (unsafeCoerce a') env
                            else ECons a (f (n - 1) env)
    in  f idx env

type family Observables env (ks :: [Symbol]) a :: Constraint where
  Observables env (x ': xs) a = (Observable env x a, Observables env xs a)
  Observables env '[] a = ()

