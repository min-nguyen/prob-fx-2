{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts, FlexibleInstances, GADTs, PolyKinds, RankNTypes, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances,  OverloadedLabels #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module ModelEnv where

import Data.Kind
-- import Data.Functor.Identity
import Data.Proxy
import qualified Data.Vector as V
-- import Fcf
import GHC.Natural
import GHC.OverloadedLabels
import GHC.TypeLits
import qualified GHC.TypeLits as TL
import Unsafe.Coerce
import Control.Lens hiding ((:>))
import FindElem

data ObsVar (x :: Symbol) where
  ObsVar :: KnownSymbol x => ObsVar x

instance (KnownSymbol x, x ~ x') => IsLabel x (ObsVar x') where
  fromLabel = ObsVar

varToStr :: forall x. ObsVar x -> String
varToStr ObsVar = symbolVal (Proxy @x)

data Assign x a = x := a

data ModelEnv (env :: [Assign Symbol *]) where
  HNil  :: ModelEnv '[]
  HCons :: forall a x env. [a] -> ModelEnv env -> ModelEnv (x := a : env)

instance (KnownSymbol x, Show a, Show (ModelEnv env)) => Show (ModelEnv ((x := a) ': env)) where
  show (HCons a env) = varToStr (ObsVar @x) ++ ":=" ++ show a ++ ", " ++ show env
instance Show (ModelEnv '[]) where
  show HNil = "[]"

instance FindElem x ((x := a) : env) where
  findElem = P 0
instance {-# OVERLAPPABLE #-} FindElem x env => FindElem x ((x' := a) : env) where
  findElem = P $ 1 + unP (findElem :: P x env)

type family LookupType x env where
  LookupType x ((x := a) : env) = a
  LookupType x ((x' := a) : env) = LookupType x env

class (FindElem x env, LookupType x env ~ a) => Observable env x a where
  getIdx :: ObsVar x -> ModelEnv env -> Int
  getIdx _ (HCons  a env) = unP $ findElem @x @env
  getOP  :: ObsVar x -> ModelEnv env -> [a]
  setOP  :: ObsVar x -> [a] -> ModelEnv env -> ModelEnv env

instance (FindElem x env, LookupType x env ~ a) => Observable env x a where
  getIdx _ _ = unP $ findElem @x @env
  getOP _ env =
    let idx = unP $ findElem @x @env
        f :: Int -> ModelEnv env' -> [a]
        f n (HCons a env) = if   n == 0
                            then unsafeCoerce a
                            else f (n - 1) env
    in  f idx env
  setOP _ a' env =
    let idx = unP $ findElem @x @env
        f :: Int -> ModelEnv env' -> ModelEnv env'
        f n (HCons a env) = if   n == 0
                            then HCons (unsafeCoerce a') env
                            else HCons a (f (n - 1) env)
    in  f idx env

type family Observables env (ks :: [Symbol]) a :: Constraint where
  Observables env (x ': xs) a = (Observable env x a, Observables env xs a)
  Observables env '[] a = ()

-- | Determine whether a var 'x' exists in a list of assocs 'kvs'
type family UniqueKey x env where
  UniqueKey x ((x ':= a) : env) = False
  UniqueKey x ((x' ':= a) : env) = UniqueKey x env
  UniqueKey x '[] = True

nil :: ModelEnv '[]
nil = HNil

infixr 5 <:>
(<:>) :: UniqueKey x env ~ 'True => Assign (ObsVar x) [a] -> ModelEnv env -> ModelEnv ((x ':= a) ': env)
(_ := a) <:> env = HCons a env

infixr 5 ∙
(∙) :: UniqueKey x env ~ 'True => Assign (ObsVar x) [a] -> ModelEnv env -> ModelEnv ((x ':= a) ': env)
(_ := a) ∙ env = HCons a env

mkLens :: forall env x a. Observable env x a => ObsVar x -> Lens' (ModelEnv env) [a]
mkLens x = lens (getOP x) (\s a -> setOP x a s)

mkGetter ::  (s -> a) -> Getting a s a
mkGetter x = dimap x (contramap x)

mkSetter :: ((a -> b) -> s -> t) -> ASetter s t a b
mkSetter f g = Identity . f (runIdentity . g)

mkGetterSetter :: forall env x a . (Observable env x a)
  => ObsVar x -> (Getting [a] (ModelEnv env) [a], ASetter (ModelEnv env) (ModelEnv env) [a] [a])
mkGetterSetter field =
  let getter' = mkGetter (\s -> getOP field s)
      setter' = mkSetter (\f s ->  let a = s ^. getter'
                                       a' = f a
                                   in  setOP field a' s)
  in (getter', setter')

-- f = #hi := 5 <: #bye := 5

-- data Any where
--   Any :: t -> Any

-- data ModelEnv (env :: [(Symbol, x)])  where
--   ModelEnv :: V.Vector Any -> ModelEnv env

-- nil :: ModelEnv '[]
-- nil = ModelEnv V.empty

-- data ObsVar (key :: Symbol) = ObsVar

-- newtype P t rs = P {unP :: Int}

-- data Assoc x a = x := a

-- class FindElem x r where
--   findElem :: P x r

-- instance {-# INCOHERENT  #-} FindElem x ((x ':= a) ': kvs) where
--   findElem = P 0
-- instance {-# OVERLAPPABLE #-} FindElem x kvs => FindElem x (kv ': kvs) where
--   findElem = P $ 1 + unP (findElem :: P x kvs)

-- getOP :: forall key env a. FindElem key env => ObsVar key -> ModelEnv env -> a
-- getOP _ (ModelEnv a) =
--   unAny (V.unsafeIndex a (unP $ findElem @key @env))
--   where
--     unAny (Any a) = unsafeCoerce a

-- type family LookupType x kvs where
--   LookupType x ((x ':= a) : kvs) = a


-- class (KnownNat (FindElem x xs), a ~ Eval (LookupType x xs))
--   => Lookup xs x a  where

-- type UniqueKey (key :: x) (env :: [(x, t)])
--  = Filter (TyEq key <=< Fst) env >>= Null

-- insert :: Eval (UniqueKey key env) ~ 'True
--        => ObsVar key -> t -> ModelEnv env -> ModelEnv ('(key, t) ': env)
-- insert _ t (ModelEnv a) = ModelEnv (V.cons (Any t) a)

-- infixr 5 <:>
-- (<:>) :: Eval (UniqueKey key env) ~ 'True => (ObsVar key, t) -> ModelEnv env -> ModelEnv ('(key, t) ': env)
-- (<:>) (x, a) op = insert x a op

-- -- # Projection
-- data FindIndex :: (a -> Exp Bool) -> [a] -> Exp (Maybe Nat)
-- type instance Eval (FindIndex f '[]) = 'Nothing
-- type instance Eval (FindIndex f (x ': xs))
--   = Eval (If (Eval (f x))
--              (Eval (Pure ('Just 0)))
--              (Eval (FindIndex f xs >>= FMap ((+!) 1))))
-- type FindElem (key :: Symbol) (env :: [(Symbol, x)]) =
--   Eval (FindIndex (TyEq key <=< Fst) env >>= FromMaybe Stuck)

-- -- The SNat type allows us to take a type-level Nat and produce a Natural number from it, where Natural can be converted to an integer.
-- newtype SNat (n :: Nat) = SNat Natural

-- findElem :: forall key env. KnownNat (FindElem key env) => Int
-- findElem = fromIntegral $ natVal (Proxy @(FindElem key env))

-- data LookUp :: x -> [(x, a)] -> Exp (Maybe a)

-- type instance Eval (LookUp x '[]) = Nothing
-- type instance Eval (LookUp x ( '(x', a) : ks  )) =
--   Eval (If (Eval (TyEq x x'))
--            (Just a)
--            (Eval (LookUp x ks)))

-- type LookupType (key :: x) (env :: [(x , t)]) =
--   LookUp key env >>= FromMaybe Stuck

-- getOP :: forall key env  . KnownNat (FindElem key env)
--     => ObsVar key -> ModelEnv env -> (Eval (LookupType key env))
-- getOP _ (ModelEnv a) =
--   unAny (V.unsafeIndex a (findElem @key @env))
--   where
--     unAny (Any a) = unsafeCoerce a

-- setOP :: forall key env . KnownNat (FindElem key env)
--     => ObsVar key -> Eval (LookupType key env) -> ModelEnv  env -> ModelEnv env
-- setOP _ ft (ModelEnv a) =
--   ModelEnv (a V.// [(findElem @key @env, Any ft)])

-- instance (key ~ key') => IsLabel key (ObsVar key') where
--   fromLabel = ObsVar

-- class (KnownNat (FindElem x xs), a ~ Eval (LookupType x xs))
--   => Lookup xs x a  where

-- getLens :: ModelEnv xs -> Getting a (ModelEnv  xs) a -> a
-- getLens record x = record ^. x

-- keyGetter :: (KnownNat (FindElem x xs), a ~ Eval (FromMaybe Stuck (Eval (LookUp x xs))))
--   => ObsVar x -> ModelEnv xs -> a
-- keyGetter x = (\s -> getOP x s)

-- makeGetter :: forall a xs x. (KnownNat (FindElem x xs), a ~ Eval (FromMaybe Stuck (Eval (LookUp x xs)))) => ObsVar x -> Getting a (ModelEnv  xs) a
-- makeGetter x' = to (keyGetter x')

-- to' ::  (s -> a) -> Getting a s a
-- to' x = dimap x (contramap x)

-- sets' :: ((a -> b) -> s -> t) -> ASetter s t a b
-- sets' f g = Identity . f (runIdentity . g)

-- lens' :: (ModelEnv s -> a) -> (ModelEnv s -> a -> ModelEnv s) -> Lens' (ModelEnv s) a
-- lens' sa sbt afb s = sbt s <$> afb (sa s)

-- keyLens :: Lookup s x a => ObsVar x -> Lens' (ModelEnv s) a
-- keyLens x = lens (\s -> getOP x s) (\s b -> setOP x b s)

-- mkGetterSetter :: (KnownNat (FindElem x xs), [a] ~ Eval (FromMaybe Stuck (Eval (LookUp x xs)))) => ObsVar x -> (Getting [a] (ModelEnv  xs) [a], ASetter (ModelEnv  xs) (ModelEnv xs) [a] [a])
-- mkGetterSetter field =
--   let getter' = to' (\s -> getOP field s)
--       setter' = sets' (\f s ->  let a = s ^. getter'
--                                     a' = f a
--                                 in  setOP field a' s)
--   in (getter', setter')


-- keyTest :: IsLabel "x" (ObsVar a) => ObsVar a
-- keyTest = #x

-- ex :: ModelEnv  '[ '("bye", Int) , '("hi", Int)]
-- ex = (#bye, 5) <:> (#hi, 5) <:> nil

-- getEx ::  Int
-- getEx = getOP #hi ex

-- getField :: (Lookup xs "key" a) => ModelEnv  xs ->  a
-- getField record = getOP #key record

-- bye :: KnownNat (FindElem "bye" xs) =>
--   Getting (Eval (LookupType "bye" xs)) (ModelEnv  xs) (Eval (LookupType "bye" xs))
-- bye = makeGetter #bye

-- getEx' :: Int
-- getEx' = ex ^. bye
