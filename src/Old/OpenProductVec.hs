{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts, FlexibleInstances, GADTs, PolyKinds, RankNTypes, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances,  OverloadedLabels #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Old.OpenProductVec where

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
-- import Data.Extensible

data Any where
  Any :: t -> Any

data ModelEnv (ts :: [Assoc Symbol k])  where
  ModelEnv :: V.Vector Any -> ModelEnv ts

nil :: ModelEnv '[]
nil = ModelEnv V.empty

-- data ObsVar (key :: Symbol) = ObsVar
data ObsVar (x :: Symbol) where
  ObsVar :: KnownSymbol x => ObsVar x

instance (KnownSymbol x, x ~ x') => IsLabel x (ObsVar x') where
  fromLabel = ObsVar

varToStr :: forall x. ObsVar x -> String
varToStr ObsVar = symbolVal (Proxy @x)

newtype P t rs = P {unP :: Int}

data Assoc x a = x :> a

(@=) :: x -> a -> Assoc x a
(@=) = (:>)

-- | Find index of var 'x' in list of assocs 'kvs'
class FindElem x env where
  findElem :: P x env
instance FindElem x ((x ':> a) ': env) where
  findElem = P 0
instance {-# OVERLAPPABLE #-} FindElem x env => FindElem x (xv ': env) where
  findElem = P $ 1 + unP (findElem :: P x env)

-- | Look up type associated with var 'x' in list of assocs 'kvs'
type family LookupType x env where
  LookupType x ((x ':> a) : kvs) = a
  LookupType x ((x' ':> a) : kvs) = LookupType x kvs

-- | Determine whether a var 'x' exists in a list of assocs 'kvs'
type family UniqueKey x env where
  UniqueKey x ((x ':> a) : env) = False
  UniqueKey x ((x' ':> a) : env) = UniqueKey x env
  UniqueKey x '[] = True

-- | State that we can lookup var 'x' with value type 'a' in list of assocs 'xs'
class (FindElem x env, LookupType x env ~ a) => Lookup env x a where
  getOP :: ObsVar x -> ModelEnv env -> a
  setOP :: ObsVar x -> a -> ModelEnv env -> ModelEnv env

instance (FindElem x env, LookupType x env ~ a) => Lookup env x a where
  getOP _ (ModelEnv a) =
    unAny (V.unsafeIndex a (unP $ findElem @x @env))
    where
      unAny (Any a) = unsafeCoerce a
  setOP _ ft (ModelEnv a) =
    ModelEnv (a V.// [(unP (findElem @x @env), Any ft)])

-- | Map the list constructor over a list of assocs
type family AsList (as :: [k]) = (bs :: [k]) | bs -> as where
  AsList ((x :> a) : env)   = ((x :> [a]) : AsList env)
  AsList '[] = '[]

-- | "Observable xs x a" is shorthand for "Lookup (AsList xs) x [a]""
class Lookup (AsList env) x [a]  => Observable env x a where

instance Lookup (AsList env) x [a] => Observable env x a where

type family Observables env ks a where
  Observables env (x ': xs) a = (Observable env x a, Observables env xs a)
  Observables env '[] a = ()

type LRec s = ModelEnv (AsList s)

insert :: UniqueKey x ts ~ 'True
       => ObsVar x -> t -> ModelEnv ts -> ModelEnv (x ':> t ': ts)
insert _ t (ModelEnv a) = ModelEnv (V.cons (Any t) a)

infixr 5 <:>
(<:>) :: UniqueKey x env ~ 'True => Assoc (ObsVar x) a -> ModelEnv env -> ModelEnv ((x ':> a) ': env)
(_ :> a) <:> (ModelEnv env) = ModelEnv (V.cons (Any a) env)

mkLens :: forall env x a. Lookup env x a => ObsVar x -> Lens' (ModelEnv env) a
mkLens x = lens (getOP x) (\s a -> setOP x a s)

mkGetter ::  (s -> a) -> Getting a s a
mkGetter x = dimap x (contramap x)

mkSetter :: ((a -> b) -> s -> t) -> ASetter s t a b
mkSetter f g = Identity . f (runIdentity . g)

mkGetterSetter :: forall env x a . (Lookup env x a)
  => ObsVar x -> (Getting a (ModelEnv env) a, ASetter (ModelEnv env) (ModelEnv env) a a)
mkGetterSetter field =
  let getter' = mkGetter (\s -> getOP field s)
      setter' = mkSetter (\f s ->  let a = s ^. getter'
                                       a' = f a
                                   in  setOP field a' s)
  in (getter', setter')

-- f = #hi :> 5 <: #bye :> 5

-- data Any where
--   Any :: t -> Any

-- data ModelEnv (ts :: [(Symbol, k)])  where
--   ModelEnv :: V.Vector Any -> ModelEnv ts

-- nil :: ModelEnv '[]
-- nil = ModelEnv V.empty

-- data ObsVar (key :: Symbol) = ObsVar

-- newtype P t rs = P {unP :: Int}

-- data Assoc k a = k :> a

-- class FindElem k r where
--   findElem :: P k r

-- instance {-# INCOHERENT  #-} FindElem k ((k ':> a) ': kvs) where
--   findElem = P 0
-- instance {-# OVERLAPPABLE #-} FindElem k kvs => FindElem k (kv ': kvs) where
--   findElem = P $ 1 + unP (findElem :: P k kvs)

-- getOP :: forall key ts a. FindElem key ts => ObsVar key -> ModelEnv ts -> a
-- getOP _ (ModelEnv a) =
--   unAny (V.unsafeIndex a (unP $ findElem @key @ts))
--   where
--     unAny (Any a) = unsafeCoerce a

-- type family LookupType k kvs where
--   LookupType k ((k ':> a) : kvs) = a


-- class (KnownNat (FindElem k xs), a ~ Eval (LookupType k xs))
--   => Lookup xs k a  where

-- type UniqueKey (key :: k) (ts :: [(k, t)])
--  = Filter (TyEq key <=< Fst) ts >>= Null

-- insert :: Eval (UniqueKey key ts) ~ 'True
--        => ObsVar key -> t -> ModelEnv ts -> ModelEnv ('(key, t) ': ts)
-- insert _ t (ModelEnv a) = ModelEnv (V.cons (Any t) a)

-- infixr 5 <:>
-- (<:>) :: Eval (UniqueKey key ts) ~ 'True => (ObsVar key, t) -> ModelEnv ts -> ModelEnv ('(key, t) ': ts)
-- (<:>) (k, a) op = insert k a op

-- -- # Projection
-- data FindIndex :: (a -> Exp Bool) -> [a] -> Exp (Maybe Nat)
-- type instance Eval (FindIndex f '[]) = 'Nothing
-- type instance Eval (FindIndex f (x ': xs))
--   = Eval (If (Eval (f x))
--              (Eval (Pure ('Just 0)))
--              (Eval (FindIndex f xs >>= FMap ((+!) 1))))
-- type FindElem (key :: Symbol) (ts :: [(Symbol, k)]) =
--   Eval (FindIndex (TyEq key <=< Fst) ts >>= FromMaybe Stuck)

-- -- The SNat type allows us to take a type-level Nat and produce a Natural number from it, where Natural can be converted to an integer.
-- newtype SNat (n :: Nat) = SNat Natural

-- findElem :: forall key ts. KnownNat (FindElem key ts) => Int
-- findElem = fromIntegral $ natVal (Proxy @(FindElem key ts))

-- data LookUp :: k -> [(k, a)] -> Exp (Maybe a)

-- type instance Eval (LookUp k '[]) = Nothing
-- type instance Eval (LookUp k ( '(k', a) : ks  )) =
--   Eval (If (Eval (TyEq k k'))
--            (Just a)
--            (Eval (LookUp k ks)))

-- type LookupType (key :: k) (ts :: [(k , t)]) =
--   LookUp key ts >>= FromMaybe Stuck

-- getOP :: forall key ts  . KnownNat (FindElem key ts)
--     => ObsVar key -> ModelEnv ts -> (Eval (LookupType key ts))
-- getOP _ (ModelEnv a) =
--   unAny (V.unsafeIndex a (findElem @key @ts))
--   where
--     unAny (Any a) = unsafeCoerce a

-- setOP :: forall key ts . KnownNat (FindElem key ts)
--     => ObsVar key -> Eval (LookupType key ts) -> ModelEnv  ts -> ModelEnv ts
-- setOP _ ft (ModelEnv a) =
--   ModelEnv (a V.// [(findElem @key @ts, Any ft)])

-- instance (key ~ key') => IsLabel key (ObsVar key') where
--   fromLabel = ObsVar

-- class (KnownNat (FindElem k xs), a ~ Eval (LookupType k xs))
--   => Lookup xs k a  where

-- getLens :: ModelEnv xs -> Getting a (ModelEnv  xs) a -> a
-- getLens record k = record ^. k

-- keyGetter :: (KnownNat (FindElem k xs), a ~ Eval (FromMaybe Stuck (Eval (LookUp k xs))))
--   => ObsVar k -> ModelEnv xs -> a
-- keyGetter k = (\s -> getOP k s)

-- makeGetter :: forall a xs k. (KnownNat (FindElem k xs), a ~ Eval (FromMaybe Stuck (Eval (LookUp k xs)))) => ObsVar k -> Getting a (ModelEnv  xs) a
-- makeGetter k' = to (keyGetter k')

-- to' ::  (s -> a) -> Getting a s a
-- to' k = dimap k (contramap k)

-- sets' :: ((a -> b) -> s -> t) -> ASetter s t a b
-- sets' f g = Identity . f (runIdentity . g)

-- lens' :: (ModelEnv s -> a) -> (ModelEnv s -> a -> ModelEnv s) -> Lens' (ModelEnv s) a
-- lens' sa sbt afb s = sbt s <$> afb (sa s)

-- keyLens :: Lookup s k a => ObsVar k -> Lens' (ModelEnv s) a
-- keyLens k = lens (\s -> getOP k s) (\s b -> setOP k b s)

-- mkGetterSetter :: (KnownNat (FindElem k xs), [a] ~ Eval (FromMaybe Stuck (Eval (LookUp k xs)))) => ObsVar k -> (Getting [a] (ModelEnv  xs) [a], ASetter (ModelEnv  xs) (ModelEnv xs) [a] [a])
-- mkGetterSetter field =
--   let getter' = to' (\s -> getOP field s)
--       setter' = sets' (\f s ->  let a = s ^. getter'
--                                     a' = f a
--                                 in  setOP field a' s)
--   in (getter', setter')


-- keyTest :: IsLabel "k" (ObsVar a) => ObsVar a
-- keyTest = #k

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
