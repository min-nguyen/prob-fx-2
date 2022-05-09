{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts, FlexibleInstances, GADTs, PolyKinds, RankNTypes, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances,  OverloadedLabels #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Old.OpenProductVec where

-- import Data.Functor.Identity
import Data.Proxy
import qualified Data.Vector as V
import GHC.Natural
import GHC.OverloadedLabels
import GHC.TypeLits
import qualified GHC.TypeLits as TL
import Unsafe.Coerce
import Control.Lens hiding ((:>))

data Any where
  Any :: t -> Any

data Env (ts :: [Assoc Symbol k])  where
  Env :: V.Vector Any -> Env ts

nil :: Env '[]
nil = Env V.empty

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
  getO :: ObsVar x -> Env env -> a
  setO :: ObsVar x -> a -> Env env -> Env env

instance (FindElem x env, LookupType x env ~ a) => Lookup env x a where
  getO _ (Env a) =
    unAny (V.unsafeIndex a (unP $ findElem @x @env))
    where
      unAny (Any a) = unsafeCoerce a
  setO _ ft (Env a) =
    Env (a V.// [(unP (findElem @x @env), Any ft)])

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

type LRec s = Env (AsList s)

insert :: UniqueKey x ts ~ 'True
       => ObsVar x -> t -> Env ts -> Env (x ':> t ': ts)
insert _ t (Env a) = Env (V.cons (Any t) a)

infixr 5 <:>
(<:>) :: UniqueKey x env ~ 'True => Assoc (ObsVar x) a -> Env env -> Env ((x ':> a) ': env)
(_ :> a) <:> (Env env) = Env (V.cons (Any a) env)

mkLens :: forall env x a. Lookup env x a => ObsVar x -> Lens' (Env env) a
mkLens x = lens (getO x) (\s a -> setO x a s)

mkGetter ::  (s -> a) -> Getting a s a
mkGetter x = dimap x (contramap x)

mkSetter :: ((a -> b) -> s -> t) -> ASetter s t a b
mkSetter f g = Identity . f (runIdentity . g)

mkGetterSetter :: forall env x a . (Lookup env x a)
  => ObsVar x -> (Getting a (Env env) a, ASetter (Env env) (Env env) a a)
mkGetterSetter field =
  let getter' = mkGetter (\s -> getO field s)
      setter' = mkSetter (\f s ->  let a = s ^. getter'
                                       a' = f a
                                   in  setO field a' s)
  in (getter', setter')

-- f = #hi :> 5 <: #bye :> 5

-- data Any where
--   Any :: t -> Any

-- data Env (ts :: [(Symbol, k)])  where
--   Env :: V.Vector Any -> Env ts

-- nil :: Env '[]
-- nil = Env V.empty

-- data ObsVar (key :: Symbol) = ObsVar

-- newtype P t rs = P {unP :: Int}

-- data Assoc k a = k :> a

-- class FindElem k r where
--   findElem :: P k r

-- instance {-# INCOHERENT  #-} FindElem k ((k ':> a) ': kvs) where
--   findElem = P 0
-- instance {-# OVERLAPPABLE #-} FindElem k kvs => FindElem k (kv ': kvs) where
--   findElem = P $ 1 + unP (findElem :: P k kvs)

-- getO :: forall key ts a. FindElem key ts => ObsVar key -> Env ts -> a
-- getO _ (Env a) =
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
--        => ObsVar key -> t -> Env ts -> Env ('(key, t) ': ts)
-- insert _ t (Env a) = Env (V.cons (Any t) a)

-- infixr 5 <:>
-- (<:>) :: Eval (UniqueKey key ts) ~ 'True => (ObsVar key, t) -> Env ts -> Env ('(key, t) ': ts)
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

-- getO :: forall key ts  . KnownNat (FindElem key ts)
--     => ObsVar key -> Env ts -> (Eval (LookupType key ts))
-- getO _ (Env a) =
--   unAny (V.unsafeIndex a (findElem @key @ts))
--   where
--     unAny (Any a) = unsafeCoerce a

-- setO :: forall key ts . KnownNat (FindElem key ts)
--     => ObsVar key -> Eval (LookupType key ts) -> Env  ts -> Env ts
-- setO _ ft (Env a) =
--   Env (a V.// [(findElem @key @ts, Any ft)])

-- instance (key ~ key') => IsLabel key (ObsVar key') where
--   fromLabel = ObsVar

-- class (KnownNat (FindElem k xs), a ~ Eval (LookupType k xs))
--   => Lookup xs k a  where

-- getLens :: Env xs -> Getting a (Env  xs) a -> a
-- getLens record k = record ^. k

-- keyGetter :: (KnownNat (FindElem k xs), a ~ Eval (FromMaybe Stuck (Eval (LookUp k xs))))
--   => ObsVar k -> Env xs -> a
-- keyGetter k = (\s -> getO k s)

-- makeGetter :: forall a xs k. (KnownNat (FindElem k xs), a ~ Eval (FromMaybe Stuck (Eval (LookUp k xs)))) => ObsVar k -> Getting a (Env  xs) a
-- makeGetter k' = to (keyGetter k')

-- to' ::  (s -> a) -> Getting a s a
-- to' k = dimap k (contramap k)

-- sets' :: ((a -> b) -> s -> t) -> ASetter s t a b
-- sets' f g = Identity . f (runIdentity . g)

-- lens' :: (Env s -> a) -> (Env s -> a -> Env s) -> Lens' (Env s) a
-- lens' sa sbt afb s = sbt s <$> afb (sa s)

-- keyLens :: Lookup s k a => ObsVar k -> Lens' (Env s) a
-- keyLens k = lens (\s -> getO k s) (\s b -> setO k b s)

-- mkGetterSetter :: (KnownNat (FindElem k xs), [a] ~ Eval (FromMaybe Stuck (Eval (LookUp k xs)))) => ObsVar k -> (Getting [a] (Env  xs) [a], ASetter (Env  xs) (Env xs) [a] [a])
-- mkGetterSetter field =
--   let getter' = to' (\s -> getO field s)
--       setter' = sets' (\f s ->  let a = s ^. getter'
--                                     a' = f a
--                                 in  setO field a' s)
--   in (getter', setter')


-- keyTest :: IsLabel "k" (ObsVar a) => ObsVar a
-- keyTest = #k

-- ex :: Env  '[ '("bye", Int) , '("hi", Int)]
-- ex = (#bye, 5) <:> (#hi, 5) <:> nil

-- getEx ::  Int
-- getEx = getO #hi ex

-- getField :: (Lookup xs "key" a) => Env  xs ->  a
-- getField record = getO #key record

-- bye :: KnownNat (FindElem "bye" xs) =>
--   Getting (Eval (LookupType "bye" xs)) (Env  xs) (Eval (LookupType "bye" xs))
-- bye = makeGetter #bye

-- getEx' :: Int
-- getEx' = ex ^. bye
