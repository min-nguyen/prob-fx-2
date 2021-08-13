{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts, FlexibleInstances, GADTs, PolyKinds, RankNTypes, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances,  OverloadedLabels #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Extensible.OpenProduct where

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

data OpenProduct (ts :: [Assoc Symbol k])  where
  OpenProduct :: V.Vector Any -> OpenProduct ts

nil :: OpenProduct '[]
nil = OpenProduct V.empty

-- data Var (key :: Symbol) = Var
data Var (x :: Symbol) where
  Var :: KnownSymbol x => Var x

instance (KnownSymbol x, x ~ x') => IsLabel x (Var x') where
  fromLabel = Var

varToStr :: forall x. Var x -> String
varToStr Var = symbolVal (Proxy @x)

newtype P t rs = P {unP :: Int}

data Assoc x v = x :> v

(@=) :: x -> v -> Assoc x v
(@=) = (:>)

-- | Find index of var 'x' in list of assocs 'kvs'
class FindElem x xvs where
  findElem :: P x xvs
instance {-# INCOHERENT  #-} FindElem x ((x ':> v) ': xvs) where
  findElem = P 0
instance {-# OVERLAPPABLE #-} FindElem x xvs => FindElem x (xv ': xvs) where
  findElem = P $ 1 + unP (findElem :: P x xvs)

-- | Look up type associated with var 'x' in list of assocs 'kvs'
type family LookupType x xvs where
  LookupType x ((x ':> v) : kvs) = v
  LookupType x ((x' ':> v) : kvs) = LookupType x kvs

-- | Determine whether a var 'x' exists in a list of assocs 'kvs'
type family UniqueKey x xvs where
  UniqueKey x ((x ':> v) : xvs) = False
  UniqueKey x ((x' ':> v) : xvs) = UniqueKey x xvs
  UniqueKey x '[] = True

-- | State that we can lookup var 'x' with value type 'a' in list of assocs 'xs'
class (FindElem x xvs, LookupType x xvs ~ a) => Lookup xvs x a

instance (FindElem x xvs, LookupType x xvs ~ a) => Lookup xvs x a

-- | Map the list constructor over a list of assocs
type family AsList (as :: [k]) = (bs :: [k]) | bs -> as where
  AsList ((x :> v) : xvs)   = ((x :> [v]) : AsList xvs)
  AsList '[] = '[]

-- | "Observable xs x v" is shorthand for "Lookup (AsList xs) x [v]""
class Lookup (AsList xvs) x [v]  => Observable xvs x v where

instance Lookup (AsList xvs) x [v] => Observable xvs x v where

type family Observables xvs ks v where
  Observables xvs (x ': xs) v = (Observable xvs x v, Observables xvs xs v)
  Observables xvs '[] v = ()

type LRec s = OpenProduct (AsList s)

insert :: UniqueKey x ts ~ 'True
       => Var x -> t -> OpenProduct ts -> OpenProduct (x ':> t ': ts)
insert _ t (OpenProduct v) = OpenProduct (V.cons (Any t) v)

infixr 5 <:
(<:) :: UniqueKey x xvs ~ 'True => Assoc (Var x) v -> OpenProduct xvs -> OpenProduct ((x ':> v) ': xvs)
(<:) (x :> v) op = insert x v op

getOP :: forall x ts a. FindElem x ts => Var x -> OpenProduct ts -> a
getOP _ (OpenProduct v) =
  unAny (V.unsafeIndex v (unP $ findElem @x @ts))
  where
    unAny (Any a) = unsafeCoerce a

setOP :: forall x ts . FindElem x ts
    => Var x -> LookupType x ts -> OpenProduct ts -> OpenProduct ts
setOP _ ft (OpenProduct v) =
  OpenProduct (v V.// [(unP (findElem @x @ts), Any ft)])

mkLens :: forall xvs x a. Lookup xvs x a => Var x -> Lens' (OpenProduct xvs) a
mkLens x = lens (getOP x) (\s a -> setOP x a s)

mkGetter ::  (s -> a) -> Getting a s a
mkGetter x = dimap x (contramap x)

mkSetter :: ((a -> b) -> s -> t) -> ASetter s t a b
mkSetter f g = Identity . f (runIdentity . g)

mkGetterSetter :: (Lookup xvs x v)
  => Var x -> (Getting v (OpenProduct xvs) v, ASetter (OpenProduct xvs) (OpenProduct xvs) v v)
mkGetterSetter field =
  let getter' = mkGetter (\s -> getOP field s)
      setter' = mkSetter (\f s ->  let a = s ^. getter'
                                       a' = f a
                                   in  setOP field a' s)
  in (getter', setter')

-- f = #hi :> 5 <: #bye :> 5

-- data Any where
--   Any :: t -> Any

-- data OpenProduct (ts :: [(Symbol, k)])  where
--   OpenProduct :: V.Vector Any -> OpenProduct ts

-- nil :: OpenProduct '[]
-- nil = OpenProduct V.empty

-- data Var (key :: Symbol) = Var

-- newtype P t rs = P {unP :: Int}

-- data Assoc k v = k :> v

-- class FindElem k r where
--   findElem :: P k r

-- instance {-# INCOHERENT  #-} FindElem k ((k ':> v) ': kvs) where
--   findElem = P 0
-- instance {-# OVERLAPPABLE #-} FindElem k kvs => FindElem k (kv ': kvs) where
--   findElem = P $ 1 + unP (findElem :: P k kvs)

-- getOP :: forall key ts a. FindElem key ts => Var key -> OpenProduct ts -> a
-- getOP _ (OpenProduct v) =
--   unAny (V.unsafeIndex v (unP $ findElem @key @ts))
--   where
--     unAny (Any a) = unsafeCoerce a

-- type family LookupType k kvs where
--   LookupType k ((k ':> v) : kvs) = v


-- class (KnownNat (FindElem k xs), a ~ Eval (LookupType k xs))
--   => Lookup xs k a  where

-- type UniqueKey (key :: k) (ts :: [(k, t)])
--  = Filter (TyEq key <=< Fst) ts >>= Null

-- insert :: Eval (UniqueKey key ts) ~ 'True
--        => Var key -> t -> OpenProduct ts -> OpenProduct ('(key, t) ': ts)
-- insert _ t (OpenProduct v) = OpenProduct (V.cons (Any t) v)

-- infixr 5 <:>
-- (<:>) :: Eval (UniqueKey key ts) ~ 'True => (Var key, t) -> OpenProduct ts -> OpenProduct ('(key, t) ': ts)
-- (<:>) (k, v) op = insert k v op

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
--     => Var key -> OpenProduct ts -> (Eval (LookupType key ts))
-- getOP _ (OpenProduct v) =
--   unAny (V.unsafeIndex v (findElem @key @ts))
--   where
--     unAny (Any a) = unsafeCoerce a

-- setOP :: forall key ts . KnownNat (FindElem key ts)
--     => Var key -> Eval (LookupType key ts) -> OpenProduct  ts -> OpenProduct ts
-- setOP _ ft (OpenProduct v) =
--   OpenProduct (v V.// [(findElem @key @ts, Any ft)])

-- instance (key ~ key') => IsLabel key (Var key') where
--   fromLabel = Var

-- class (KnownNat (FindElem k xs), a ~ Eval (LookupType k xs))
--   => Lookup xs k a  where

-- getLens :: OpenProduct xs -> Getting a (OpenProduct  xs) a -> a
-- getLens record k = record ^. k

-- keyGetter :: (KnownNat (FindElem k xs), a ~ Eval (FromMaybe Stuck (Eval (LookUp k xs))))
--   => Var k -> OpenProduct xs -> a
-- keyGetter k = (\s -> getOP k s)

-- makeGetter :: forall a xs k. (KnownNat (FindElem k xs), a ~ Eval (FromMaybe Stuck (Eval (LookUp k xs)))) => Var k -> Getting a (OpenProduct  xs) a
-- makeGetter k' = to (keyGetter k')

-- to' ::  (s -> a) -> Getting a s a
-- to' k = dimap k (contramap k)

-- sets' :: ((a -> b) -> s -> t) -> ASetter s t a b
-- sets' f g = Identity . f (runIdentity . g)

-- lens' :: (OpenProduct s -> a) -> (OpenProduct s -> a -> OpenProduct s) -> Lens' (OpenProduct s) a
-- lens' sa sbt afb s = sbt s <$> afb (sa s)

-- keyLens :: Lookup s k a => Var k -> Lens' (OpenProduct s) a
-- keyLens k = lens (\s -> getOP k s) (\s b -> setOP k b s)

-- mkGetterSetter :: (KnownNat (FindElem k xs), [a] ~ Eval (FromMaybe Stuck (Eval (LookUp k xs)))) => Var k -> (Getting [a] (OpenProduct  xs) [a], ASetter (OpenProduct  xs) (OpenProduct xs) [a] [a])
-- mkGetterSetter field =
--   let getter' = to' (\s -> getOP field s)
--       setter' = sets' (\f s ->  let a = s ^. getter'
--                                     a' = f a
--                                 in  setOP field a' s)
--   in (getter', setter')


-- keyTest :: IsLabel "k" (Var a) => Var a
-- keyTest = #k

-- ex :: OpenProduct  '[ '("bye", Int) , '("hi", Int)]
-- ex = (#bye, 5) <:> (#hi, 5) <:> nil

-- getEx ::  Int
-- getEx = getOP #hi ex

-- getField :: (Lookup xs "key" a) => OpenProduct  xs ->  a
-- getField record = getOP #key record

-- bye :: KnownNat (FindElem "bye" xs) =>
--   Getting (Eval (LookupType "bye" xs)) (OpenProduct  xs) (Eval (LookupType "bye" xs))
-- bye = makeGetter #bye

-- getEx' :: Int
-- getEx' = ex ^. bye
