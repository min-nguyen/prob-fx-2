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
import Control.Lens

data Any where
  Any :: t -> Any

data OpenProduct (ts :: [(Symbol, k)])  where
  OpenProduct :: V.Vector Any -> OpenProduct ts

nil :: OpenProduct '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) = Key

type Exp a = a -> Type
type family Eval (e :: Exp a) :: a

-- ## Exp as type-level monad
data Pure :: a -> Exp a
data (>>=) :: Exp a -> (a -> Exp b) -> Exp b
data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
infixl 0 >>=
infixl 1 <=<
type instance Eval (Pure a) = a
type instance Eval (mx >>= mf) = Eval (mf (Eval mx))
type instance Eval ((mf <=< mg) a) = Eval (mf (Eval (mg a)))

-- ## A type family for equality between types.
type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False
-- A defunctionalised label for type equality
data TyEq :: a -> b -> Exp Bool
type instance Eval (TyEq a b) = TyEqImpl a b

-- ## Fst
data Fst :: (a, b) -> Exp a
type instance Eval (Fst '(a, b)) = a
-- ## If
data If :: Bool -> a -> a -> Exp a
type instance Eval (If 'True a1 a2) = a1
type instance Eval (If 'False a1 a2) = a2
-- ## FMap
data FMap :: (a -> Exp b) -> f a -> Exp (f b)
type instance Eval (FMap f '[]) = '[]
type instance Eval (FMap f (x ': xs)) = Eval (f x) ': Eval (FMap f xs)
type instance Eval (FMap f 'Nothing)  = 'Nothing
type instance Eval (FMap f ('Just a)) = 'Just (Eval (f a))
-- ## FromMaybe
data FromMaybe :: a -> Maybe a -> Exp a
type instance Eval (FromMaybe a' ('Just a)) = a
type instance Eval (FromMaybe a 'Nothing)   = a
-- ## Addition
data (+!) :: Nat -> Nat -> Exp Nat
type instance Eval ((+!) a b) = a TL.+ b
-- ## Filter
data Filter :: (a -> Exp Bool) -> [a] -> Exp [a]
type instance Eval (Filter f '[]) = '[]
type instance Eval (Filter f (x ': xs)) =
  Eval (If (Eval (f x))
           (x ': Eval (Filter f xs))
           (Eval (Filter f xs)))
-- ## Stuck - equivalent to a type-level undefined
type family Stuck :: a
-- ## Null
data Null :: [a] -> Exp Bool
type instance Eval (Null '[]) = 'True
type instance Eval (Null (x ': xs))  = 'False

type UniqueKey (key :: k) (ts :: [(k, t)])
 = Filter (TyEq key <=< Fst) ts >>= Null

insert :: Eval (UniqueKey key ts) ~ 'True
       => Key key -> t -> OpenProduct ts -> OpenProduct ('(key, t) ': ts)
insert _ t (OpenProduct v) = OpenProduct (V.cons (Any t) v)

infixr 5 <:>
(<:>) :: Eval (UniqueKey key ts) ~ 'True => (Key key, t) -> OpenProduct ts -> OpenProduct ('(key, t) ': ts)
(<:>) (k, v) op = insert k v op

-- # Projection
data FindIndex :: (a -> Exp Bool) -> [a] -> Exp (Maybe Nat)
type instance Eval (FindIndex f '[]) = 'Nothing
type instance Eval (FindIndex f (x ': xs))
  = Eval (If (Eval (f x))
             (Eval (Pure ('Just 0)))
             (Eval (FindIndex f xs >>= FMap ((+!) 1))))
type FindElem (key :: Symbol) (ts :: [(Symbol, k)]) =
  Eval (FindIndex (TyEq key <=< Fst) ts >>= FromMaybe Stuck)

-- The SNat type allows us to take a type-level Nat and produce a Natural number from it, where Natural can be converted to an integer.
newtype SNat (n :: Nat) = SNat Natural

findElem :: forall key ts. KnownNat (FindElem key ts) => Int
findElem = fromIntegral $ natVal (Proxy @(FindElem key ts))

data LookUp :: k -> [(k, a)] -> Exp (Maybe a)

type instance Eval (LookUp k '[]) = Nothing
type instance Eval (LookUp k ( '(k', a) : ks  )) =
  Eval (If (Eval (TyEq k k'))
           (Just a)
           (Eval (LookUp k ks)))

type LookupType (key :: k) (ts :: [(k , t)]) =
  LookUp key ts >>= FromMaybe Stuck

getOP :: forall key ts  . KnownNat (FindElem key ts)
    => Key key -> OpenProduct ts -> (Eval (LookupType key ts))
getOP _ (OpenProduct v) =
  unAny (V.unsafeIndex v (findElem @key @ts))
  where
    unAny (Any a) = unsafeCoerce a

setOP :: forall key ts . KnownNat (FindElem key ts)
    => Key key -> Eval (LookupType key ts) -> OpenProduct  ts -> OpenProduct ts
setOP _ ft (OpenProduct v) =
  OpenProduct (v V.// [(findElem @key @ts, Any ft)])

instance (key ~ key') => IsLabel key (Key key') where
  fromLabel = Key

keyTest :: IsLabel "k" (Key a) => Key a
keyTest = #k

ex :: OpenProduct  '[ '("bye", Int) , '("hi", Int)]
ex = (#bye, 5) <:> (#hi, 5) <:> nil

getEx ::  Int
getEx = getOP #hi ex

class (KnownNat (FindElem k xs), a ~ Eval (LookupType k xs))
  => Lookup xs k a  where

getLens :: OpenProduct  xs -> Getting a (OpenProduct  xs) a -> a
getLens record k = record ^. k

getField :: (Lookup xs "key" a) =>
            OpenProduct  xs ->  a
getField record = getOP #key record

keyGetter :: (KnownNat (FindElem k xs), a ~ Eval (FromMaybe Stuck (Eval (LookUp k xs))))
  => Key k -> OpenProduct xs -> a
keyGetter k = (\s -> getOP k s)

makeGetter :: forall a xs k. (KnownNat (FindElem k xs), a ~ Eval (FromMaybe Stuck (Eval (LookUp k xs)))) => Key k -> Getting a (OpenProduct  xs) a
makeGetter k' = to (keyGetter k')

to' ::  (s -> a) -> Getting a s a
to' k = dimap k (contramap k)

sets' :: ((a -> b) -> s -> t) -> ASetter s t a b
sets' f g = Identity . f (runIdentity . g)

bye :: KnownNat (FindElem "bye" xs) =>
  Getting (Eval (LookupType "bye" xs)) (OpenProduct  xs) (Eval (LookupType "bye" xs))
bye = makeGetter #bye

getEx' :: Int
getEx' = ex ^. bye

lens' :: (OpenProduct s -> a) -> (OpenProduct s -> a -> OpenProduct s) -> Lens' (OpenProduct s) a
lens' sa sbt afb s = sbt s <$> afb (sa s)

-- keyLens :: (KnownNat (FindElem k xs), a ~ Eval (FromMaybe Stuck (Eval (LookUp k xs))))
--   => Key k -> OpenProduct xs -> a
keyLens :: Lookup s k a => Key k -> Lens' (OpenProduct s) a
keyLens k = lens (\s -> getOP k s) (\s b -> setOP k b s)    -- (\s -> get k s)

type family AsList (as :: [k]) = (bs :: [k]) | bs -> as where
  AsList ((f, a) : as)   = ((f , [a]) : AsList as)
  AsList '[] = '[]

class Lookup (AsList xs) k [v]  => HasVar xs k v where

instance Lookup (AsList xs) k [v] => HasVar xs k v where
