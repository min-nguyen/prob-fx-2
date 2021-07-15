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

data Key (key :: Symbol) = Key

instance (key ~ key') => IsLabel key (Key key') where
  fromLabel = Key

newtype P t rs = P {unP :: Int}

data Assoc k v = k :> v

(@=) :: k -> v -> Assoc k v
(@=) = (:>)

-- | Find index of key 'k' in list of assocs 'kvs'
class FindElem k kvs where
  findElem :: P k kvs
instance {-# INCOHERENT  #-} FindElem k ((k ':> v) ': kvs) where
  findElem = P 0
instance {-# OVERLAPPABLE #-} FindElem k kvs => FindElem k (kv ': kvs) where
  findElem = P $ 1 + unP (findElem :: P k kvs)

-- | Look up type associated with key 'k' in list of assocs 'kvs'
type family LookupType k kvs where
  LookupType k ((k ':> v) : kvs) = v
  LookupType k ((k' ':> v) : kvs) = LookupType k kvs

-- | Determine whether a key 'k' exists in a list of assocs 'kvs'
type family UniqueKey k kvs where
  UniqueKey k ((k ':> v) : kvs) = False
  UniqueKey k ((k' ':> v) : kvs) = UniqueKey k kvs
  UniqueKey k '[] = True

-- | State that we can lookup key 'k' with value type 'a' in list of assocs 'xs'
class (FindElem k xs, LookupType k xs ~ a) => Lookup xs k a

instance (FindElem k xs, LookupType k xs ~ a) => Lookup xs k a

-- | Map the list constructor over a list of assocs
type family AsList (as :: [k]) = (bs :: [k]) | bs -> as where
  AsList ((f :> a) : as)   = ((f :> [a]) : AsList as)
  AsList '[] = '[]

-- | "HasVar xs k v" is shorthand for "Lookup (AsList xs) k [v]""
class Lookup (AsList xs) k [v]  => HasVar xs k v where

instance Lookup (AsList xs) k [v] => HasVar xs k v where

type LRec s = OpenProduct (AsList s)

insert :: UniqueKey key ts ~ 'True
       => Key key -> t -> OpenProduct ts -> OpenProduct (key ':> t ': ts)
insert _ t (OpenProduct v) = OpenProduct (V.cons (Any t) v)

infixr 5 <:
(<:) :: UniqueKey key ts ~ 'True => Assoc (Key key) t -> OpenProduct ts -> OpenProduct ((key ':> t) ': ts)
(<:) (k :> v) op = insert k v op

getOP :: forall key ts a. FindElem key ts => Key key -> OpenProduct ts -> a
getOP _ (OpenProduct v) =
  unAny (V.unsafeIndex v (unP $ findElem @key @ts))
  where
    unAny (Any a) = unsafeCoerce a

setOP :: forall key ts . FindElem key ts
    => Key key -> LookupType key ts -> OpenProduct ts -> OpenProduct ts
setOP _ ft (OpenProduct v) =
  OpenProduct (v V.// [(unP (findElem @key @ts), Any ft)])

mkGetter ::  (s -> a) -> Getting a s a
mkGetter k = dimap k (contramap k)

mkSetter :: ((a -> b) -> s -> t) -> ASetter s t a b
mkSetter f g = Identity . f (runIdentity . g)

mkGetterSetter :: (Lookup xs k a)
  => Key k -> (Getting a (OpenProduct xs) a, ASetter (OpenProduct xs) (OpenProduct xs) a a)
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

-- data Key (key :: Symbol) = Key

-- newtype P t rs = P {unP :: Int}

-- data Assoc k v = k :> v

-- class FindElem k r where
--   findElem :: P k r

-- instance {-# INCOHERENT  #-} FindElem k ((k ':> v) ': kvs) where
--   findElem = P 0
-- instance {-# OVERLAPPABLE #-} FindElem k kvs => FindElem k (kv ': kvs) where
--   findElem = P $ 1 + unP (findElem :: P k kvs)

-- getOP :: forall key ts a. FindElem key ts => Key key -> OpenProduct ts -> a
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
--        => Key key -> t -> OpenProduct ts -> OpenProduct ('(key, t) ': ts)
-- insert _ t (OpenProduct v) = OpenProduct (V.cons (Any t) v)

-- infixr 5 <:>
-- (<:>) :: Eval (UniqueKey key ts) ~ 'True => (Key key, t) -> OpenProduct ts -> OpenProduct ('(key, t) ': ts)
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
--     => Key key -> OpenProduct ts -> (Eval (LookupType key ts))
-- getOP _ (OpenProduct v) =
--   unAny (V.unsafeIndex v (findElem @key @ts))
--   where
--     unAny (Any a) = unsafeCoerce a

-- setOP :: forall key ts . KnownNat (FindElem key ts)
--     => Key key -> Eval (LookupType key ts) -> OpenProduct  ts -> OpenProduct ts
-- setOP _ ft (OpenProduct v) =
--   OpenProduct (v V.// [(findElem @key @ts, Any ft)])

-- instance (key ~ key') => IsLabel key (Key key') where
--   fromLabel = Key

-- class (KnownNat (FindElem k xs), a ~ Eval (LookupType k xs))
--   => Lookup xs k a  where

-- getLens :: OpenProduct xs -> Getting a (OpenProduct  xs) a -> a
-- getLens record k = record ^. k

-- keyGetter :: (KnownNat (FindElem k xs), a ~ Eval (FromMaybe Stuck (Eval (LookUp k xs))))
--   => Key k -> OpenProduct xs -> a
-- keyGetter k = (\s -> getOP k s)

-- makeGetter :: forall a xs k. (KnownNat (FindElem k xs), a ~ Eval (FromMaybe Stuck (Eval (LookUp k xs)))) => Key k -> Getting a (OpenProduct  xs) a
-- makeGetter k' = to (keyGetter k')

-- to' ::  (s -> a) -> Getting a s a
-- to' k = dimap k (contramap k)

-- sets' :: ((a -> b) -> s -> t) -> ASetter s t a b
-- sets' f g = Identity . f (runIdentity . g)

-- lens' :: (OpenProduct s -> a) -> (OpenProduct s -> a -> OpenProduct s) -> Lens' (OpenProduct s) a
-- lens' sa sbt afb s = sbt s <$> afb (sa s)

-- keyLens :: Lookup s k a => Key k -> Lens' (OpenProduct s) a
-- keyLens k = lens (\s -> getOP k s) (\s b -> setOP k b s)

-- mkGetterSetter :: (KnownNat (FindElem k xs), [a] ~ Eval (FromMaybe Stuck (Eval (LookUp k xs)))) => Key k -> (Getting [a] (OpenProduct  xs) [a], ASetter (OpenProduct  xs) (OpenProduct xs) [a] [a])
-- mkGetterSetter field =
--   let getter' = to' (\s -> getOP field s)
--       setter' = sets' (\f s ->  let a = s ^. getter'
--                                     a' = f a
--                                 in  setOP field a' s)
--   in (getter', setter')


-- keyTest :: IsLabel "k" (Key a) => Key a
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
