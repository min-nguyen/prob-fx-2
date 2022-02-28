{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, FlexibleContexts, FlexibleInstances, GADTs, PolyKinds, RankNTypes, DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances,  OverloadedLabels #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Extensible.ModelEnv where

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
import Extensible.Member

data ObsVar (x :: Symbol) where
  ObsVar :: KnownSymbol x => ObsVar x

instance (KnownSymbol x, x ~ x') => IsLabel x (ObsVar x') where
  fromLabel = ObsVar

varToStr :: forall x. ObsVar x -> String
varToStr ObsVar = symbolVal (Proxy @x)

data Assign x v = x := v

data ModelEnv (ts :: [Assign Symbol *]) where
  HNil  :: ModelEnv '[]
  HCons :: forall a k ts. [a] -> ModelEnv ts -> ModelEnv (k := a : ts)

instance (KnownSymbol k, Show v, Show (ModelEnv ts)) => Show (ModelEnv ((k := v) ': ts)) where
  show (HCons v ts) = varToStr (ObsVar @k) ++ ":=" ++ show v ++ ", " ++ show ts

instance Show (ModelEnv '[]) where
  show HNil = "[]"

instance FindElem x ((x := v) : ts) where
  findElem = P 0

instance {-# OVERLAPPABLE #-} FindElem x ts => FindElem x ((x' := v) : ts) where
  findElem =  P $ 1 + unP (findElem :: P x ts)

type family LookupType x ts where
  LookupType x ((x := v) : ts) = v
  LookupType x ((x' := v) : ts) = LookupType x ts

class (FindElem x ts, LookupType x ts ~ a) => Observable ts x a where
  getIdx :: ObsVar x -> ModelEnv ts -> Int
  getIdx _ (HCons  v ts) = unP $ findElem @x @ts
  getOP  :: ObsVar x -> ModelEnv ts -> [a]
  setOP  :: ObsVar x -> [a] -> ModelEnv ts -> ModelEnv ts

instance (FindElem x env, LookupType x env ~ a) => Observable env x a where
  getIdx _ _ = unP $ findElem @x @env
  getOP _ ts =
    let idx = unP $ findElem @x @env
        f :: Int -> ModelEnv ts -> [a]
        f n (HCons v ts) = if   n == 0
                           then unsafeCoerce v
                           else f (n - 1) ts
    in  f idx ts
  setOP _ v' ts =
    let idx = unP $ findElem @x @env
        f :: Int -> ModelEnv ts -> ModelEnv ts
        f n (HCons v ts) = if   n == 0
                           then HCons (unsafeCoerce v') ts
                           else HCons v (f (n - 1) ts)
    in  f idx ts

type family Observables env (ks :: [Symbol]) v :: Constraint where
  Observables env (x ': xs) v = (Observable env x v, Observables env xs v)
  Observables env '[] v = ()

-- | Determine whether a var 'x' exists in a list of assocs 'kvs'
type family UniqueKey x env where
  UniqueKey x ((x ':= v) : env) = False
  UniqueKey x ((x' ':= v) : env) = UniqueKey x env
  UniqueKey x '[] = True

nil :: ModelEnv '[]
nil = HNil

infixr 5 <:>
(<:>) :: UniqueKey x env ~ 'True => Assign (ObsVar x) [v] -> ModelEnv env -> ModelEnv ((x ':= v) ': env)
(_ := v) <:> ts = HCons v ts

mkLens :: forall env x a. Observable env x a => ObsVar x -> Lens' (ModelEnv env) [a]
mkLens x = lens (getOP x) (\s a -> setOP x a s)

mkGetter ::  (s -> a) -> Getting a s a
mkGetter x = dimap x (contramap x)

mkSetter :: ((a -> b) -> s -> t) -> ASetter s t a b
mkSetter f g = Identity . f (runIdentity . g)

mkGetterSetter :: forall env x v . (Observable env x v)
  => ObsVar x -> (Getting [v] (ModelEnv env) [v], ASetter (ModelEnv env) (ModelEnv env) [v] [v])
mkGetterSetter field =
  let getter' = mkGetter (\s -> getOP field s)
      setter' = mkSetter (\f s ->  let a = s ^. getter'
                                       a' = f a
                                   in  setOP field a' s)
  in (getter', setter')

-- f = #hi := 5 <: #bye := 5

-- data Any where
--   Any :: t -> Any

-- data ModelEnv (ts :: [(Symbol, k)])  where
--   ModelEnv :: V.Vector Any -> ModelEnv ts

-- nil :: ModelEnv '[]
-- nil = ModelEnv V.empty

-- data ObsVar (key :: Symbol) = ObsVar

-- newtype P t rs = P {unP :: Int}

-- data Assoc k v = k := v

-- class FindElem k r where
--   findElem :: P k r

-- instance {-# INCOHERENT  #-} FindElem k ((k ':= v) ': kvs) where
--   findElem = P 0
-- instance {-# OVERLAPPABLE #-} FindElem k kvs => FindElem k (kv ': kvs) where
--   findElem = P $ 1 + unP (findElem :: P k kvs)

-- getOP :: forall key ts a. FindElem key ts => ObsVar key -> ModelEnv ts -> a
-- getOP _ (ModelEnv v) =
--   unAny (V.unsafeIndex v (unP $ findElem @key @ts))
--   where
--     unAny (Any a) = unsafeCoerce a

-- type family LookupType k kvs where
--   LookupType k ((k ':= v) : kvs) = v


-- class (KnownNat (FindElem k xs), a ~ Eval (LookupType k xs))
--   => Lookup xs k a  where

-- type UniqueKey (key :: k) (ts :: [(k, t)])
--  = Filter (TyEq key <=< Fst) ts >>= Null

-- insert :: Eval (UniqueKey key ts) ~ 'True
--        => ObsVar key -> t -> ModelEnv ts -> ModelEnv ('(key, t) ': ts)
-- insert _ t (ModelEnv v) = ModelEnv (V.cons (Any t) v)

-- infixr 5 <:>
-- (<:>) :: Eval (UniqueKey key ts) ~ 'True => (ObsVar key, t) -> ModelEnv ts -> ModelEnv ('(key, t) ': ts)
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
--     => ObsVar key -> ModelEnv ts -> (Eval (LookupType key ts))
-- getOP _ (ModelEnv v) =
--   unAny (V.unsafeIndex v (findElem @key @ts))
--   where
--     unAny (Any a) = unsafeCoerce a

-- setOP :: forall key ts . KnownNat (FindElem key ts)
--     => ObsVar key -> Eval (LookupType key ts) -> ModelEnv  ts -> ModelEnv ts
-- setOP _ ft (ModelEnv v) =
--   ModelEnv (v V.// [(findElem @key @ts, Any ft)])

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
