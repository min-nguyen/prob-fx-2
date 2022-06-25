{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module FusedEffects.Sum where
  
import Data.Kind (Constraint, Type)

-- | Higher-order sums are used to combine multiple effects into a signature, typically by chaining on the right.
data (f :+: g) (m :: Type -> Type) k
  = L (f m k)
  | R (g m k)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixr 4 :+:

class Member (sub :: (Type -> Type) -> (Type -> Type)) sup where
  -- | Inject a member of a signature into the signature.
  inj :: sub m a -> sup m a

-- | Reflexivity: @t@ is a member of itself.
instance Member t t where
  inj = id
  {-# INLINE inj #-}

-- | Left-recursion: if @t@ is a member of @l1 ':+:' l2 ':+:' r@, then we can inject it into @(l1 ':+:' l2) ':+:' r@ by injection into a right-recursive signature, followed by left-association.
instance {-# OVERLAPPABLE #-}
         Member t (l1 :+: l2 :+: r)
      => Member t ((l1 :+: l2) :+: r) where
  inj = reassociateSumL . inj
  {-# INLINE inj #-}

-- | Left-occurrence: if @t@ is at the head of a signature, we can inject it in O(1).
instance {-# OVERLAPPABLE #-}
         Member l (l :+: r) where
  inj = L
  {-# INLINE inj #-}

-- | Right-recursion: if @t@ is a member of @r@, we can inject it into @r@ in O(n), followed by lifting that into @l ':+:' r@ in O(1).
instance {-# OVERLAPPABLE #-}
         Member l r
      => Member l (l' :+: r) where
  inj = R . inj
  {-# INLINE inj #-}

-- | Reassociate a right-nested sum leftwards.
--
-- @since 1.0.2.0
reassociateSumL :: (l1 :+: l2 :+: r) m a -> ((l1 :+: l2) :+: r) m a
reassociateSumL = \case
  L l     -> L (L l)
  R (L l) -> L (R l)
  R (R r) -> R r

type family Members sub sup :: Constraint where
  Members (l :+: r) u = (Members l u, Members r u)
  Members t         u = Member t u
