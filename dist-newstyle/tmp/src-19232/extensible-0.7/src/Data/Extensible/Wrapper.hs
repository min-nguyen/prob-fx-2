{-# LANGUAGE DeriveTraversable, StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Wrapper
-- Copyright   :  (c) Fumiaki Kinoshita 2018
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-----------------------------------------------------------------------------
module Data.Extensible.Wrapper (
  Wrapper(..)
  , _WrapperAs
  , Comp(..)
  , comp
  , Prod(..)
  ) where

import Control.Applicative
import Control.DeepSeq
import Data.Typeable (Typeable)
import Data.Proxy (Proxy(..))
import Data.Profunctor.Unsafe (Profunctor(..))
import Data.Functor.Identity (Identity(..))
import Data.Extensible.Internal.Rig
import Data.Hashable
import Data.Kind (Type)
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)
import Language.Haskell.TH.Lift
import Language.Haskell.TH (conE, appE)
import Test.QuickCheck.Arbitrary


-- | The extensible data types should take @k -> Type@ as a parameter.
-- This class allows us to take a shortcut for direct representation.
class Wrapper (h :: k -> Type) where
  -- | @'Repr' h v@ is the user-facing representation of @h v@.
  type Repr h (v :: k) :: Type

  -- | This is an isomorphism between @h v@ and @'Repr' h v@.
  --
  -- @_Wrapper :: Iso' (h v) (Repr h v)@
  --
  _Wrapper :: (Functor f, Profunctor p) => Optic' p f (h v) (Repr h v)
  _Wrapper = dimap unwrap (fmap wrap)
  {-# INLINE _Wrapper #-}

  wrap :: Repr h v -> h v
  wrap = review _Wrapper
  {-# INLINE wrap #-}

  unwrap :: h v -> Repr h v
  unwrap = view _Wrapper
  {-# INLINE unwrap #-}

  {-# MINIMAL wrap, unwrap | _Wrapper #-}

-- | Restricted version of '_Wrapper'.
-- It is useful for eliminating ambiguousness.
_WrapperAs :: (Functor f, Profunctor p, Wrapper h) => proxy v -> Optic' p f (h v) (Repr h v)
_WrapperAs _ = _Wrapper
{-# INLINE _WrapperAs #-}

instance Wrapper Identity where
  type Repr Identity a = a
  unwrap = runIdentity
  {-# INLINE unwrap #-}
  wrap = Identity
  {-# INLINE wrap #-}

instance Wrapper Maybe where
  type Repr Maybe a = Maybe a
  _Wrapper = id

instance Wrapper (Either e) where
  type Repr (Either e) a = Either e a
  _Wrapper = id

instance Wrapper [] where
  type Repr [] a = [a]
  _Wrapper = id

-- | Poly-kinded composition
newtype Comp (f :: j -> Type) (g :: i -> j) (a :: i) = Comp { getComp :: f (g a) }
  deriving (Show, Eq, Ord, Typeable, NFData, Generic, Semigroup, Monoid, Arbitrary, Hashable, Pretty)

deriving instance (Functor f, Functor g) => Functor (Comp f g)
deriving instance (Foldable f, Foldable g) => Foldable (Comp f g)
deriving instance (Traversable f, Traversable g) => Traversable (Comp f g)

instance Lift (f (g a)) => Lift (Comp f g a) where
  lift = appE (conE 'Comp) . lift . getComp

-- | Wrap a result of 'fmap'
comp :: Functor f => (a -> g b) -> f a -> Comp f g b
comp f = Comp #. fmap f
{-# INLINE comp #-}

instance (Functor f, Wrapper g) => Wrapper (Comp f g) where
  type Repr (Comp f g) x = f (Repr g x)
  _Wrapper = withIso _Wrapper $ \f g -> dimap (fmap f .# getComp) (fmap (comp g))
  {-# INLINE _Wrapper #-}

instance Wrapper (Const a) where
  type Repr (Const a) b = a
  wrap = Const
  {-# INLINE wrap #-}
  unwrap = getConst
  {-# INLINE unwrap #-}

instance Wrapper Proxy where
  type Repr Proxy x = ()
  wrap _ = Proxy
  {-# INLINE wrap #-}
  unwrap _ = ()
  {-# INLINE unwrap #-}

-- | Poly-kinded product
data Prod f g a = Prod (f a) (g a)
  deriving (Show, Eq, Ord, Typeable, Generic, Functor, Foldable, Traversable)

instance (NFData (f a), NFData (g a)) => NFData (Prod f g a)
instance (Hashable (f a), Hashable (g a)) => Hashable (Prod f g a)

instance (Wrapper f, Wrapper g) => Wrapper (Prod f g) where
  type Repr (Prod f g) a = (Repr f a, Repr g a)
  unwrap (Prod f g) = (unwrap f, unwrap g)
  {-# INLINE unwrap #-}
  wrap (f, g) = wrap f `Prod` wrap g
  {-# INLINE wrap #-}

instance (Semigroup (f a), Semigroup (g a)) => Semigroup (Prod f g a) where
  Prod a b <> Prod c d = Prod (a <> c) (b <> d)

instance (Monoid (f a), Monoid (g a)) => Monoid (Prod f g a) where
  mempty = Prod mempty mempty
  mappend = (<>)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Prod f g a) where
  arbitrary = Prod <$> arbitrary <*> arbitrary
  shrink (Prod a b) = Prod a `map` shrink b ++ flip Prod b `map` shrink a
