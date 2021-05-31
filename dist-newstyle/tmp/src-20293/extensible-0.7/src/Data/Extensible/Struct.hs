{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns, BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
------------------------------------------------------------------------
-- |
-- Module      :  Data.Extensible.Struct
-- Copyright   :  (c) Fumiaki Kinoshita 2018
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
--
-- Mutable structs
------------------------------------------------------------------------
module Data.Extensible.Struct (
  -- * Mutable struct
  Struct
  , set
  , get
  , new
  , newRepeat
  , newFor
  , newFromHList
  , WrappedPointer(..)
  , (-$>)
  -- ** Atomic operations
  , atomicModify
  , atomicModify'
  , atomicModify_
  , atomicModify'_
  -- * Immutable product
  , (:&)
  , (:*)
  , unsafeFreeze
  , newFrom
  , hlookup
  , hlength
  , type (++)
  , happend
  , hfoldrWithIndex
  , thaw
  , hfrozen
  , hmodify
  , toHList) where

import GHC.Prim
import Control.Comonad
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Constraint
import Data.Extensible.Class
import Data.Extensible.Internal.Rig
import Data.Extensible.Wrapper
import Data.Profunctor.Rep
import Data.Profunctor.Sieve
import Data.Proxy
import Data.Kind (Type)
import qualified Data.StateVar as V
import GHC.Types
import qualified Type.Membership.HList as L

-- | Mutable type-indexed struct.
data Struct s (h :: k -> Type) (xs :: [k]) = Struct (SmallMutableArray# s Any)

-- | Write a value in a 'Struct'.
set :: PrimMonad m => Struct (PrimState m) h xs -> Membership xs x -> h x -> m ()
set (Struct m) (getMemberId -> I# i) e = primitive
  $ \s -> case unsafeCoerce# writeSmallArray# m i e s of
    s' -> (# s', () #)
{-# INLINE set #-}

-- | Read a value from a 'Struct'.
get :: PrimMonad m => Struct (PrimState m) h xs -> Membership xs x -> m (h x)
get (Struct m) (getMemberId -> I# i) = primitive $ unsafeCoerce# readSmallArray# m i
{-# INLINE get #-}

-- | Atomically modify an element in a 'Struct'.
atomicModify :: PrimMonad m
  => Struct (PrimState m) h xs -> Membership xs x -> (h x -> (h x, a)) -> m a
atomicModify (Struct m) (getMemberId -> I# i) f = primitive
  $ \s0 -> case readSmallArray# m i s0 of
    (# s, x #) -> retry x s
  where
    retry x s = let p = unsafeCoerce# f x in
      case casSmallArray# m i x (fst p) s of
        (# s', b, y #) -> case b of
          0# -> (# s', snd p #)
          _ -> retry y s'
{-# INLINE atomicModify #-}

-- | Strict version of 'atomicModify'.
atomicModify' :: PrimMonad m
  => Struct (PrimState m) h xs -> Membership xs x -> (h x -> (h x, a)) -> m a
atomicModify' s i f = atomicModify s i
  (\x -> let (y, a) = f x in (y, y `seq` a))
  >>= (return $!)
{-# INLINE atomicModify' #-}

-- | Apply a function to an element atomically.
atomicModify_ :: PrimMonad m
  => Struct (PrimState m) h xs -> Membership xs x -> (h x -> h x) -> m (h x)
atomicModify_ (Struct m) (getMemberId -> I# i) f = primitive
  $ \s0 -> case readSmallArray# m i s0 of
    (# s, x #) -> retry x s
  where
    retry x s = case casSmallArray# m i x (unsafeCoerce# f x) s of
      (# s', b, y #) -> case b of
        0# -> (# s', unsafeCoerce# y #)
        _ -> retry y s'
{-# INLINE atomicModify_ #-}

-- | Strict version of 'atomicModify_'.
atomicModify'_ :: PrimMonad m
  => Struct (PrimState m) h xs -> Membership xs x -> (h x -> h x) -> m (h x)
atomicModify'_ s i f = atomicModify_ s i f >>= (return $!)
{-# INLINE atomicModify'_ #-}

-- | A pointer to an element in a 'Struct'.
data WrappedPointer s h a where
  WrappedPointer :: !(Struct s h xs)
    -> !(Membership xs x)
    -> WrappedPointer s h (Repr h x)

instance (s ~ RealWorld, Wrapper h) => V.HasGetter (WrappedPointer s h a) a where
  get (WrappedPointer s i) = liftIO $ view _Wrapper <$> get s i

instance (s ~ RealWorld, Wrapper h) => V.HasSetter (WrappedPointer s h a) a where
  WrappedPointer s i $= v = liftIO $ set s i $ review _Wrapper v

instance (s ~ RealWorld, Wrapper h) => V.HasUpdate (WrappedPointer s h a) a a where
  WrappedPointer s i $~ f = liftIO $ void $ atomicModify_ s i $ over _Wrapper f
  WrappedPointer s i $~! f = liftIO $ void $ atomicModify'_ s i $ over _Wrapper f

-- | Get a 'WrappedPointer' from a name.
(-$>) :: forall k h xs v s. (Lookup xs k v) => Struct s h xs -> Proxy k -> WrappedPointer s h (Repr h (k ':> v))
s -$> _ = WrappedPointer s (association :: Membership xs (k ':> v))
{-# INLINE (-$>) #-}

-- | Create a new 'Struct' using the supplied initializer.
new :: forall h m xs. (PrimMonad m, Generate xs)
  => (forall x. Membership xs x -> h x)
  -> m (Struct (PrimState m) h xs)
new = newDict Dict
{-# INLINE new #-}

newDict :: PrimMonad m
  => Dict (Generate xs)
  -> (forall x. Membership xs x -> h x)
  -> m (Struct (PrimState m) h xs)
newDict Dict k = do
  m <- newRepeat undefined
  henumerate (\i cont -> set m i (k i) >> cont) $ return m
{-# NOINLINE[0] newDict #-}

-- | Create a 'Struct' full of the specified value.
newRepeat :: forall h m xs. (PrimMonad m, Generate xs)
  => (forall x. h x)
  -> m (Struct (PrimState m) h xs)
newRepeat x = do
  let !(I# n) = hcount (Proxy :: Proxy xs)
  primitive $ \s -> case newSmallArray# n (unsafeCoerce# x) s of
    (# s', a #) -> (# s', Struct a #)
{-# INLINE newRepeat #-}

-- | Create a new 'Struct' using the supplied initializer with a context.
newFor :: forall proxy c h m xs. (PrimMonad m, Forall c xs)
  => proxy c
  -> (forall x. c x => Membership xs x -> h x)
  -> m (Struct (PrimState m) h xs)
newFor = newForDict Dict
{-# INLINE newFor #-}

newForDict :: forall proxy c h m xs. (PrimMonad m)
  => Dict (Forall c xs)
  -> proxy c
  -> (forall x. c x => Membership xs x -> h x)
  -> m (Struct (PrimState m) h xs)
newForDict Dict p k = do
  m <- newRepeat undefined
  henumerateFor p (Proxy :: Proxy xs) (\i cont -> set m i (k i) >> cont) $ return m
{-# NOINLINE[0] newForDict #-}

-- | Create a new 'Struct' from an 'HList'.
newFromHList :: forall h m xs. PrimMonad m => L.HList h xs -> m (Struct (PrimState m) h xs)
newFromHList l = do
  let !(I# size) = L.hlength l
  m <- primitive $ \s -> case newSmallArray# size undefined s of
    (# s', a #) -> (# s', Struct a #)

  let go :: Int -> L.HList h t -> m ()
      go _ L.HNil = return ()
      go i (L.HCons x xs) = set m (unsafeMembership i) x >> go (i + 1) xs

  go 0 l
  return m
{-# NOINLINE newFromHList #-}

-- | The type of extensible products.
--
-- @(:&) :: [k] -> (k -> *) -> *@
--
data (s :: [k]) :& (h :: k -> *) = HProduct (SmallArray# Any)

type h :* xs = xs :& h
{-# DEPRECATED (:*) "Use :& instead" #-}

-- | Turn 'Struct' into an immutable product. The original 'Struct' may not be used.
unsafeFreeze :: PrimMonad m => Struct (PrimState m) h xs -> m (xs :& h)
unsafeFreeze (Struct m) = primitive $ \s -> case unsafeFreezeSmallArray# m s of
  (# s', a #) -> (# s', HProduct a #)
{-# INLINE unsafeFreeze #-}

-- | Create a new 'Struct' from a product.
thaw :: PrimMonad m => xs :& h -> m (Struct (PrimState m) h xs)
thaw (HProduct ar) = primitive $ \s -> case thawSmallArray# ar 0# (sizeofSmallArray# ar) s of
  (# s', m #) -> (# s', Struct m #)

-- | The size of a product.
hlength :: xs :& h -> Int
hlength (HProduct ar) = I# (sizeofSmallArray# ar)
{-# INLINE hlength #-}

-- | Concatenate type level lists
type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': xs ++ ys

infixr 5 ++

-- | Combine products.
happend :: xs :& h -> ys :& h -> (xs ++ ys) :& h
happend (HProduct lhs) (HProduct rhs) = runST $ primitive $ \s0 ->
  let lhsSz = sizeofSmallArray# lhs
      rhsSz = sizeofSmallArray# rhs
  in  case newSmallArray# (lhsSz +# rhsSz) undefined s0 of { (# s1, a #) ->
      case copySmallArray# lhs 0# a 0# lhsSz s1 of { s2 ->
      case copySmallArray# rhs 0# a lhsSz rhsSz s2 of { s3 ->
      case unsafeFreezeSmallArray# a s3 of { (# s4, frz #) ->
      (# s4, HProduct frz #) }}}}

infixr 5 `happend`

unsafeMembership :: Int -> Membership xs x
unsafeMembership = unsafeCoerce#

-- | Right-associative fold of a product.
hfoldrWithIndex :: (forall x. Membership xs x -> h x -> r -> r) -> r -> xs :& h -> r
hfoldrWithIndex f r p = foldr
  (\i -> let m = unsafeMembership i in f m (hlookup m p)) r [0..hlength p - 1]
{-# INLINE hfoldrWithIndex #-}

-- | Convert a product into an 'HList'.
toHList :: forall h xs. xs :& h -> L.HList h xs
toHList p = go 0 where
  go :: Int -> L.HList h xs
  go i
    | i == hlength p = unknownHList L.HNil
    | otherwise = unknownHList $ L.HCons (hlookup (unsafeMembership i) p) (go (i + 1))

  unknownHList :: L.HList h ys -> L.HList h zs
  unknownHList = unsafeCoerce#
{-# NOINLINE toHList #-}

{-# RULES "toHList/fromHList" forall x. toHList (hfrozen (newFromHList x)) = x #-}

-- | Create a new 'Struct' using the contents of a product.
newFrom :: forall g h m xs. (PrimMonad m)
  => xs :& g
  -> (forall x. Membership xs x -> g x -> h x)
  -> m (Struct (PrimState m) h xs)
newFrom hp@(HProduct ar) k = do
  let !n = sizeofSmallArray# ar
  st <- primitive $ \s -> case newSmallArray# n undefined s of
    (# s', a #) -> (# s', Struct a #)
  let go i
        | i == I# n = return st
        | otherwise = do
          let !m = unsafeMembership i
          set st m $! k m (hlookup m hp)
          go (i + 1)
  go 0
{-# NOINLINE newFrom #-}

{-# RULES "newFrom/newFrom" forall p (f :: forall x. Membership xs x -> f x -> g x)
 (g :: forall x. Membership xs x -> g x -> h x)
  . newFrom (hfrozen (newFrom p f)) g = newFrom p (\i x -> g i (f i x)) #-}

{-# RULES "newFrom/newDict" forall d (f :: forall x. Membership xs x -> g x)
 (g :: forall x. Membership xs x -> g x -> h x)
  . newFrom (hfrozen (newDict d f)) g = newDict d (\i -> g i (f i)) #-}

{-# RULES "newFrom/newForDict" forall d p (f :: forall x. Membership xs x -> g x)
 (g :: forall x. Membership xs x -> g x -> h x)
  . newFrom (hfrozen (newForDict d p f)) g = newForDict d p (\i -> g i (f i)) #-}

-- | Get an element in a product.
hlookup :: Membership xs x -> xs :& h -> h x
hlookup (getMemberId -> I# i) (HProduct ar) = case indexSmallArray# ar i of
  (# a #) -> unsafeCoerce# a
{-# INLINE hlookup #-}

-- | Create a product from an 'ST' action which returns a 'Struct'.
hfrozen :: (forall s. ST s (Struct s h xs)) -> xs :& h
hfrozen m = runST $ m >>= unsafeFreeze
{-# INLINE[0] hfrozen #-}

-- | Turn a product into a 'Struct' temporarily.
hmodify :: (forall s. Struct s h xs -> ST s ()) -> xs :& h -> xs :& h
hmodify f m = runST $ do
  s <- thaw m
  f s
  unsafeFreeze s
{-# INLINE[0] hmodify #-}

{-# RULES "hmodify/batch" forall
  (a :: forall s. Struct s h xs -> ST s ())
  (b :: forall s. Struct s h xs -> ST s ())
  (x :: xs :& h). hmodify b (hmodify a x) = hmodify (\s -> a s >> b s) x  #-}

instance (Corepresentable p, Comonad (Corep p), Functor f) => Extensible f p (:&) where
  -- | A lens for a value in a known position.
  pieceAt i pafb = cotabulate $ \ws -> sbt (extract ws) <$> cosieve pafb (hlookup i <$> ws) where
    sbt xs !x = hmodify (\s -> set s i x) xs
  {-# INLINE pieceAt #-}
