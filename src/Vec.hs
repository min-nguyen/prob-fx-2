{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeOperators #-}

module Vec (module Vec, module Data.Vec.Lazy) where

import           Data.Type.Nat
import           Data.Typeable
import           Data.Vec.Lazy

class (SNatI n, Typeable n) => TyNat n

instance (SNatI n, Typeable n) => TyNat n

replicate :: SNat n -> a -> Vec n a
replicate SZ x = VNil
replicate SS x = x ::: Vec.replicate snat x

iterate :: SNat n -> (t -> t) -> t -> Vec n t
iterate SZ f a = VNil
iterate SS f a = a ::: Vec.iterate snat f (f a)
