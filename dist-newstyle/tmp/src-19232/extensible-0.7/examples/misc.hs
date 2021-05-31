{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeApplications #-}

import Data.Extensible
import Data.Functor.Identity
import Data.Proxy
import GHC.TypeLits
import Data.Extensible.Internal.Rig
import Data.Typeable
import Data.Functor.Product

-- | Collect keys
keys :: forall proxy xs. Forall (KeyIs KnownSymbol) xs => proxy xs -> [String]
keys _ = henumerateFor (Proxy @ (KeyIs KnownSymbol)) (Proxy @ xs)
  ((:) . symbolVal . proxyAssocKey) []

values :: Forall (ValueIs Show) xs => Record xs -> [String]
values = hfoldrWithIndexFor (Proxy @ (ValueIs Show))
  (const $ (:) . show . view _Wrapper) []

values' :: Forall (ValueIs (Product Typeable Show)) xs => Record xs -> [String]
values' = hfoldrWithIndexFor (Proxy @ (ValueIs Show))
  (const $ (:) . show . view _Wrapper) []
