{-# LANGUAGE RankNTypes, GADTs, TypeApplications, FlexibleInstances, DerivingStrategies, DataKinds, TypeOperators, TypeFamilies, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds, PolyKinds, UndecidableSuperClasses, TemplateHaskell, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, OverloadedLabels, UndecidableInstances, FunctionalDependencies, TypeFamilyDependencies #-}

module Extensible.Model where

import Dist
import FreeT
import Sample
import Util
import Extensible.Freer
import Extensible.Reader
import GHC.Generics
import GHC.Types
import GHC.TypeLits
import Data.Kind
import Data.Profunctor.Unsafe
import Data.Extensible hiding (wrap, Head, Member)
import Control.Lens ( Identity, (^.), review, Getting )
import Control.Monad
import Control.Monad.Trans.Class ( MonadTrans(lift) )
-- import Control.Monad.Reader


type family Maybes (as :: [k]) = (bs :: [k]) | bs -> as where
  Maybes ((f :> v) : as) = (f :> Maybe v) : Maybes as
  Maybes (a : as) = Maybe a : Maybes as
  Maybes '[] = '[]

-- HasVar: Lookup for maybe types
class Lookup (Maybes xs) k (Maybe v) => HasVar xs k v where

instance Lookup (Maybes xs) k (Maybe v) => HasVar xs k v where

type MRec s = Record (Maybes s)

-- type ModelT s m a = Free Dist (ReaderT (Record (Maybes s)) m) a

-- type Model s a = ModelT s Identity a
type Model s rs a =
  Freer (Dist ': Reader (MRec s) ': Sampler ': rs) a

access :: forall s rs a. a ~ Maybe a => 
     Getting a (Maybes s :& Field Identity) a
  -> Model s rs a
access f = do
    env :: MRec s <- get
    return $ env ^. f

-- normal :: Double -> Double -> Maybe Double 
--        -> Model s rs Double 
-- normal mu sigma maybe_y = do
--   send (NormalDist mu sigma maybe_y return)

-- normal' :: (a ~ Maybe Double, Monad m)
--   => Double -> Double -> Getting a (s :& Field Identity) a
--   -> FreeT Dist (ReaderT (Record s) m) Double
-- normal' mu sigma field = do
--   y <- access field
--   suspend (NormalDist mu sigma y return)
